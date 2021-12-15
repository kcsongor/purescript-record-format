module Record.Format where

import Prelude (identity, (<>), class Show, show)
import Prim.Row as Row
import Prim.Symbol as Symbol
import Record as Record
import Type.Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)

--------------------------------------------------------------------------------
-- * Format strings

data Fmt -- ^ a format token is...
foreign import data Var :: Symbol -> Fmt -- ^ either a variable (to be replaced)
foreign import data Lit :: Symbol -> Fmt -- ^ or a literal

-- | A list of format tokens
data FList
foreign import data FNil :: FList
foreign import data FCons :: Fmt -> FList -> FList

data FProxy (fl :: FList) = FProxy

-- | Format a row with a (type-level) format string. If @row@ doesn't contain
--   all the necessary fields, constraint resolution fails
class Format (string :: Symbol) (row :: Row Type) where
  format :: SProxy string -> Record row -> String

-- parse the format string and delegate the formatting to @FormatParsed@
instance formatParsedFormat ::
  ( Parse string parsed
  , FormatParsed parsed row
  ) => Format string row where
  format _ = formatParsed (FProxy :: FProxy parsed)

-- | Format a row with a list of format tokens. If @row@ doesn't contain
--   all the necessary fields, constraint resolution fails
class FormatParsed (strings :: FList) (row :: Row Type) where
  formatParsed :: FProxy strings -> Record row -> String

instance formatFNil :: FormatParsed FNil row where
  formatParsed _ _ = ""

instance formatVar ::
  ( IsSymbol key
  , Row.Cons key typ tail row
  , FormatParsed ks row
  , FormatVar typ
  ) => FormatParsed (FCons (Var key) ks) row where
  formatParsed _ row
    = var <> rest
    where var  = fmtVar (Record.get (SProxy :: SProxy key) row)
          rest = formatParsed (FProxy :: FProxy ks) row

instance formatLit ::
  ( IsSymbol l
  , FormatParsed ks row
  ) => FormatParsed (FCons (Lit l) ks) row where
  formatParsed _ row
    = lit <> rest
    where lit  = reflectSymbol (SProxy :: SProxy l)
          rest = formatParsed (FProxy :: FProxy ks) row

-- | Formatting variables - we don't want to show the quotes around strings, so
--   we treat them specially
class FormatVar a where
  fmtVar :: a -> String

instance aFmtVar :: FormatVar String where
  fmtVar = identity
else instance bFmtVar :: Show a => FormatVar a where
  fmtVar = show

--------------------------------------------------------------------------------
-- Parsing

class Parse (i :: Symbol) (o :: FList) | i -> o

instance aParse :: Parse "" FNil
else instance bParse :: (Symbol.Cons h t i, ParseLit h t o) => Parse i o

-- | Parse literals. @h@ is the current character, @t@ is the remaining string
class ParseLit (h :: Symbol) (t :: Symbol) (o :: FList) | h t -> o

instance aParseLitNil :: ParseLit o "" (FCons (Lit o) FNil)
-- when we find a '{' character, call @ParseVar@
else instance bParseLitVar ::
  ( Symbol.Cons h' t' t
  , ParseVar h' t' (Var match) rest
  , Parse rest pRest
  ) => ParseLit "{" t (FCons (Lit "") (FCons (Var match) pRest))
else instance cParseLit ::
  ( Parse i (FCons (Lit l) fs)
  , Symbol.Cons c l cl
  ) => ParseLit c i (FCons (Lit cl) fs)

-- | Parse variables. Returns the symbol between {}s and the remaining string
--   after the closing '}'
class ParseVar (h :: Symbol) (t :: Symbol) (var :: Fmt) (rest :: Symbol) | h t -> var rest

instance aParseVar :: ParseVar "" a (Var "") ""
else instance bParseVar :: ParseVar "}" i (Var "") i
else instance cParseVar :: ParseVar curr "" (Var curr) ""
else instance dParseVar ::
  ( Symbol.Cons h' t' t
  , ParseVar h' t' (Var var) rest
  , Symbol.Cons h var var'
  ) => ParseVar h t (Var var') rest

parse :: forall i o. Parse i o => SProxy i -> FProxy o
parse _ = FProxy :: FProxy o
