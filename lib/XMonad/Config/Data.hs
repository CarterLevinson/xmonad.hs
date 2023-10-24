module XMonad.Config.Data
    ( KeyPair
    , KeyMap
    , Cell
    , Grid
    , Command
    , HexColorCode
    , Path
    , Font
    , Weight (..)
    , Slant (..)
    , Spacing (..)
    , XFTFont (..)
    , ScreenshotAction (..)
    ) where

import           Graphics.X11       (KeyMask, KeySym)
import           XMonad.Core        (X)

-- type aliases
type KeyPair      = (KeyMask, KeySym)
type KeyMap       = (KeyPair, X())

type Path         = String
type Font         = String

type Command      = String
type HexColorCode = String

type Cell         = (String, String)
type Grid         = [Cell]

-- custom data types
--
data ScreenshotAction
    = FocusedWindow
    | SelectedWindow
    | FullScreen
    deriving (Read, Show, Enum, Eq)

data Weight
    = Light
    | Medium
    | DemiBold
    | Bold
    | Black
    deriving (Read, Show, Enum, Eq)

data Slant
    = Roman
    | Italic
    | Oblique
    deriving (Read, Show, Enum, Eq)

data Spacing
    = Proportional
    | Mono
    | CharCell
    deriving (Read, Show, Enum, Eq)

data XFTFont = XFTFont
    { family    :: String
    , size      :: Double
    , slant     :: Slant
    , weight    :: Weight
    , spacing   :: Spacing
    , antialias :: Bool
    , hinting   :: Bool
    }
    deriving (Read, Show, Eq)
