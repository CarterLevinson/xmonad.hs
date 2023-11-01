module Config.Data
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
    , XftFont (..)
    , ScreenshotAction (..)
    ) where

import qualified Data.Text as T
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
    = Fullscreen
    | Selected
    | Focused
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

data XftFont = XFT
    { family    :: String
    , size      :: Double
    , slant     :: Slant
    , weight    :: Weight
    , spacing   :: Spacing
    , hinting   :: Bool
    , antialias :: Bool
    , pixelsize :: Maybe Double
    }
    deriving (Read,Eq)

showFont :: XftFont -> Font
showFont xft =
    let toLower           =  T.unpack . T.toLower . T.pack
        showLower f       =  toLower  . show      . f
        ps                =  pixelsize xft
     in family xft ++ "-" ++ showLower size         xft
        ++ ":slant="      ++ showLower slant        xft
        ++ ":weight="     ++ showLower weight       xft
        ++ ":spacing="    ++ showLower spacing      xft
        ++ ":hinting="    ++ showLower hinting      xft
        ++ ":antialias="  ++ showLower antialias    xft
        ++ case ps of
             Just p       ->
                 ":pixelsize=" ++ show (Just p)
             Nothing      ->
                 ""

instance Show XftFont where
    show = showFont :: XftFont -> String
