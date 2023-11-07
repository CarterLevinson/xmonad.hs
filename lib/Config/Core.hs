module Config.Core where

-- import Graphics.X11 (KeyMask, KeySym)
import Data.Bits ((.|.))
import Graphics.X11
import Graphics.X11.ExtraTypes.XF86
import XMonad.Core (X)

import qualified Data.Text as T

-- type aliases
type KeyPair = (KeyMask, KeySym)
type KeyMap = (KeyPair, X ())

type Path = String
type Font = String

type Command = String
type HexColorCode = String

type Cell = (String, String)
type Grid = [Cell]

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
    { family :: String
    , size :: Double
    , slant :: Slant
    , weight :: Weight
    , spacing :: Spacing
    , hinting :: Bool
    , antialias :: Bool
    , pixelsize :: Maybe Double
    }
    deriving (Read, Eq)

showFont :: XftFont -> String
showFont xft =
    let toLower = T.unpack . T.toLower . T.pack
        showLower f = toLower . show . f
        ps = pixelsize xft
     in family xft
            ++ "-"
            ++ showLower size xft
            ++ ":slant="
            ++ showLower slant xft
            ++ ":weight="
            ++ showLower weight xft
            ++ ":spacing="
            ++ showLower spacing xft
            ++ ":hinting="
            ++ showLower hinting xft
            ++ ":antialias="
            ++ showLower antialias xft
            ++ case ps of
                Just p ->
                    ":pixelsize=" ++ show (Just p)
                Nothing ->
                    ""

instance Show XftFont where
    show = showFont :: XftFont -> String

-- static data
modm, alt, shift, ctrl, noMod :: KeyMask
modm = mod4Mask
alt = mod1Mask
shift = shiftMask
ctrl = controlMask
noMod = noModMask

modShift, modCtrl, modAlt :: KeyMask
modShift = modm .|. shift
modCtrl = modm .|. ctrl
modAlt = modm .|. alt

altShift, ctrlAlt :: KeyMask
altShift = alt .|. shift
ctrlAlt = alt .|. ctrl

black, white, purple, gray :: HexColorCode
black = "#000000"
white = "#FFFFFF"
purple = "#A865C9"
gray = "#DDDDDD"

f86AudioNext, f86AudioPrev, f86AudioPlay, f86AudioPause :: KeySym
f86AudioNext = xF86XK_AudioNext
f86AudioPrev = xF86XK_AudioPrev
f86AudioPlay = xF86XK_AudioPlay
f86AudioPause = xF86XK_AudioPause

f86AudioMute, f86AudioRaise, f86AudioLower, f86AudioMicMute :: KeySym
f86AudioMute = xF86XK_AudioMute
f86AudioRaise = xF86XK_AudioRaiseVolume
f86AudioLower = xF86XK_AudioLowerVolume
f86AudioMicMute = xF86XK_AudioMicMute

term, browser, editor, shell, hledger :: Command
term = "kitty"
browser = "firefox"
editor = "nvim"
shell = "zsh"
-- do ledger env file
hledger = "hledger-ui -f ~/finance/.hledger.journal"

-- home, anonPro :: String
home, source :: Path
home = "/home/carterlevo"
source = home ++ "/.xmonad"

fallbackFont :: XftFont
fallbackFont =
    XFT
        { family = "DejaVu Sans Mono"
        , size = 14.0
        , weight = Bold
        , slant = Roman
        , spacing = Mono
        , hinting = True
        , antialias = True
        , pixelsize = Nothing
        }

hackFont :: XftFont
hackFont =
    fallbackFont
        { family = "Hack"
        }

firaCode :: XftFont
firaCode =
    fallbackFont
        { family = "Fira Code"
        }

firaSans :: XftFont
firaSans =
    fallbackFont
        { family = "Fira Sans"
        }

inputMono :: XftFont
inputMono =
    fallbackFont
        { family = "Input Mono"
        }

inputSerif :: XftFont
inputSerif =
    fallbackFont
        { family = "Input Serif"
        }

anonymousPro :: XftFont
anonymousPro =
    fallbackFont
        { family = "Anonymous Pro"
        }

sourceCodePro :: XftFont
sourceCodePro =
    fallbackFont
        { family = "Source Code Pro"
        }

cliCells, guiCells, bmCells, dfCells :: [Cell]
cliCells =
    [ ("Ipython", "ipython")
    , ("GHCi", "ghci")
    , ("Radian", "radian")
    , ("GiNsh", "ginsh")
    , ("Basic Calculator", "bc -q")
    , ("Maxima", "maxima")
    , ("Cmus", "cmus")
    , ("Calcurse", "calcurse")
    , ("GCal", "gcalcli")
    , ("FriCAS", "fricas")
    , ("Gap", "gap")
    , ("Sc-im", "sc-im")
    , ("Neomutt", "neomutt")
    , ("VisiData", "vd")
    , ("WeeChat", "weechat")
    , ("Gnuplot", "gnuplot")
    , ("Htop", "htop")
    , ("Btop++", "btop")
    , ("S-tui", "s-tui")
    , ("Termshark", "termshark")
    , ("Ncdu", "ncdu")
    , ("Cfdisk", "cfdisk")
    , ("Lvm Manager", "lvm")
    , ("Ranger", "ranger")
    , ("Ledger", "ledger")
    , ("Hledger", hledger)
    , ("Spotify", "spt")
    ]
guiCells =
    [ ("Vimiv", "vimiv")
    , -- , ("Spotify", "spotify-launcer")
      ("Gimp", "gimp")
    , ("wxMaxima", "wxmaxima")
    , ("FontForge", "fontforge")
    , ("Firefox", "firefox")
    , ("Qutebrowser", "qutebrowser")
    , ("Xasy", "xasy")
    , ("Inkscape", "inkscape")
    , ("Trader Workstation", "tws")
    , ("TradingView", "tradingview")
    ]
dfCells =
    [ ("Config", "~/.config")
    , ("Neovim", "~/.config/nvim")
    , ("Kitty", "~/.config/kitty")
    , ("Ranger", "~/.config/ranger")
    , ("Bash", "~/.bashrc")
    , ("Git", "~/.config/git/config")
    , ("Dunst", "~/.config/dunst/dunstrc")
    , ("Isync", "~/.config/mbsync/config")
    , ("Msmtp", "~/.config/msmtp/config")
    , ("Neomutt", "~/.config/neomutt")
    , ("XMonad", "~/.xmonad")
    , ("Xmobar", "~/.xmobar")
    , ("KMonad", "~/.config/kmonad")
    , ("Firefox", "~/.mozilla")
    , ("StartX", "~/.xinitrc")
    , ("Zsh", "~/.zshrc")
    , ("Zathura", "~/.config/zathura/zathurarc")
    , ("Qutebrowser", "~/.config/qutebrowser")
    ]
bmCells =
    [ ("Gmail", "mail.google.com")
    , ("Posteo", "posteo.de/en")
    , ("LessWrong", "lesswrong.com")
    , ("Leetcode", "leetcode.com")
    , ("Python Documentation", "docs.python.com")
    , ("Haskell Wiki", "wiki.haskell.org")
    , ("Vim cheatsheet", "vim.rotrr.com")
    , ("Neovim Documentation", "neovio.io/doc")
    , ("Git Documentation", "git-scm.com/doc")
    , ("Arch Wiki", "wiki.archlinux.org")
    , ("Linux manpages", "linux.die.net/man")
    , ("Wikichip", "wikichip.org")
    , ("Libgen", "libgen.is")
    , ("Sci-hub", "sc-hubtw.hkvisa.net")
    , ("Wikibooks", "wikibooks.org")
    , ("Open Library", "openlibrary.org")
    , ("arXiv", "arxiv.org")
    , ("SSRN", "ssrn.com")
    , ("JSTOR", "jstor.org")
    , ("Lexis Nexis", "lexisnexis.org")
    , ("ACM Library", "dl.acm.org")
    , ("CIA World Factbook", "cia.gov/the-world-factbook")
    , ("Project Euler", "projecteuler.net")
    , ("Wolfram Mathworld", "mathworld.wolfram.com")
    ]

searchCells :: [Cell]
searchCells =
    [ ("", "")
    ]
