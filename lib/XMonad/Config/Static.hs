module XMonad.Config.Static
    ( modm
    , alt
    , shift
    , ctrl
    , noMod
    , modShift
    , modCtrl
    , modAlt
    , altShift
    , ctrlAlt
    , black
    , white
    , purple
    , gray
    , term
    , shell
    , browser
    , editor
    , home
    -- , anonPro
    , cliCells
    , guiCells
    , bmCells
    , dfCells
    ) where

import Data.Bits ((.|.))
import Graphics.X11
import XMonad.Config.Data

-- static data
modm, alt, shift, ctrl, noMod :: KeyMask
modm     = mod4Mask
alt      = mod1Mask
shift    = shiftMask
ctrl     = controlMask
noMod    = noModMask

modShift, modCtrl, modAlt :: KeyMask
modShift = modm .|. shift
modCtrl  = modm .|. ctrl
modAlt   = modm .|. alt

altShift, ctrlAlt :: KeyMask
altShift = alt  .|. shift
ctrlAlt  = alt  .|. ctrl

black, white, purple, gray :: HexColorCode
black  = "#000000"
white  = "#FFFFFF"
purple = "#A865C9"
gray   = "#DDDDDD"


term, browser, editor, shell, hledger :: Command
term     = "kitty"
browser  = "firefox"
editor   = "nvim"
shell    = "zsh"
-- do ledger env file
hledger  = "hledger-ui -f ~/finance/.hledger.journal"

-- home, anonPro :: String
home :: Path
home     = "/home/carterlevo"
-- anonPro  = "xft:Anonymous Pro Mono:weight=bold:pixelsize=14:antialias=true"

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
    ]
guiCells =
    [ ("Vimiv", "vimiv")
    , ("Spotify", "spotify-launcer")
    , ("Gimp", "gimp")
    , ("wxMaxima", "wxmaxima")
    , ("Xasy", "xasy")
    , ("Inkscape", "inkscape")
    , ("Trader Workstation", "tws")
    , ("TradingView", "tradingview")
    ]
dfCells =
    [ ("Neovim", "~/.config/nvim")
    , ("Kitty", "~/.config/kitty")
    , ("Ranger", "~/.config/ranger/")
    , ("Bash", "~/.bashrc")
    , ("Qutebrowser", "~/.config/qutebrowser")
    , ("XMonad", "~/.config/xmonad/")
    , ("Xmobar", "~/.config/xmonad/xmobar")
    , ("KMonad", "~/.config/kmonad")
    , ("i3", "~/.config/i3/config")
    , ("py3status", "~/.config/py3status/config")
    , ("Zathura", "~/.config/zathura/zathurarc")
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
