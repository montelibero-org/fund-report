module Main (main) where

import           Control.Lens ((&~), (.=), (.~), (^.))
import           Control.Monad (void)
import           Data.Aeson (FromJSON)
import           Data.Default (def)
import           Data.Function ((&))
import           Data.List (sortOn)
import qualified Data.Map.Strict as Map
import           Data.Ord (Down (..))
import           Data.Ratio ((%))
import           GHC.Generics (Generic)
import           Graphics.Rendering.Chart (HTextAnchor (HTA_Centre), PickFn,
                                           PieLayout (..), Renderable,
                                           VTextAnchor (VTA_Top), addMargins,
                                           fillBackground, font_size, label,
                                           nullPickFn, pieChartToRenderable,
                                           pie_data, pie_label_style, pie_plot,
                                           pie_title, pitem_label, pitem_value,
                                           setPickFn)
import           Graphics.Rendering.Chart.Backend.Cairo (fo_size,
                                                         renderableToFile)
import           Graphics.Rendering.Chart.Easy (execEC)
import           Graphics.Rendering.Chart.Grid (Grid, aboveN, gridToRenderable,
                                                tval, weights)
import           Network.Wreq (asJSON, get, responseBody)
import           Text.Printf (printf)

newtype ResponseOk a = ResponseOk{_embedded :: Embedded a}
  deriving (FromJSON, Generic)

newtype Embedded a = Embedded{records :: [a]}
  deriving (FromJSON, Generic)

data Holder = Holder{account, balance :: String}
  deriving (FromJSON, Generic)

data Member = Member{account :: String, balance :: Rational}

data Foundation = Foundation
  {assetName, assetIssuer :: String, treasury :: Maybe String}

main :: IO ()
main =
  do
    mtlMembers     <- getMembers mtl
    mtlcityMembers <- getMembers mtlcity
    let mtlcityViaMtlMembers = substMembers mtl mtlMembers mtlcityMembers
    let
      grid =
        aboveN
          [ pieToGrid $ membersPie mtl     mtlMembers
          , pieToGrid $ membersPie mtlcity mtlcityMembers
          , pieToGrid $ membersPie mtlcity mtlcityViaMtlMembers
          ]
    void $
      renderableToFile fileOptions reportFile $
      fillBackground def $
      gridToRenderable grid
  where
    mtl =
      Foundation
        { assetName = "MTL"
        , assetIssuer =
            "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"
        , treasury =
            Just "GDX23CPGMQ4LN55VGEDVFZPAJMAUEHSHAMJ2GMCU2ZSHN5QF4TMZYPIS"
        }
    mtlcity =
      Foundation
        { assetName = "MTLCITY"
        , assetIssuer =
            "GDUI7JVKWZV4KJVY4EJYBXMGXC2J3ZC67Z6O5QFP4ZMVQM2U5JXK2OK3"
        , treasury = Nothing
        }
    reportFile  = "report.png"
    fileOptions = def & fo_size .~ (1000, 1500)

assetId :: Foundation -> String
assetId Foundation{assetName, assetIssuer} = assetName <> "-" <> assetIssuer

membersPie :: Foundation -> [Member] -> PieLayout
membersPie Foundation{assetName} members =
  execEC $ do
    pie_title .= assetName <> " holders/members"
    pie_plot . pie_label_style . font_size .= 20
    pie_plot . pie_data .=
      [ def &~ do
          pitem_value .= realToFrac balance
          pitem_label .=
            "..." <> drop (length account - 4) account <> ", " <>
            showLocal ' ' (round balance :: Integer) <>
            ", " <> printf "%.1f%%" (realToFrac share :: Double)
      | Member{account, balance} <- members
      , let share = balance / sumBalance * 100
      ]
  where
    sumBalance = sum $ map (\Member{..} -> balance) members

pieToGrid :: PieLayout -> Grid (Renderable (PickFn a))
pieToGrid PieLayout{_pie_margin, _pie_plot, _pie_title, _pie_title_style} =
  aboveN
    [ tval $ addMargins (_pie_margin / 2, 0, 0, 0) (setPickFn nullPickFn title)
    , weights (1, 1) $ tval $
      addMargins (_pie_margin, _pie_margin, _pie_margin, _pie_margin) $
      pieChartToRenderable _pie_plot
    ]
  where
    title = label _pie_title_style HTA_Centre VTA_Top _pie_title

getMembers :: Foundation -> IO [Member]
getMembers foundation@Foundation{treasury} = do
  holders <- getHolders foundation
  pure
    [ Member{account, balance = read @Integer balance % 10_000_000}
    | Holder{account, balance} <- holders
    , Just account /= treasury
    ]


getHolders :: Foundation -> IO [Holder]
getHolders foundation =
  do
    r <-
      asJSON =<<
      get
        (concat
          [ "https://api.stellar.expert/explorer/", network
          , "/asset/", assetId foundation, "/holders"
          ])
    let ResponseOk{_embedded = Embedded{records}} = r ^. responseBody
    pure records
  where
    network = "public"

showLocal :: Char -> Integer -> String
showLocal groupSeparator = snd . foldr go (0 :: Int, "") . show where
  go c (len, r) =
    (len + 1, c : [groupSeparator | len `mod` 3 == 0, len > 0] ++ r)

substMembers :: Foundation -> [Member] -> [Member] -> [Member]
substMembers Foundation{treasury = parentTreasury} parentMembers members =
  map (\(account, balance) -> Member{..}) $
    sortOn (Down . snd) $
    Map.assocs $
    Map.fromListWith (+) $
    concat
      [ if Just directAccount == parentTreasury then let
          parentBalance = directBalance
          in
          [ (account, balance / sumParent * parentBalance)
          | Member{account, balance} <- parentMembers
          ]
        else
          [(directAccount, directBalance)]
      | Member{account = directAccount, balance = directBalance} <- members
      ]
  where
    sumParent = sum $ map (\Member{..} -> balance) parentMembers
