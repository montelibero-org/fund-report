module Main (main) where

import           Control.Lens                           ((&~), (.=), (.~), (^.))
import           Control.Monad                          (void)
import           Data.Aeson                             (FromJSON)
import           Data.Default                           (def)
import           Data.Function                          ((&))
import           Data.Ratio                             ((%))
import           GHC.Generics                           (Generic)
import           Graphics.Rendering.Chart               (HTextAnchor (HTA_Centre),
                                                         PickFn, PieLayout (..),
                                                         Renderable,
                                                         VTextAnchor (VTA_Top),
                                                         addMargins,
                                                         fillBackground, label,
                                                         nullPickFn,
                                                         pieChartToRenderable,
                                                         pie_data, pie_plot,
                                                         pie_title, pitem_label,
                                                         pitem_value, setPickFn)
import           Graphics.Rendering.Chart.Backend.Cairo (fo_size,
                                                         renderableToFile)
import           Graphics.Rendering.Chart.Easy          (execEC)
import           Graphics.Rendering.Chart.Grid          (Grid, aboveN,
                                                         gridToRenderable, tval,
                                                         weights)
import           Network.Wreq                           (asJSON, get,
                                                         responseBody)
import           Text.Printf                            (printf)

newtype ResponseOk a = ResponseOk{_embedded :: Embedded a}
  deriving (FromJSON, Generic)

newtype Embedded a = Embedded{records :: [a]}
  deriving (FromJSON, Generic)

data Holder = Holder{account, balance :: String}
  deriving (FromJSON, Generic)

data Foundation = Foundation
  {assetName, assetIssuer :: String, treasury :: Maybe String}

main :: IO ()
main =
  do
    mtlHolders     <- getHolders mtl
    mtlcityHolders <- getHolders mtlcity
    let
      grid =
        aboveN
          [ pieToGrid $ holdersPie mtl     mtlHolders
          , pieToGrid $ holdersPie mtlcity mtlcityHolders
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
    fileOptions = def & fo_size .~ (1500, 1000)

assetId :: Foundation -> String
assetId Foundation{assetName, assetIssuer} = assetName <> "-" <> assetIssuer

holdersPie :: Foundation -> [Holder] -> PieLayout
holdersPie Foundation{assetName, treasury} holders =
  execEC $ do
    pie_title .= assetName <> " asset holders/members"
    pie_plot . pie_data .=
      [ def &~ do
          pitem_value .= balanceD
          pitem_label .=
            account <> ", " <> show (round balanceL :: Integer) <> ", " <>
            printf "%.1f" share <> "%"
      | (account, balanceI) <- members
      , let
        balanceD = fromIntegral balanceI
        balanceL = balanceI % 10_000_000
        share = balanceD / fromIntegral sumBalance * 100
      ]
  where
    members =
      [ (account, read @Integer balance)
      | Holder{account, balance} <- holders
      , Just account /= treasury
      ]
    sumBalance = sum $ map snd members

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

getHolders :: Foundation -> IO [Holder]
getHolders asset =
  do
    r <-
      asJSON =<<
      get
        (concat
          [ "https://api.stellar.expert/explorer/", network
          , "/asset/", assetId asset, "/holders"
          ])
    let ResponseOk{_embedded = Embedded{records}} = r ^. responseBody
    pure records
  where
    network = "public"
