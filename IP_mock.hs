-- Define data types for investments and portfolios
data Investment = Stock { symbol :: String, price :: Double }
                | Bond { symbol :: String, yield :: Double }
                | MutualFund { symbol :: String, returns :: [Double] }

data Portfolio = Portfolio { name :: String, holdings :: [Investment] }


-- Function to calculate the total value of a portfolio
portfolioValue :: Portfolio -> Double
portfolioValue portfolio = sum [calculateValue inv | inv <- holdings portfolio]
  where
    calculateValue (Stock _ p) = p
    calculateValue (Bond _ y) = y
    calculateValue (MutualFund _ returns) = last returns
    

-- Function to calculate the ROI of a portfolio
portfolioROI :: Portfolio -> Double
portfolioROI portfolio = (portfolioValue portfolio - initialInvestment) / initialInvestment
  where
    initialInvestment = sum [calculateInvestment inv | inv <- holdings portfolio]
    calculateInvestment (Stock _ p) = p
    calculateInvestment (Bond _ y) = y
    calculateInvestment (MutualFund _ _) = 0

-- Function to analyze the risk of a portfolio
portfolioRisk :: Portfolio -> Double
portfolioRisk portfolio = sqrt (sum [(calculateRisk inv) ^ 2 | inv <- holdings portfolio])
  where
    calculateRisk (Stock _ _) = 0.2 -- Placeholder value for stock risk
    calculateRisk (Bond _ _) = 0.1 -- Placeholder value for bond risk
    calculateRisk (MutualFund _ _) = 0.3 -- Placeholder value for mutual fund risk

-- Function to generate a report for a portfolio
generateReport :: Portfolio -> String
generateReport portfolio =
  "Portfolio: " ++ name portfolio ++
  "\nTotal Value: " ++ show (portfolioValue portfolio) ++
  "\nROI: " ++ show (portfolioROI portfolio) ++
  "\nRisk: " ++ show (portfolioRisk portfolio)

-- Example usage
main :: IO ()
main = do
  let stock1 = Stock "AAPL" 150.0
      bond1 = Bond "US Treasury" 2.5
      mutualFund1 = MutualFund "Vanguard" [1000.0, 1050.0, 1100.0, 1005.0, 1030.0]
      myPortfolio = Portfolio "My Investments" [stock1, bond1, mutualFund1]
  putStrLn (generateReport myPortfolio)
