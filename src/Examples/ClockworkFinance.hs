{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples.ClockworkFinance where
import Engine.Engine
import Preprocessor.Preprocessor
import Engine.BayesianGames
import Data.List
import Control.Lens

data Token = A | B
  deriving (Eq,Ord,Show)

type TokenAmount = Double

type AccountID = Int

-- ID, amount A, amount B
type Account = (AccountID, TokenAmount, TokenAmount)

type AccountStates = [Account]

-- Sender, Receiver, Token, Amount
-- Should this also have a data field?
type Tx = (AccountID, AccountID, Token, TokenAmount)

type TxBlock = [Tx]

getAccountID :: Account -> AccountID
getAccountID (name, _, _) = name

getReceiver :: Tx -> AccountID
getReceiver (_,receiver, _, _) = receiver

balance :: Account -> Token -> TokenAmount
balance (userID, amountA, amountB) token
    | token==A = amountA
    | otherwise =  amountB

uniSwapExchange :: AccountStates -> Tx -> AccountStates
-- Use lenses to 'modify' account states
uniSwapExchange accountStates (userID, uniSwapID, token, tokenAmount)
   | userbalanceInsufficient = accountStates
   | otherwise = accountStates & (ix userID) .~ userUpdateA2B & (ix uniSwapID) .~ uniswapUpdate
  where
   user_bal = balance (accountStates!!userID)
   userbalanceInsufficient = user_bal token < tokenAmount
   uniSwap_bal = balance (accountStates!!uniSwapID)
   dA = uniSwap_bal A * tokenAmount / (uniSwap_bal B + tokenAmount) 
   dB = uniSwap_bal B * tokenAmount / (uniSwap_bal A + tokenAmount) 
   userUpdateA2B = 
      if token == A
      then (userID, user_bal A - tokenAmount, user_bal B + dB)
      else (userID, user_bal A + dA, user_bal B - tokenAmount)
   uniswapUpdate = 
      if token == A
      then (uniSwapID, uniSwap_bal A + tokenAmount, uniSwap_bal B - dB)
      else (uniSwapID, uniSwap_bal A - dA, uniSwap_bal B + tokenAmount)



betOnExchange :: AccountStates -> AccountID -> Double -> Tx -> AccountStates
-- Old state, price oracle, bet threshold, bet amount, new state
-- Use lenses to 'modify' account states
betOnExchange accountStates oracleID ratio (userID, betID, betToken, betAmount)
   | (userbalanceInsufficient || betAmount<=0) = accountStates
   | ((uniSwap_bal A)/(uniSwap_bal B) >= ratio) = accountStates & (ix userID) .~ userUpdate_userWin & (ix betID) .~ betUpdate_userWin
   | ((uniSwap_bal A)/(uniSwap_bal B) <  ratio) = accountStates & (ix userID) .~ userUpdate_userLose & (ix betID) .~ betUpdate_userLose
  where
   uniSwap_bal = balance (accountStates!!oracleID)
   bet_bal = balance (accountStates!!betID)
   user_bal = balance (accountStates!!userID)
   userbalanceInsufficient = user_bal betToken < betAmount

   -- The prize is either the amount bet, or the remaining balance if that is less
   prize = minimum [betAmount, balance (accountStates!!betID) betToken]

   userUpdate_userWin = 
      if betToken==A 
      then (userID, user_bal A + prize, user_bal B) 
      else (userID, user_bal A, user_bal B + prize)
   betUpdate_userWin =  
      if betToken==A
      then (betID, bet_bal A - betAmount, bet_bal B)
      else (betID, bet_bal A, bet_bal B - betAmount)
   userUpdate_userLose = 
      if betToken==A
      then (userID, user_bal A - betAmount, user_bal B)
      else (userID, user_bal A, user_bal B - betAmount)
   betUpdate_userLose = 
      if betToken==A
      then (betID, bet_bal A + betAmount, bet_bal B)
      else (betID, bet_bal A, bet_bal B + betAmount)




p0_ac = (0, 10, 10)
p1_ac = (1, 10, 10)
uniswap_ac = (2, 100, 100)
bet_ac = (3, 100, 100)

initAccounts :: AccountStates
initAccounts = [p0_ac, p1_ac, uniswap_ac, bet_ac]

executeTx :: AccountStates -> Tx -> AccountStates
executeTx states tx
   | getReceiver tx == 2 = uniSwapExchange states tx
   | getReceiver tx == 3 = betOnExchange states 2 1.1 tx
   | otherwise = states

executeBlock :: AccountStates -> TxBlock -> AccountStates
executeBlock accountStates_init block = foldl executeTx accountStates_init block
   

tx1 = (getAccountID p0_ac, getAccountID uniswap_ac, A, 2.0)
tx2 = (getAccountID p1_ac, getAccountID uniswap_ac, A, 3.0)
tx3 = (getAccountID p0_ac, getAccountID uniswap_ac, B, 2.0)
tx4 = (getAccountID p1_ac, getAccountID uniswap_ac, B, 3.0)

txBet :: TokenAmount -> Tx
txBet amount = (getAccountID p0_ac, getAccountID bet_ac, A, amount)

blockPayoff :: AccountStates -> TxBlock -> AccountID -> Payoff
-- (could add choice of payoff token, now set to A)
blockPayoff initStates block userID = newBalance - oldBalance
  where
   newBalance = balance ((executeBlock initStates block)!!userID) A
   oldBalance = balance (initStates!!userID) A



block1 :: TxBlock
block1 = [tx1, tx2, tx3, tx4]

txOrderingGame  = [opengame|
   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "proposer" (const actionSpace);
   outputs   : ordering ;
   returns   : blockPayoff initAccounts (blockPerm ordering) 0     ;
   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]
  where
   actionSpace = [0..(product [1..4]-1)]
   blockPerm = \x -> ((permutations block1)!!x)


analyseTxOrderingGame strat = generateIsEq $ evaluate txOrderingGame strat void

choosePerm :: Int -> List '[Kleisli Stochastic () Int]
choosePerm permID = pureAction permID ::- Nil

-- analyseTxOrderingGame $ choosePerm 0
-- analyseTxOrderingGame $ choosePerm 4



blockWithbet :: TokenAmount -> TxBlock
blockWithbet amount = [tx1, tx2, tx3, txBet amount]

txOrderingGame_withBet  = [opengame|
   inputs    :      ;
   feedback  :      ;

   :----------------------------:

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "proposer" (const betAmounts);
   outputs   : betAmount ;
   returns   :  0   ;

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "proposer" (const actionSpace);
   outputs   : ordering ;
   returns   : blockPayoff initAccounts (blockPerm ordering betAmount) 0     ;
   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]
  where
   betAmounts = [0,0.1..(balance (initAccounts!!0) A)]
   actionSpace = [0..(product [1..(length $ blockWithbet 0)]-1)]
   blockPerm = \orderChoice betAmount -> ((permutations $ blockWithbet betAmount)!!orderChoice)


betAndOrderStrat :: Double -> Int -> List '[Kleisli Stochastic () Double,
   Kleisli Stochastic () Int]
betAndOrderStrat amount orderChoice = Kleisli (\x -> playDeterministically amount) ::- Kleisli (\x -> playDeterministically orderChoice) ::- Nil

analyseTxOrderingGame_withBet strat = generateIsEq $ evaluate txOrderingGame_withBet strat void

-- analyseTxOrderingGame_withBet $ betAndOrderStrat 4 0
-- analyseTxOrderingGame_withBet $ betAndOrderStrat 4 17
-- analyseTxOrderingGame_withBet $ betAndOrderStrat 8 17