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

type Mempool = [Tx]
type TxBlock = [Tx]


getSender :: Tx -> AccountID
getSender (sender, _, _, _) = sender

getReceiver :: Tx -> AccountID
getReceiver (_,receiver, _, _) = receiver

balance :: Account -> Token -> TokenAmount
balance (userID, amountA, amountB) A = amountA
balance (userID, amountA, amountB) B = amountB


uniSwapExchange :: AccountStates -> Tx -> AccountStates
-- Use lenses to 'modify' account states
uniSwapExchange accountStates (userID, uniSwapID, token, tokenAmount)
   | userbalanceInsufficient = accountStates
   | token==A = accountStates & (ix userID) .~ userUpdateA2B & (ix uniSwapID) .~ uniswapUpdateA2B
   | token==B = accountStates & (ix userID) .~ userUpdateB2A & (ix uniSwapID) .~ uniswapUpdateB2A
  where
   userbalanceInsufficient = balance (accountStates!!userID) token < tokenAmount

   userA = balance (accountStates!!userID) A
   userB = balance (accountStates!!userID) B
   uniSwapA = balance (accountStates!!2) A
   uniSwapB = balance (accountStates!!2) B
   dA = uniSwapA * tokenAmount / (uniSwapB + tokenAmount) 
   dB = uniSwapB * tokenAmount / (uniSwapA + tokenAmount) 

   userUpdateA2B = (userID, userA - tokenAmount, userB + dB)
   userUpdateB2A = (userID, userA + dA, userB - tokenAmount)
   uniswapUpdateA2B = (uniSwapID, uniSwapA + tokenAmount, uniSwapB - dB)
   uniswapUpdateB2A = (uniSwapID, uniSwapA - dA, uniSwapB + tokenAmount)



betOnExchange :: AccountStates -> Double -> Tx -> AccountStates
-- oracle account, bet threshold, bet amount, updated user account
-- Use lenses to 'modify' account states
betOnExchange accountStates ratio (userID, betID, betToken, betAmount)
   | userbalanceInsufficient = accountStates
   | (uniSwapA/uniSwapB >= ratio && betToken == A) =  accountStates & (ix userID) .~ userUpdateBetA_userWin & (ix betID) .~ betUpdateA_userWin
   | (uniSwapA/uniSwapB >= ratio && betToken == B) =  accountStates & (ix userID) .~ userUpdateBetB_userWin & (ix betID) .~ betUpdateB_userWin
   | (uniSwapA/uniSwapB < ratio && betToken == A) =  accountStates & (ix userID) .~ userUpdateBetA_userLose & (ix betID) .~ betUpdateA_userLose
   | (uniSwapA/uniSwapB < ratio && betToken == B) =  accountStates & (ix userID) .~ userUpdateBetB_userLose & (ix betID) .~ betUpdateB_userLose
  where
   uniSwapA = balance (accountStates!!2) A
   uniSwapB = balance (accountStates!!2) B
   userA = balance (accountStates!!userID) A
   userB = balance (accountStates!!userID) B
   userbalanceInsufficient = balance (accountStates!!userID) betToken < betAmount

   betA = balance (accountStates!!betID) A
   betB = balance (accountStates!!betID) B

   userUpdateBetA_userWin = (userID, userA + betA + betAmount, userB)
   betUpdateA_userWin = (betID, 0, betB)

   userUpdateBetB_userWin = (userID, userA, userB + betB + betAmount)
   betUpdateB_userWin = (betID, betA, 0)

   userUpdateBetA_userLose = (userID, userA - betAmount, userB)
   betUpdateA_userLose = (betID, betA + betAmount, betB)

   userUpdateBetB_userLose = (userID, userA, userB - betAmount)
   betUpdateB_userLose = (betID, betA, betB + betAmount)


executeTx :: AccountStates -> Tx -> AccountStates
executeTx states tx
   | getReceiver tx == 2 = uniSwapExchange states tx
   | getReceiver tx == 3 = betOnExchange states 1 tx
   | otherwise = states

executeBlock :: AccountStates -> TxBlock -> AccountStates
executeBlock accountStates_init block = foldl executeTx accountStates_init block
   


p0_ac = (0, 10, 10)
p1_ac = (1, 10, 10)
uniswap_ac = (2, 100, 100)
bet_ac = (3, 100, 100)

p0_ID :: AccountID
p0_ID = 0

p1_ID :: AccountID
p1_ID = 1

uniswap_ID :: AccountID
uniswap_ID = 2

bet_ID :: AccountID
bet_ID = 3

tx1 = (p0_ID, uniswap_ID, A, 2.0)
tx2 = (p1_ID, uniswap_ID, A, 3.0)
tx3 = (p0_ID, uniswap_ID, B, 2.0)
tx4 = (p1_ID, uniswap_ID, B, 3.0)

txBet :: TokenAmount -> Tx
txBet amount = (p0_ID, bet_ID, A, amount)

blockPayoff :: AccountStates -> TxBlock -> AccountID -> Payoff
-- (could add choice of payoff token, now set to A)
blockPayoff initStates block userID = newBalance - oldBalance
  where
   newBalance = balance ((executeBlock initStates block)!!userID) A
   oldBalance = balance (initStates!!userID) A


initAccounts :: AccountStates
initAccounts = [p0_ac, p1_ac, uniswap_ac, bet_ac]

block1 :: TxBlock
block1 = [tx1, tx2, tx3, tx4]

txOrderingGame  = [opengame|
   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player1" (const actionSpace);
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


-- Somehow, permutations [tx1, tx2, tx3] and [tx2, tx1, tx3] are both equilibria. Why?
-- Is the only thing that matters that tx3 is last?
-- Yes! Since the only thing that matters is the ratio A/B in the uniswap account, the ordering of the two transactions doesn't matter, only that 2A is converted twice. 






blockWithbet :: TokenAmount -> TxBlock
blockWithbet amount = [tx1, tx2, tx3, txBet amount]

txOrderingGame_withBet  = [opengame|
   inputs    :      ;
   feedback  :      ;

   :----------------------------:

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player1" (const betAmounts);
   outputs   : betAmount ;
   returns   :  0   ;

   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "player1" (const actionSpace);
   outputs   : ordering ;
   returns   : blockPayoff initAccounts (blockPerm ordering betAmount) 0     ;
   :----------------------------:

   outputs   :      ;
   returns   :      ;
  |]
  where
   betAmounts = [0,0.1..(balance (initAccounts!!0) A)]
   actionSpace = [0..(product [1..4]-1)]
   blockPerm = \orderChoice betAmount -> ((permutations $ blockWithbet betAmount)!!orderChoice)


betAndOrderStrat :: Double -> Int -> List '[Kleisli Stochastic () Double,
   Kleisli Stochastic () Int]
betAndOrderStrat amount orderChoice = Kleisli (\x -> playDeterministically amount) ::- Kleisli (\x -> playDeterministically orderChoice) ::- Nil

analyseTxOrderingGame_withBet strat = generateIsEq $ evaluate txOrderingGame_withBet strat void
-- analyseTxOrderingGame_withBet $ betAndOrderStrat 4 0