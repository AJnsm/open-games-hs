{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}



module Examples.CakeCutting.PieCutting where
import Engine.Engine
import Preprocessor.Preprocessor

import           Data.Tuple.Extra (uncurry3)
import           Engine.Engine
import           Preprocessor.Preprocessor


-----------------------
-- 1. Types and payoffs

-- 1.0 pieSharing Game

type Pie = Double
type Proposal = Double

data ResponderAction = Accept | Reject
  deriving (Eq,Ord,Show)


pieSharingGamePayoff_proposer,pieSharingGamePayoff_responder :: Pie -> Proposal -> ResponderAction -> Payoff

pieSharingGamePayoff_proposer pie proposal reaction =
  if reaction == Accept then (pie - proposal) else proposal

pieSharingGamePayoff_responder pie proposal reaction =
  if reaction == Accept then proposal else (pie - proposal)


  --------------------
-- 1. Representation
-- 1.0. pieSharing Game
pieSharingGame pie = [opengame|

   inputs    :      ;
   feedback  :      ;

   :----------------------------:
   inputs    :      ;
   feedback  :      ;
   operation : dependentDecision "proposer" (const [0..pie]);
   outputs   : proposal ;
   returns   : pieSharingGamePayoff_proposer pie proposal reaction;

   inputs    : proposal ;
   feedback  :      ;
   operation : dependentDecision "responder" (const [Accept,Reject]);
   outputs   : reaction ;
   returns   : pieSharingGamePayoff_responder pie proposal reaction ;

   :----------------------------:

   outputs   :      ;
   returns   :      ;
   |]




---------------
-- 2.  Analysis
-- 2.0. Piecutting Game
isEquilibriumPieSharingGame pie strat = generateIsEq $ evaluate (pieSharingGame pie) strat void

fullPieSize :: Double
fullPieSize = 10


-- proposer plays selfish
proposerStrategyPCG_Selfish :: Kleisli Stochastic () Proposal
proposerStrategyPCG_Selfish = pureAction 2

-- proposer plays fair
proposerStrategyPCG_Fair :: Kleisli Stochastic () Proposal
proposerStrategyPCG_Fair = pureAction (fullPieSize/2)

-- responder always accepts
responderStrategyPCG_Accept :: Kleisli Stochastic Proposal ResponderAction
responderStrategyPCG_Accept = pureAction Accept

-- responder chooses biggest piece (i.e. accepts iff piece is at least half of the pie)
responderStrategyPCG_BiggestPiece :: Kleisli Stochastic Proposal ResponderAction
responderStrategyPCG_BiggestPiece = Kleisli $ chooseBiggestPiece fullPieSize
   where
    chooseBiggestPiece :: Pie -> Proposal -> Stochastic ResponderAction
    chooseBiggestPiece fullPie proposal = if (proposal >= (fullPie/2)) then playDeterministically Accept else playDeterministically Reject
  

stratTuplePCG_selfishProposer_acceptingResponder = proposerStrategyPCG_Selfish ::- responderStrategyPCG_Accept ::- Nil

stratTuplePCG_fairProposer_acceptingResponder = proposerStrategyPCG_Fair ::- responderStrategyPCG_Accept ::- Nil

stratTuplePCG_selfishProposer_selfishResponder = proposerStrategyPCG_Selfish ::- responderStrategyPCG_BiggestPiece ::- Nil

stratTuplePCG_fairProposer_selfishResponder = proposerStrategyPCG_Fair ::- responderStrategyPCG_BiggestPiece ::- Nil



-- Example usage
-- isEquilibriumPieSharingGame 10 stratTuplePCG_selfishProposer_acceptingResponder
-- isEquilibriumPieSharingGame 10 stratTuplePCG_fairProposer_acceptingResponder
-- isEquilibriumPieSharingGame 10 stratTuplePCG_selfishProposer_selfishResponder
-- isEquilibriumPieSharingGame 10 stratTuplePCG_fairProposer_selfishResponder



