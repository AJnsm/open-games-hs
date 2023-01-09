{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}



module Examples.CakeCutting.PieCutting_open where
import Engine.Engine
import Preprocessor.Preprocessor


-----------------------
-- 1. Types and payoffs

-- 1.0 pieSharing Game

type Pie = Double
type Offer = (Double, Double)

data ResponderAction = Accept | Reject
  deriving (Eq,Ord,Show)






-- Now: Compositional version

-- idea: make the game *explicitly* open. Every player just sees an offer, and decides to accept or decline. Their payoff depends on the response of the next player. Does this lead to a situation in which every player offers 2/3 of their slice, except for the last two, who share the pie equally? Only if the BigPlayer rule is implemented. I will start without this rule and to the simple case, which should lead to an equilibrium of an unfair distribution of the nth player ending up with 2^-n of the pie. 


respondToOffer playerName = [opengame|

   inputs    : offer  ;
   feedback  : response;

   :----------------------------:
   inputs    : offer  ;
   feedback  : ;
   operation : dependentDecision playerName (const [Accept,Reject]) ;
   outputs   : response ;
   returns   : 0;
   // Postpone payoff calculation, so here just return a payoff of 0. 

   inputs    : offer, response  ;
   feedback  : ;
   operation : forwardFunction $ uncurry propagateChosenPiece;
   outputs   : chosenPiece ;
   returns   : ;
   :----------------------------:

   outputs   : chosenPiece ;
   returns   : ;
   |]



openPieSharingGamePayoff ::  Offer -> ResponderAction -> Payoff
openPieSharingGamePayoff newOffer nextResponse =
    if nextResponse == Accept then (snd newOffer) else (fst newOffer)

propagateChosenPiece :: Offer -> ResponderAction -> Double
propagateChosenPiece offer response = if response == Accept then (fst offer) else (snd offer)

fromOfferedPieceToOffer :: Double -> Double -> Offer
fromOfferedPieceToOffer chosenPiece offeredPiece = (offeredPiece, chosenPiece - offeredPiece)


offerNewSlice playerName = [opengame|
   inputs    : chosenPiece  ;
   feedback  : ;

   :----------------------------:


   inputs    : chosenPiece  ;
   feedback  : ;
   operation : dependentDecision playerName actionSpace ;
   outputs   : newOfferedPiece ;
   returns   : openPieSharingGamePayoff (newOfferedPiece, chosenPiece- newOfferedPiece) newResponse;

   inputs    : chosenPiece, newOfferedPiece  ;
   feedback  : ;
   operation : forwardFunction $ uncurry fromOfferedPieceToOffer;
   outputs   : newOffer ;
   returns   : ;

   :----------------------------:

   outputs   : newOffer ;
   returns   : newResponse;
   |]
   where 
      actionSpace x = [0, 0.1 .. x]

openPieSharing_unit playerName = [opengame|

   inputs    : inputOffer  ;
   feedback  : inputResponse;

   :----------------------------:
   inputs    : inputOffer  ;
   feedback  : inputResponse;
   operation : respondToOffer playerName ;
   outputs   : chosenPiece ;
   returns   : ;


   inputs    : chosenPiece ;
   feedback  : ;
   operation : offerNewSlice playerName ;
   outputs   : newOffer   ;
   returns   : newResponse;

   :----------------------------:

   outputs   : newOffer   ;
   returns   : newResponse;
   |]




-- *********** Closure game ************

pieSharingModule_closure pieSize = [opengame|

   inputs    :  ;
   feedback  :  ;

   :---------------------------:

   inputs:    ;
   feedback:  ;
   operation: dependentDecision "inputState" (const lsPie);
   outputs: defaultOffer;
   returns: 0;


   inputs    : defaultOffer ;
   feedback  : nullResponse ;
   operation : openPieSharing_unit "p1";
   outputs   : offer ;
   returns   : nextResponse;

   inputs:    offer;
   feedback:  ;
   operation: dependentDecision "continuationState" (const lsAccept);
   outputs: nextResponse;
   returns: 0;

   :----------------------------:
   outputs   :  ;
   returns   :  ;
   |]
  where 
    lsPie = [(pieSize, 0)]
    lsAccept = [Accept]





contextContPie :: Double -> StochasticStatefulContext
          Offer
          ResponderAction
          Offer
          ResponderAction

contextContPie fullPieSize = StochasticStatefulContext (pure ((),(fullPieSize, 0))) (\_ _ -> pure Accept)

evalOpenPie strat = evaluate (openPieSharing_unit "p1") strat (contextContPie 10)
isEquilibriumPieSharingGameOpen strat = generateIsEq $ evalOpenPie strat


-- -- Selfish strategy

selfishStrat_PieSharingOpen :: List [Kleisli Stochastic Offer ResponderAction,
         Kleisli Stochastic Double Double]
selfishStrat_PieSharingOpen = Kleisli pureAccept ::- Kleisli smallOffer ::- Nil
    where 
        pureAccept x = playDeterministically Accept
        smallOffer x = playDeterministically $ 0.2*x



-- *************** 2 players ***************


openPieSharing_twoPlayers = [opengame|

   inputs    : inputOffer  ;
   feedback  : inputResponse;

   :----------------------------:
   inputs    : inputOffer  ;
   feedback  : inputResponse;
   operation : openPieSharing_unit "p1" ;
   outputs   : newOffer1   ;
   returns   : newResponse1 ;

   inputs    : newOffer1  ;
   feedback  : newResponse1 ;
   operation : openPieSharing_unit "p2" ;
   outputs   : newOffer2   ;
   returns   : newResponse2 ;
   
   :----------------------------:

   outputs   : newOffer2   ;
   returns   : newResponse2 ;
   |]


pureAccept :: Offer -> Stochastic ResponderAction
pureAccept x = playDeterministically Accept

pureReject :: Offer -> Stochastic ResponderAction
pureReject x = playDeterministically Reject

acceptHalf :: Offer -> Stochastic ResponderAction
acceptHalf x = if (fst x) >= (snd x) then playDeterministically Accept else playDeterministically Reject


smallOffer :: Double -> Stochastic Double
smallOffer x = playDeterministically $ 0.2*x

bigOffer :: Double -> Stochastic Double
bigOffer x = playDeterministically $ 0.8*x

halfOffer :: Double -> Stochastic Double
halfOffer x = playDeterministically $ 0.5*x

offerOneThird :: Double -> Stochastic Double
offerOneThird x = playDeterministically $ 10/3

offerTwoThirds :: Double -> Stochastic Double
offerTwoThirds x = playDeterministically $ 20/3

zeroOffer :: Double -> Stochastic Double
zeroOffer x = playDeterministically $ 0



strat_2p_pureAc_smallOf :: List [Kleisli Stochastic Offer ResponderAction,
         Kleisli Stochastic Double Double, Kleisli Stochastic Offer ResponderAction,
         Kleisli Stochastic Double Double]
strat_2p_pureAc_smallOf = Kleisli pureAccept ::- Kleisli smallOffer ::- Kleisli pureAccept ::- Kleisli smallOffer ::- Nil

strat_2p_halfAc_smallOf = Kleisli acceptHalf ::- Kleisli smallOffer ::- Kleisli acceptHalf ::- Kleisli smallOffer ::- Nil

strat_2p_halfAc_halfOf = Kleisli acceptHalf ::- Kleisli halfOffer ::- Kleisli acceptHalf ::- Kleisli halfOffer ::- Nil

strat_2p_purRej_halfOf = Kleisli pureReject ::- Kleisli halfOffer ::- Kleisli pureReject ::- Kleisli halfOffer ::- Nil

strat_2p_eq = Kleisli pureAccept ::- Kleisli halfOffer ::- Kleisli acceptHalf ::- Kleisli zeroOffer ::- Nil

        

evalOpenPie_twoPlayers strat = evaluate (openPieSharing_twoPlayers) strat (contextContPie 10)
isEquilibriumPieSharingGameOpen_twoPlayers strat = generateIsEq $ evalOpenPie_twoPlayers strat 




-- *************** 3 players ***************

openPieSharing_threePlayers = [opengame|

   inputs    : inputOffer  ;
   feedback  : inputResponse;

   :----------------------------:
   inputs    : inputOffer  ;
   feedback  : inputResponse;
   operation : openPieSharing_unit "p1" ;
   outputs   : newOffer1   ;
   returns   : newResponse1 ;

   inputs    : newOffer1  ;
   feedback  : newResponse1 ;
   operation : openPieSharing_unit "p2" ;
   outputs   : newOffer2   ;
   returns   : newResponse2 ;

   inputs    : newOffer2  ;
   feedback  : newResponse2 ;
   operation : openPieSharing_unit "p3" ;
   outputs   : newOffer3   ;
   returns   : newResponse3 ;
   
   :----------------------------:

   outputs   : newOffer3   ;
   returns   : newResponse3 ;
   |]

strat_3p_greedyP1 = Kleisli acceptHalf ::- Kleisli smallOffer ::- Kleisli acceptHalf ::- Kleisli halfOffer ::- Kleisli acceptHalf ::- Kleisli halfOffer ::- Nil

strat_3p_fairPlay = Kleisli acceptHalf ::- Kleisli halfOffer ::- Kleisli acceptHalf ::- Kleisli halfOffer ::- Kleisli acceptHalf ::- Kleisli halfOffer ::- Nil

strat_3p_eq = Kleisli acceptHalf ::- Kleisli halfOffer ::- Kleisli acceptHalf ::- Kleisli halfOffer ::- Kleisli acceptHalf ::- Kleisli zeroOffer ::- Nil

strat_3p_nice = Kleisli acceptHalf ::- Kleisli offerTwoThirds ::- Kleisli acceptHalf ::- Kleisli offerOneThird ::- Kleisli acceptHalf ::- Kleisli zeroOffer ::- Nil


evalOpenPie_threePlayers strat = evaluate (openPieSharing_threePlayers) strat (contextContPie 10)
isEquilibriumPieSharingGameOpen_threePlayers strat = generateIsEq $ evalOpenPie_threePlayers strat 


-- Run:

-- isEquilibriumPieSharingGameOpen_threePlayers strat_3p_greedyP1
-- isEquilibriumPieSharingGameOpen_threePlayers strat_3p_fairPlay
-- isEquilibriumPieSharingGameOpen_threePlayers strat_3p_nice
-- isEquilibriumPieSharingGameOpen_threePlayers strat_3p_eq