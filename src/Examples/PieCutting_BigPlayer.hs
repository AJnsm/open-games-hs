{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Examples.PieCutting_BigPlayer where
import Engine.Engine
import Preprocessor.Preprocessor
import Engine.BayesianGames

-----------------------
-- 1. Types and payoffs


type Pie = Double
type Offer = (Double, Double)

data ResponderAction = Accept | Reject
  deriving (Eq,Ord,Show)


propagateChosenPiece :: Offer -> ResponderAction -> Double
propagateChosenPiece offer response = if response == Accept then (fst offer) else (snd offer)

fromOfferedPieceToOffer :: Double -> Double -> Offer
fromOfferedPieceToOffer chosenPiece offeredPiece = (offeredPiece, chosenPiece - offeredPiece)


-- *************** Games ***************

respondToOffer_dependent = [opengame|

   inputs    : playerName, offer  ;
   feedback  : response;

   :----------------------------:
   inputs    : playerName, offer  ;
   feedback  : ;
   operation : dependentRoleDecision  (\(x, y) -> [Accept,Reject]) ;
   outputs   : response ;
   returns   : 0;
   // Postpone payoff calculation, so here just return a payoff of 0. 

   :----------------------------:

   outputs   : response ;
   returns   : ;
   |]


offerNewSlice_dependent = [opengame|
   inputs    : playerName, offer, response ;
   feedback  : ;

   :----------------------------:
   inputs    : offer, response  ;
   feedback  : ;
   operation : forwardFunction $ uncurry propagateChosenPiece;
   outputs   : chosenPiece ;
   returns   : ;

   inputs    : playerName, chosenPiece ;
   feedback  : ;
   operation : dependentRoleDecision (\(playerName, x) -> actionSpace x) ;
   outputs   : newOfferedPiece ;
   returns   : 0 ;

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



orderBySize :: (Agent, Agent, Offer, ResponderAction) -> ((Agent, Payoff), (Agent, Payoff))
orderBySize (p1Name, p2Name, offer, response) = if (pieceP1 >= pieceP2) 
    then ((p1Name, pieceP1), (p2Name, pieceP2)) 
    else ((p2Name, pieceP2), (p1Name, pieceP1))
    where
        pieceP1 = if response==Accept then (snd offer) else (fst offer)
        pieceP2 = if response==Accept then (fst offer) else (snd offer)

-- ******** unit of 2 players with the BigPlayer rule *********
bigPlayer_unit player2Name payoffBP = [opengame|

   inputs    : inputBigPlayer, inputOffer  ;
   feedback  : ;

   :----------------------------:

   //Player 1 is last round's BigPlayer, so quickly "accepts" the full piece to propagate their piece into this round. They offer a slice to Player 2, but get no payoff yet!
   inputs    : inputBigPlayer, inputOffer, Accept  ;
   feedback  :  ;
   operation : offerNewSlice_dependent;
   outputs   : offerP1   ;
   returns   : responseP2_backwards ;

   //Player 2 responds
   inputs    : player2Name, offerP1   ;
   feedback  : responseP2_backwards;
   operation : respondToOffer_dependent ;
   outputs   : responseP2   ;
   returns   :  ;


   //Find the smallest player and give them their payoff
   inputs    : inputBigPlayer, player2Name, offerP1, responseP2;
   feedback  :  ;
   operation : forwardFunction $ orderBySize ;
   outputs   : (bigPlayerName, biggestPiece), (smallPlayerName, smallestPiece);
   returns   :  ;

   inputs    : smallPlayerName, smallestPiece ;
   feedback  :  ;
   operation : addRolePayoffs;
   outputs   :  ;
   returns   :  ;

   
   //The bigPlayer only gets a payoff if they are the last player
   inputs    : bigPlayerName, biggestPiece * payoffBP ;
   feedback  :  ;
   operation : addRolePayoffs;
   outputs   :  ;
   returns   :  ;

   :----------------------------:

   outputs   : bigPlayerName, fromOfferedPieceToOffer biggestPiece biggestPiece ;
   returns   :  ;
   |]


-- -- -- ********* Composition **********

bigPlayers_composed_3Players = [opengame|

   inputs    : inputBigPlayer, inputOffer ;
   feedback  : ;

   :----------------------------:

   inputs    : inputBigPlayer, inputOffer ;
   feedback  : ;
   operation : bigPlayer_unit "p2" 0 ;
   outputs   : bigPlayer1, offer1 ;
   returns   :  ;

   inputs    : bigPlayer1, offer1;
   feedback  : ;
   operation : bigPlayer_unit "p3" 1 ;
   outputs   : bigPlayer2, newOffer  ;
   returns   :  ;
   
   :----------------------------:

   outputs   : bigPlayer2, newOffer   ;
   returns   :  ;
   |]


contextContPie :: Double -> StochasticStatefulContext
          (String, Offer)
          ()
          (String, Offer)
          ()
contextContPie fullPieSize = StochasticStatefulContext 
    (pure ((),("p1", (fullPieSize, 0)))) (\_ _-> pure ())




-- *************** Strategies ***************


varAccept :: Double -> Offer -> Stochastic ResponderAction
varAccept r x = if (fst x) >= r then playDeterministically Accept else playDeterministically Reject

varAccept_conditional :: Double -> Double -> Offer -> Stochastic ResponderAction
varAccept_conditional r s x = if (fst x) >= r && (fst x) < (snd x) ||  (fst x) >= s then playDeterministically Accept else playDeterministically Reject



varOffer :: Double -> Double -> Stochastic Double
varOffer r x = playDeterministically $ r*x

varOffer_abs :: Double -> Double -> Stochastic Double
varOffer_abs r x = playDeterministically r


-- Strategies: p1 offers -> p2 responds -> BP offers -> p3 responds
strat_bigPlayer_3Players_test = 
    Kleisli (varOffer 0.51) ::- Kleisli  (varAccept 3.33) ::- 
    Kleisli (varOffer 0.51) ::- Kleisli (varAccept 3.33) ::- Nil


strat_bigPlayer_3Players_eq = 
    Kleisli (varOffer_abs 3.33) ::- Kleisli  (varAccept_conditional 3.33 6.66) ::-
    Kleisli (varOffer_abs 3.33) ::- Kleisli (varAccept 3.33) ::- Nil

-- *************** Evaluation ***************

evalOpenPie_BigPlayer_3Players strat = evaluate (bigPlayers_composed_3Players) strat (contextContPie 10)
isEquilibrium_BigPlayer_3Players strat = generateIsEq $ evalOpenPie_BigPlayer_3Players strat 



-- *********** Bigger composition ************


bigPlayers_composed_4Players = [opengame|

   inputs    : inputBigPlayer, inputOffer ;
   feedback  : ;

   :----------------------------:

   inputs    : inputBigPlayer, inputOffer ;
   feedback  : ;
   operation : bigPlayer_unit "p2" 0 ;
   outputs   : bigPlayer1, offer1 ;
   returns   :  ;

   inputs    : bigPlayer1, offer1;
   feedback  : ;
   operation : bigPlayer_unit "p3" 0 ;
   outputs   : bigPlayer2, offer2  ;
   returns   :  ;

   inputs    : bigPlayer2, offer2;
   feedback  : ;
   operation : bigPlayer_unit "p4" 1 ;
   outputs   : bigPlayer3, newOffer  ;
   returns   :  ;

   :----------------------------:

   outputs   : bigPlayer3, newOffer   ;
   returns   :  ;
   |]


-- Strategies: p1 offers -> p2 responds -> BP offers -> p3 responds. -> BP2 offers -> p4 responds 
strat_bigPlayer_4Players_eq = 
    Kleisli (varOffer_abs 2.49) ::- Kleisli  (varAccept_conditional 2.5 7.5) ::-
    Kleisli (varOffer_abs 2.49) ::- Kleisli  (varAccept_conditional 2.5 5) ::-
    Kleisli (varOffer_abs 2.49) ::- Kleisli  (varAccept 2.5 )  ::- Nil

-- *************** Evaluation ***************

evalOpenPie_BigPlayer_4Players strat = evaluate (bigPlayers_composed_4Players) strat (contextContPie 10)
isEquilibrium_BigPlayer_4Players strat = generateIsEq $ evalOpenPie_BigPlayer_4Players strat 


-- run: 

-- isEquilibrium_BigPlayer_4Players strat_bigPlayer_4Players_test
-- isEquilibrium_BigPlayer_3Players strat_bigPlayer_3Players_test