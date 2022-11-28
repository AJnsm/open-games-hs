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

openPieSharingGamePayoff ::  Offer -> ResponderAction -> Payoff
openPieSharingGamePayoff newOffer nextResponse =
    if nextResponse == Accept then (snd newOffer) else (fst newOffer)

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


offerNewSlice_dependent payoffFactor = [opengame|
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
   returns   : payoffFactor * (openPieSharingGamePayoff (newOfferedPiece, chosenPiece- newOfferedPiece) newResponse) ;

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

-- Might need to be changed to: [0, 0.1000000000001 .. 1] to make sure cuts cannot be exactly half. 



decideBigPlayer :: Offer -> ResponderAction -> (String, Double)
decideBigPlayer offer response = if (pieceP1 > pieceP2) then ("p1", pieceP1) else ("p2", pieceP2)
    where
        pieceP1 = if response==Accept then (snd offer) else (fst offer)
        pieceP2 = if response==Accept then (fst offer) else (snd offer)

decideSmallPlayer :: Offer -> ResponderAction -> (String, Double)
decideSmallPlayer offer response = if (pieceP1 <= pieceP2) then ("p1", pieceP1) else ("p2", pieceP2)
    where
        pieceP1 = if response==Accept then (snd offer) else (fst offer)
        pieceP2 = if response==Accept then (fst offer) else (snd offer)


-- ******** unit of 2 players with the BigPlayer rule *********


bigPlayers_unit player2Name = [opengame|

   inputs    : inputBigPlayer, inputOffer  ;
   feedback  : responseP1_backwards;

   :----------------------------:

   //Player 1 responds
   inputs    : inputBigPlayer, inputOffer  ;
   feedback  : responseP1_backwards;
   operation : respondToOffer_dependent ;
   outputs   : responseP1   ;
   returns   :  ;

   //Player 1 offers, but gets no payoff yet!
   inputs    : inputBigPlayer, inputOffer, responseP1  ;
   feedback  :  ;
   operation : offerNewSlice_dependent 0;
   outputs   : offerP1   ;
   returns   : responseP2_backwards ;

   //Player 2 responds
   inputs    : player2Name, offerP1   ;
   feedback  : responseP2_backwards;
   operation : respondToOffer_dependent ;
   outputs   : responseP2   ;
   returns   :  ;

   //Find the smallest player and give them their payoff
   inputs    : offerP1, responseP2  ;
   feedback  :  ;
   operation : forwardFunction $ uncurry decideSmallPlayer ;
   outputs   : (smallPlayerName, smallestPiece)   ;
   returns   :  ;

   inputs    : smallPlayerName, smallestPiece ;
   feedback  :  ;
   operation : addRolePayoffs;
   outputs   :  ;
   returns   :  ;

   //Find the biggest player and propagate their ID and piece to the context
   inputs    : offerP1, responseP2  ;
   feedback  :  ;
   operation : forwardFunction $ uncurry decideBigPlayer ;
   outputs   : (bigPlayerName, biggestPiece)   ;
   returns   :  ;

   inputs    : bigPlayerName, (biggestPiece, 0), Accept  ;
   feedback  :  ;
   operation : offerNewSlice_dependent 0 ;
   outputs   : newOffer   ;
   returns   : newResponse ;
   
   :----------------------------:

   outputs   : bigPlayerName, newOffer  ;
   returns   : newResponse ;
   |]



-- -- ********* Composition **********

bigPlayers_composed = [opengame|

   inputs    : inputBigPlayer, inputOffer ;
   feedback  : responseP1_backwards;

   :----------------------------:

   inputs    : inputBigPlayer, inputOffer ;
   feedback  : responseP1_backwards;
   operation : bigPlayers_unit "p2";
   outputs   : bigPlayer1, offer1 ;
   returns   : response2 ;

   inputs    : bigPlayer1, offer1;
   feedback  : response2;
   operation : bigPlayers_unit "p3"  ;
   outputs   : bigPlayer2, newOffer  ;
   returns   : newResponse ;
   
   :----------------------------:

   outputs   : bigPlayer2, newOffer   ;
   returns   : newResponse ;
   |]




bigPlayers_unit_V2 player2Name payoffBP = [opengame|

   inputs    : inputBigPlayer, inputOffer  ;
   feedback  : ;

   :----------------------------:

   //Player 1 offers, but gets no payoff yet!
   inputs    : inputBigPlayer, inputOffer, Accept  ;
   feedback  :  ;
   operation : offerNewSlice_dependent 0;
   outputs   : offerP1   ;
   returns   : responseP2_backwards ;

   //Player 2 responds
   inputs    : player2Name, offerP1   ;
   feedback  : responseP2_backwards;
   operation : respondToOffer_dependent ;
   outputs   : responseP2   ;
   returns   :  ;

   //Find the smallest player and give them their payoff
   inputs    : offerP1, responseP2  ;
   feedback  :  ;
   operation : forwardFunction $ uncurry decideSmallPlayer ;
   outputs   : (smallPlayerName, smallestPiece)   ;
   returns   :  ;

   inputs    : smallPlayerName, smallestPiece ;
   feedback  :  ;
   operation : addRolePayoffs;
   outputs   :  ;
   returns   :  ;

   //Find the biggest player and propagate their ID and piece to the context
   inputs    : offerP1, responseP2  ;
   feedback  :  ;
   operation : forwardFunction $ uncurry decideBigPlayer ;
   outputs   : (bigPlayerName, biggestPiece)   ;
   returns   :  ;
   
   inputs    : bigPlayerName, biggestPiece * payoffBP ;
   feedback  :  ;
   operation : addRolePayoffs;
   outputs   :  ;
   returns   :  ;


   :----------------------------:

   outputs   : bigPlayerName, fromOfferedPieceToOffer biggestPiece biggestPiece ;
   returns   :  ;
   |]


bigPlayers_composed_V2 = [opengame|

   inputs    : inputBigPlayer, inputOffer ;
   feedback  : ;

   :----------------------------:

   inputs    : inputBigPlayer, inputOffer ;
   feedback  : ;
   operation : bigPlayers_unit_V2 "p2" 0 ;
   outputs   : bigPlayer1, offer1 ;
   returns   :  ;

   inputs    : bigPlayer1, offer1;
   feedback  : ;
   operation : bigPlayers_unit_V2 "p3" 1 ;
   outputs   : bigPlayer2, newOffer  ;
   returns   :  ;
   
   :----------------------------:

   outputs   : bigPlayer2, newOffer   ;
   returns   :  ;
   |]


-- *************** Context ***************

contextContPie :: Double -> StochasticStatefulContext
          (String, Offer)
          ResponderAction
          (String, Offer)
          ResponderAction

contextContPie fullPieSize = StochasticStatefulContext (pure ((),("p0", (fullPieSize, 0)))) (\_ _ -> pure Accept)


contextContPie_V2 :: Double -> StochasticStatefulContext
          (String, Offer)
          ()
          (String, Offer)
          ()

contextContPie_V2 fullPieSize = StochasticStatefulContext (pure ((),("p1", (fullPieSize, 0)))) (\_ _-> pure ())





-- *************** Strategies ***************


pureAccept :: Offer -> Stochastic ResponderAction
pureAccept x = playDeterministically Accept

pureReject :: Offer -> Stochastic ResponderAction
pureReject x = playDeterministically Reject

halfAccept :: Offer -> Stochastic ResponderAction
halfAccept x = if (fst x) >= (snd x) then playDeterministically Accept else playDeterministically Reject


smallOffer :: Double -> Stochastic Double
smallOffer x = playDeterministically $ 0.2*x

bigOffer :: Double -> Stochastic Double
bigOffer x = playDeterministically $ 0.8*x

halfOffer :: Double -> Stochastic Double
halfOffer x = playDeterministically $ 0.5*x

zeroOffer :: Double -> Stochastic Double
zeroOffer x = playDeterministically $ 0

varOffer :: Double -> Double -> Stochastic Double
varOffer r x = playDeterministically $ r*x

strat_bigPlayer_test = Kleisli halfAccept ::- Kleisli smallOffer ::- Kleisli halfAccept ::- Kleisli halfOffer ::- Nil

strat_bigPlayer_eq = Kleisli halfAccept ::- Kleisli halfOffer ::- Kleisli halfAccept ::- Kleisli zeroOffer ::- Nil


strat_bigPlayer_2Games = Kleisli halfAccept ::- Kleisli halfOffer ::- Kleisli halfAccept ::- Kleisli halfOffer ::- Kleisli halfAccept ::- Kleisli halfOffer ::- Kleisli halfAccept ::- Kleisli zeroOffer ::- Nil


-- Strategies: p1 offers -> p2 responds -> BP offers -> p2 responds
strat_bigPlayer_2Games_V2 = Kleisli smallOffer ::- Kleisli halfAccept ::- Kleisli smallOffer ::- Kleisli pureAccept ::- Nil

strat_bigPlayer_2Games_V2_test = Kleisli (varOffer 1) ::- Kleisli halfAccept ::- Kleisli (varOffer 0.1) ::- Kleisli halfAccept ::- Nil


-- *************** Evaluation ***************

evalOpenPie_BigPlayer strat = evaluate (bigPlayers_unit "p2") strat (contextContPie 10)
isEquilibrium_BigPlayer strat = generateIsEq $ evalOpenPie_BigPlayer strat 





evalOpenPie_BigPlayer_2Games strat = evaluate (bigPlayers_composed) strat (contextContPie 10)
isEquilibrium_BigPlayer_2Games strat = generateIsEq $ evalOpenPie_BigPlayer_2Games strat 





-- Something is wrong: payoffs do not make sense. 

evalOpenPie_BigPlayer_2Games_V2 strat = evaluate (bigPlayers_composed_V2) strat (contextContPie_V2 10)
isEquilibrium_BigPlayer_2Games_V2 strat = generateIsEq $ evalOpenPie_BigPlayer_2Games_V2 strat 

