module Main where

import           Control.Concurrent
import           Control.Lens
import qualified Data.Binary           as B
import qualified Data.Binary.Get       as B
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Text             as T
import           Foundation
import qualified Prelude               (show)

import           Network.ABCI

data State = State {
  stateHashCount ∷ Integer,
  stateTxCount   ∷ Integer,
  stateSerial    ∷ Bool
} deriving (Show, Eq)

main ∷ IO ()
main = serve defaultHost defaultPort =<< counter False

handleInfo ∷ MVar State → RequestInfo → IO ResponseInfo
handleInfo state RequestInfo = do
  (State hashCount txCount _) ← readMVar state
  return ResponseInfo {
    _ResponseInfo'data'             = T.concat ["{\"hashes\":", T.pack (Prelude.show hashCount), ",\"txs\":", T.pack (Prelude.show txCount), "}"],
    _ResponseInfo'version           = "0.1.0",
    _ResponseInfo'lastBlockHeight   = 0,
    _ResponseInfo'lastBlockAppHash  = B.empty
  }

handleEcho ∷ RequestEcho → IO ResponseEcho
handleEcho (RequestEcho e) = return $ ResponseEcho e

handleFlush ∷ RequestFlush → IO ResponseFlush
handleFlush RequestFlush = return ResponseFlush

handleOption ∷ MVar State → RequestSetOption → IO ResponseSetOption
handleOption state (RequestSetOption key val) = do
  if key == "serial" && val == "on" then
    modifyMVar_ state (\s → return $ s { stateSerial = True })
  else return ()
  return $ ResponseSetOption ""

handleInitChain ∷ RequestInitChain → IO ResponseInitChain
handleInitChain _ = return ResponseInitChain

handleBeginBlock ∷ RequestBeginBlock → IO ResponseBeginBlock
handleBeginBlock _ = return ResponseBeginBlock

handleCheckTx ∷ MVar State → RequestCheckTx → IO ResponseCheckTx
handleCheckTx state (RequestCheckTx tx) =
  case B.runGetOrFail B.getInt64be $ BL.fromStrict $ B.append (B.replicate (8 - B.length tx) '\NUL') tx of
    Right (_, _, num) → do
      (State _ txCount serial) ← readMVar state
      if not serial || fromIntegral num >= txCount then
        return $ ResponseCheckTx OK B.empty T.empty
      else
        return $ ResponseCheckTx BadNonce B.empty invalidNumber
    Left _ → return $ ResponseCheckTx EncodingError B.empty undecodable

handleDeliverTx ∷ MVar State → RequestDeliverTx → IO ResponseDeliverTx
handleDeliverTx state (RequestDeliverTx tx) =
  case B.runGetOrFail B.getInt64be $ BL.fromStrict $ B.append (B.replicate (8 - B.length tx) '\NUL') tx of
    Right (_, _, num) →
      modifyMVar state $ \state@(State _ txCount serial) →
        if not serial || fromIntegral num == txCount then
          return (state { stateTxCount = txCount + 1 }, ResponseDeliverTx OK B.empty T.empty)
        else
          return (state, ResponseDeliverTx BadNonce B.empty invalidNumber)
    Left _ → return $ ResponseDeliverTx EncodingError B.empty undecodable

handleEndBlock ∷ RequestEndBlock → IO ResponseEndBlock
handleEndBlock _ = return $ ResponseEndBlock []

handleCommit ∷ MVar State → RequestCommit → IO ResponseCommit
handleCommit state _ = do
  count ← modifyMVar state $ \state@(State hashCount txCount _) → do
    let newCount = hashCount + 1
    return (state { stateHashCount = newCount }, fromIntegral txCount ∷ Int64)
  return $ ResponseCommit OK (if count == 0 then "" else BL.toStrict $ B.encode count) T.empty

handleQuery ∷ MVar State → RequestQuery → IO ResponseQuery
handleQuery state (RequestQuery _ path _ _) =
  case path of
    "hash"  → do
      (State hashCount _ _) ← readMVar state
      return $ ResponseQuery OK 0 "" (BL.toStrict $ B.encode hashCount) "" 0 ""
    "tx"    → do
      (State _ txCount _) ← readMVar state
      return $ ResponseQuery OK 0 "" (BL.toStrict $ B.encode txCount) "" 0 ""
    _       →  return $ ResponseQuery EncodingError 0 "" "" "" 0 $ T.append "Invalid query path. Expected hash or tx, got " path

counter ∷ Bool → IO (Request → IO Response)
counter serial = do
  state ← newMVar $ State 0 0 serial
  let respondWith func val = func .~ val $ emptyResponse
  return $ \req →
    handleCase _Request'info (fmap (respondWith info) . handleInfo state) req $
      handleCase _Request'echo (fmap (respondWith echo) . handleEcho) req $
        handleCase _Request'flush (fmap (respondWith flush) . handleFlush) req $
          handleCase _Request'setOption (fmap (respondWith setOption) . handleOption state) req $
            handleCase _Request'initChain (fmap (respondWith initChain) . handleInitChain) req $
              handleCase _Request'checkTx (fmap (respondWith checkTx) . handleCheckTx state) req $
                handleCase _Request'beginBlock (fmap (respondWith beginBlock) . handleBeginBlock) req $
                  handleCase _Request'deliverTx (fmap (respondWith deliverTx) . handleDeliverTx state) req $
                    handleCase _Request'endBlock (fmap (respondWith endBlock) . handleEndBlock) req $
                      handleCase _Request'commit (fmap (respondWith commit) . handleCommit state) req $
                        handleCase _Request'query (fmap (respondWith query) . handleQuery state) req $
                         return $ exception .~ ResponseException unknown $ emptyResponse

flushResponse ∷ Response
flushResponse = flush .~ ResponseFlush $ emptyResponse

handleCase ∷ ∀ a b c m . (a → Maybe b) → (b → m c) → a → m c → m c
handleCase getter func val cont =
  case getter val of
    Just x  → func x
    Nothing → cont

emptyResponse ∷ Response
emptyResponse = Response Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

unknown ∷ T.Text
unknown = "unknown_request"

invalidNumber ∷ T.Text
invalidNumber = "invalid_number"

undecodable ∷ T.Text
undecodable = "undecodable"
