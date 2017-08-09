module Network.ABCI.Server where

import qualified Conduit                 as C
import           Control.Monad
import qualified Data.Binary             as B
import qualified Data.Bits               as B
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Conduit.Network    as N
import           Data.List               (unfoldr)
import qualified Data.ProtoLens.Encoding as P
import           Foundation
import qualified Prelude

import           Proto.Types

defaultHost ∷ Prelude.String
defaultHost = "*"

defaultPort ∷ Int
defaultPort = 46658

serve ∷ Prelude.String → Int → (Request → IO Response) → IO ()
serve host port func =
  N.runTCPServer (N.serverSettings port (fromString host)) $ \app →
    void $ (N.appSource app C.=$= unbuffer C.=$= handle func) C.$$+ N.appSink app

unbuffer ∷ C.ConduitM B.ByteString B.ByteString IO ()
unbuffer = go B.empty
  where go buffer = do
          val ← C.await
          case val of
            Just v  → do
              let joined = buffer `B.append` v
                  loop str =
                    if B.length str == 0 then go str else do
                      let (enc, length) = decodeLength str
                          remaining     = B.drop enc str
                      if B.length remaining >= length then do
                        let msg = B.take length remaining
                        C.yield msg
                        loop (B.drop length remaining)
                      else go str
              loop joined
            Nothing → return ()

handle ∷ (Request → IO Response) → C.ConduitM B.ByteString B.ByteString IO ()
handle func = go
  where go = do
          val ← C.await
          case val of
            Just v →
              case P.decodeMessage v of
                Right m → do
                  r ← C.liftIO $ func m
                  let encoded       = P.encodeMessage r
                      encodedLength = fromIntegral $ B.length encoded
                      lengthEncoded = B.pack $ unroll encodedLength
                      lengthLength  ∷ Word8
                      lengthLength  = fromIntegral $ B.length lengthEncoded
                      concatenated  = lengthLength `B.cons` lengthEncoded `B.append` encoded
                  C.yield concatenated
                  go
                Left _ → return ()
            Nothing → return ()

decodeLength ∷ B.ByteString → (Int, Int)
decodeLength str =
  let lengthSizeEncoded = BL.fromStrict $ B.take 1 str
      lengthSizeDecoded ∷ Word8
      lengthSizeDecoded = B.decode lengthSizeEncoded
      lengthSize        ∷ Int
      lengthSize        = fromIntegral lengthSizeDecoded
      lengthEncoded     = B.take lengthSize $ B.drop 1 str
      lengthDecoded     ∷ Integer
      lengthDecoded     = roll $ B.unpack lengthEncoded
  in (1 + lengthSize, fromIntegral lengthDecoded)

roll ∷ [Word8] → Integer
roll = foldr unstep 0 . reverse
  where
    unstep b a = a `B.shiftL` 8 B..|. fromIntegral b

unroll ∷ Integer → [Word8]
unroll = reverse . unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `B.shiftR` 8)
