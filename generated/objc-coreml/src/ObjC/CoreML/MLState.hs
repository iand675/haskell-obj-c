{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Handle to the state buffers.
--
-- A stateful model maintains a state from one prediction to another by storing the information in the state buffers. To use such a model, the client must request the model to create state buffers and get @MLState@ object, which is the handle to those buffers. Then, at the prediction time, pass the @MLState@ object in one of the stateful prediction functions.
--
-- ```swift // Load a stateful model let modelAsset = try MLModelAsset(url: modelURL) let model = try await MLModel.load(asset: modelAsset, configuration: MLModelConfiguration())
--
-- // Request a state let state = model.newState()
--
-- // Run predictions for _ in 0 ..< 42 {   _ = try await model.prediction(from: inputFeatures, using: state) }
--
-- // Access the state buffer. state.withMultiArray(for: "accumulator") { stateMultiArray in   ... } ```
--
-- The object is a handle to the state buffers. The client shall not read or write the buffers while a prediction is in-flight.
--
-- Each stateful prediction that uses the same @MLState@ must be serialized. Otherwise, if two such predictions run concurrently, the behavior is undefined.
--
-- Generated bindings for @MLState@.
module ObjC.CoreML.MLState
  ( MLState
  , IsMLState(..)
  , getMultiArrayForStateNamed_handler
  , init_
  , new
  , getMultiArrayForStateNamed_handlerSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Gets a mutable view into a state buffer.
--
-- The underlying state buffer's address can differ for each call; one shall not access the state buffer outside of the closure.
--
-- - Parameters:   - handler: Block to access the state buffer through @MLMultiArray@.
--
-- ObjC selector: @- getMultiArrayForStateNamed:handler:@
getMultiArrayForStateNamed_handler :: (IsMLState mlState, IsNSString stateName) => mlState -> stateName -> Ptr () -> IO ()
getMultiArrayForStateNamed_handler mlState  stateName handler =
withObjCPtr stateName $ \raw_stateName ->
    sendMsg mlState (mkSelector "getMultiArrayForStateNamed:handler:") retVoid [argPtr (castPtr raw_stateName :: Ptr ()), argPtr (castPtr handler :: Ptr ())]

-- | @- init@
init_ :: IsMLState mlState => mlState -> IO (Id MLState)
init_ mlState  =
  sendMsg mlState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLState)
new  =
  do
    cls' <- getRequiredClass "MLState"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getMultiArrayForStateNamed:handler:@
getMultiArrayForStateNamed_handlerSelector :: Selector
getMultiArrayForStateNamed_handlerSelector = mkSelector "getMultiArrayForStateNamed:handler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

