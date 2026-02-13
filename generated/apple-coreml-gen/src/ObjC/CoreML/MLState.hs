{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
getMultiArrayForStateNamed_handler mlState stateName handler =
  sendMessage mlState getMultiArrayForStateNamed_handlerSelector (toNSString stateName) handler

-- | @- init@
init_ :: IsMLState mlState => mlState -> IO (Id MLState)
init_ mlState =
  sendOwnedMessage mlState initSelector

-- | @+ new@
new :: IO (Id MLState)
new  =
  do
    cls' <- getRequiredClass "MLState"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getMultiArrayForStateNamed:handler:@
getMultiArrayForStateNamed_handlerSelector :: Selector '[Id NSString, Ptr ()] ()
getMultiArrayForStateNamed_handlerSelector = mkSelector "getMultiArrayForStateNamed:handler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLState)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLState)
newSelector = mkSelector "new"

