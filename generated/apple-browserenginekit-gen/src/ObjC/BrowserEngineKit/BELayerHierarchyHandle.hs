{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @BELayerHierarchyHandle@.
module ObjC.BrowserEngineKit.BELayerHierarchyHandle
  ( BELayerHierarchyHandle
  , IsBELayerHierarchyHandle(..)
  , init_
  , new
  , handleWithPort_data_error
  , encodeWithBlock
  , encodeWithBlockSelector
  , handleWithPort_data_errorSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.BrowserEngineKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsBELayerHierarchyHandle beLayerHierarchyHandle => beLayerHierarchyHandle -> IO (Id BELayerHierarchyHandle)
init_ beLayerHierarchyHandle =
  sendOwnedMessage beLayerHierarchyHandle initSelector

-- | @+ new@
new :: IO (Id BELayerHierarchyHandle)
new  =
  do
    cls' <- getRequiredClass "BELayerHierarchyHandle"
    sendOwnedClassMessage cls' newSelector

-- | Decodes a handle form a @mach_port_t@ send right and its accompanying metadata. - This method takes ownership of the port right (even if it returns an error).
--
-- ObjC selector: @+ handleWithPort:data:error:@
handleWithPort_data_error :: (IsNSData data_, IsNSError error_) => CUInt -> data_ -> error_ -> IO (Id BELayerHierarchyHandle)
handleWithPort_data_error port data_ error_ =
  do
    cls' <- getRequiredClass "BELayerHierarchyHandle"
    sendClassMessage cls' handleWithPort_data_errorSelector port (toNSData data_) (toNSError error_)

-- | Encodes the handle into a @mach_port_t@ send right and its accompanying metadata. - The block is responsible for disposing of @copiedPort@ - failure to manage its lifecycle will leak the port. Note that some functions (like ``handleWithPort:data:error:``) will assume control of the right for you. - @copiedPort@ will be @MACH_PORT_NULL@ if the ``BELayerHierarchy`` pointed to by the handle is already invalidated. - The port and data should ultimately be consumed together  by ``handleWithPort:data:error:``.
--
-- ObjC selector: @- encodeWithBlock:@
encodeWithBlock :: IsBELayerHierarchyHandle beLayerHierarchyHandle => beLayerHierarchyHandle -> Ptr () -> IO ()
encodeWithBlock beLayerHierarchyHandle block =
  sendMessage beLayerHierarchyHandle encodeWithBlockSelector block

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id BELayerHierarchyHandle)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id BELayerHierarchyHandle)
newSelector = mkSelector "new"

-- | @Selector@ for @handleWithPort:data:error:@
handleWithPort_data_errorSelector :: Selector '[CUInt, Id NSData, Id NSError] (Id BELayerHierarchyHandle)
handleWithPort_data_errorSelector = mkSelector "handleWithPort:data:error:"

-- | @Selector@ for @encodeWithBlock:@
encodeWithBlockSelector :: Selector '[Ptr ()] ()
encodeWithBlockSelector = mkSelector "encodeWithBlock:"

