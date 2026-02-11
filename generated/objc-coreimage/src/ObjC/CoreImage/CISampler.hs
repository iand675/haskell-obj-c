{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CISampler@.
module ObjC.CoreImage.CISampler
  ( CISampler
  , IsCISampler(..)
  , samplerWithImage
  , samplerWithImage_keysAndValues
  , samplerWithImage_options
  , initWithImage
  , initWithImage_keysAndValues
  , initWithImage_options
  , definition
  , samplerWithImageSelector
  , samplerWithImage_keysAndValuesSelector
  , samplerWithImage_optionsSelector
  , initWithImageSelector
  , initWithImage_keysAndValuesSelector
  , initWithImage_optionsSelector
  , definitionSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ samplerWithImage:@
samplerWithImage :: IsCIImage im => im -> IO (Id CISampler)
samplerWithImage im =
  do
    cls' <- getRequiredClass "CISampler"
    withObjCPtr im $ \raw_im ->
      sendClassMsg cls' (mkSelector "samplerWithImage:") (retPtr retVoid) [argPtr (castPtr raw_im :: Ptr ())] >>= retainedObject . castPtr

-- | @+ samplerWithImage:keysAndValues:@
samplerWithImage_keysAndValues :: IsCIImage im => im -> RawId -> IO (Id CISampler)
samplerWithImage_keysAndValues im key0 =
  do
    cls' <- getRequiredClass "CISampler"
    withObjCPtr im $ \raw_im ->
      sendClassMsg cls' (mkSelector "samplerWithImage:keysAndValues:") (retPtr retVoid) [argPtr (castPtr raw_im :: Ptr ()), argPtr (castPtr (unRawId key0) :: Ptr ())] >>= retainedObject . castPtr

-- | @+ samplerWithImage:options:@
samplerWithImage_options :: (IsCIImage im, IsNSDictionary dict) => im -> dict -> IO (Id CISampler)
samplerWithImage_options im dict =
  do
    cls' <- getRequiredClass "CISampler"
    withObjCPtr im $ \raw_im ->
      withObjCPtr dict $ \raw_dict ->
        sendClassMsg cls' (mkSelector "samplerWithImage:options:") (retPtr retVoid) [argPtr (castPtr raw_im :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithImage:@
initWithImage :: (IsCISampler ciSampler, IsCIImage im) => ciSampler -> im -> IO (Id CISampler)
initWithImage ciSampler  im =
withObjCPtr im $ \raw_im ->
    sendMsg ciSampler (mkSelector "initWithImage:") (retPtr retVoid) [argPtr (castPtr raw_im :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithImage:keysAndValues:@
initWithImage_keysAndValues :: (IsCISampler ciSampler, IsCIImage im) => ciSampler -> im -> RawId -> IO (Id CISampler)
initWithImage_keysAndValues ciSampler  im key0 =
withObjCPtr im $ \raw_im ->
    sendMsg ciSampler (mkSelector "initWithImage:keysAndValues:") (retPtr retVoid) [argPtr (castPtr raw_im :: Ptr ()), argPtr (castPtr (unRawId key0) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithImage:options:@
initWithImage_options :: (IsCISampler ciSampler, IsCIImage im, IsNSDictionary dict) => ciSampler -> im -> dict -> IO (Id CISampler)
initWithImage_options ciSampler  im dict =
withObjCPtr im $ \raw_im ->
  withObjCPtr dict $ \raw_dict ->
      sendMsg ciSampler (mkSelector "initWithImage:options:") (retPtr retVoid) [argPtr (castPtr raw_im :: Ptr ()), argPtr (castPtr raw_dict :: Ptr ())] >>= ownedObject . castPtr

-- | @- definition@
definition :: IsCISampler ciSampler => ciSampler -> IO (Id CIFilterShape)
definition ciSampler  =
  sendMsg ciSampler (mkSelector "definition") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @samplerWithImage:@
samplerWithImageSelector :: Selector
samplerWithImageSelector = mkSelector "samplerWithImage:"

-- | @Selector@ for @samplerWithImage:keysAndValues:@
samplerWithImage_keysAndValuesSelector :: Selector
samplerWithImage_keysAndValuesSelector = mkSelector "samplerWithImage:keysAndValues:"

-- | @Selector@ for @samplerWithImage:options:@
samplerWithImage_optionsSelector :: Selector
samplerWithImage_optionsSelector = mkSelector "samplerWithImage:options:"

-- | @Selector@ for @initWithImage:@
initWithImageSelector :: Selector
initWithImageSelector = mkSelector "initWithImage:"

-- | @Selector@ for @initWithImage:keysAndValues:@
initWithImage_keysAndValuesSelector :: Selector
initWithImage_keysAndValuesSelector = mkSelector "initWithImage:keysAndValues:"

-- | @Selector@ for @initWithImage:options:@
initWithImage_optionsSelector :: Selector
initWithImage_optionsSelector = mkSelector "initWithImage:options:"

-- | @Selector@ for @definition@
definitionSelector :: Selector
definitionSelector = mkSelector "definition"

