{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKClusterAnnotation@.
module ObjC.MapKit.MKClusterAnnotation
  ( MKClusterAnnotation
  , IsMKClusterAnnotation(..)
  , init_
  , initWithMemberAnnotations
  , title
  , setTitle
  , subtitle
  , setSubtitle
  , memberAnnotations
  , initSelector
  , initWithMemberAnnotationsSelector
  , titleSelector
  , setTitleSelector
  , subtitleSelector
  , setSubtitleSelector
  , memberAnnotationsSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMKClusterAnnotation mkClusterAnnotation => mkClusterAnnotation -> IO (Id MKClusterAnnotation)
init_ mkClusterAnnotation  =
    sendMsg mkClusterAnnotation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithMemberAnnotations:@
initWithMemberAnnotations :: (IsMKClusterAnnotation mkClusterAnnotation, IsNSArray memberAnnotations) => mkClusterAnnotation -> memberAnnotations -> IO (Id MKClusterAnnotation)
initWithMemberAnnotations mkClusterAnnotation  memberAnnotations =
  withObjCPtr memberAnnotations $ \raw_memberAnnotations ->
      sendMsg mkClusterAnnotation (mkSelector "initWithMemberAnnotations:") (retPtr retVoid) [argPtr (castPtr raw_memberAnnotations :: Ptr ())] >>= ownedObject . castPtr

-- | @- title@
title :: IsMKClusterAnnotation mkClusterAnnotation => mkClusterAnnotation -> IO (Id NSString)
title mkClusterAnnotation  =
    sendMsg mkClusterAnnotation (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsMKClusterAnnotation mkClusterAnnotation, IsNSString value) => mkClusterAnnotation -> value -> IO ()
setTitle mkClusterAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mkClusterAnnotation (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subtitle@
subtitle :: IsMKClusterAnnotation mkClusterAnnotation => mkClusterAnnotation -> IO (Id NSString)
subtitle mkClusterAnnotation  =
    sendMsg mkClusterAnnotation (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubtitle:@
setSubtitle :: (IsMKClusterAnnotation mkClusterAnnotation, IsNSString value) => mkClusterAnnotation -> value -> IO ()
setSubtitle mkClusterAnnotation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mkClusterAnnotation (mkSelector "setSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- memberAnnotations@
memberAnnotations :: IsMKClusterAnnotation mkClusterAnnotation => mkClusterAnnotation -> IO (Id NSArray)
memberAnnotations mkClusterAnnotation  =
    sendMsg mkClusterAnnotation (mkSelector "memberAnnotations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMemberAnnotations:@
initWithMemberAnnotationsSelector :: Selector
initWithMemberAnnotationsSelector = mkSelector "initWithMemberAnnotations:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @memberAnnotations@
memberAnnotationsSelector :: Selector
memberAnnotationsSelector = mkSelector "memberAnnotations"

