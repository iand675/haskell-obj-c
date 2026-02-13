{-# LANGUAGE DataKinds #-}
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
  , memberAnnotationsSelector
  , setSubtitleSelector
  , setTitleSelector
  , subtitleSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMKClusterAnnotation mkClusterAnnotation => mkClusterAnnotation -> IO (Id MKClusterAnnotation)
init_ mkClusterAnnotation =
  sendOwnedMessage mkClusterAnnotation initSelector

-- | @- initWithMemberAnnotations:@
initWithMemberAnnotations :: (IsMKClusterAnnotation mkClusterAnnotation, IsNSArray memberAnnotations) => mkClusterAnnotation -> memberAnnotations -> IO (Id MKClusterAnnotation)
initWithMemberAnnotations mkClusterAnnotation memberAnnotations =
  sendOwnedMessage mkClusterAnnotation initWithMemberAnnotationsSelector (toNSArray memberAnnotations)

-- | @- title@
title :: IsMKClusterAnnotation mkClusterAnnotation => mkClusterAnnotation -> IO (Id NSString)
title mkClusterAnnotation =
  sendMessage mkClusterAnnotation titleSelector

-- | @- setTitle:@
setTitle :: (IsMKClusterAnnotation mkClusterAnnotation, IsNSString value) => mkClusterAnnotation -> value -> IO ()
setTitle mkClusterAnnotation value =
  sendMessage mkClusterAnnotation setTitleSelector (toNSString value)

-- | @- subtitle@
subtitle :: IsMKClusterAnnotation mkClusterAnnotation => mkClusterAnnotation -> IO (Id NSString)
subtitle mkClusterAnnotation =
  sendMessage mkClusterAnnotation subtitleSelector

-- | @- setSubtitle:@
setSubtitle :: (IsMKClusterAnnotation mkClusterAnnotation, IsNSString value) => mkClusterAnnotation -> value -> IO ()
setSubtitle mkClusterAnnotation value =
  sendMessage mkClusterAnnotation setSubtitleSelector (toNSString value)

-- | @- memberAnnotations@
memberAnnotations :: IsMKClusterAnnotation mkClusterAnnotation => mkClusterAnnotation -> IO (Id NSArray)
memberAnnotations mkClusterAnnotation =
  sendMessage mkClusterAnnotation memberAnnotationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MKClusterAnnotation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithMemberAnnotations:@
initWithMemberAnnotationsSelector :: Selector '[Id NSArray] (Id MKClusterAnnotation)
initWithMemberAnnotationsSelector = mkSelector "initWithMemberAnnotations:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector '[Id NSString] ()
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @memberAnnotations@
memberAnnotationsSelector :: Selector '[] (Id NSArray)
memberAnnotationsSelector = mkSelector "memberAnnotations"

