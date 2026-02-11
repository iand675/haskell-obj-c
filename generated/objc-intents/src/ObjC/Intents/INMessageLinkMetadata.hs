{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INMessageLinkMetadata@.
module ObjC.Intents.INMessageLinkMetadata
  ( INMessageLinkMetadata
  , IsINMessageLinkMetadata(..)
  , initWithSiteName_summary_title_openGraphType_linkURL
  , siteName
  , setSiteName
  , summary
  , setSummary
  , title
  , setTitle
  , openGraphType
  , setOpenGraphType
  , linkURL
  , setLinkURL
  , initWithSiteName_summary_title_openGraphType_linkURLSelector
  , siteNameSelector
  , setSiteNameSelector
  , summarySelector
  , setSummarySelector
  , titleSelector
  , setTitleSelector
  , openGraphTypeSelector
  , setOpenGraphTypeSelector
  , linkURLSelector
  , setLinkURLSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSiteName:summary:title:openGraphType:linkURL:@
initWithSiteName_summary_title_openGraphType_linkURL :: (IsINMessageLinkMetadata inMessageLinkMetadata, IsNSString siteName, IsNSString summary, IsNSString title, IsNSString openGraphType, IsNSURL linkURL) => inMessageLinkMetadata -> siteName -> summary -> title -> openGraphType -> linkURL -> IO (Id INMessageLinkMetadata)
initWithSiteName_summary_title_openGraphType_linkURL inMessageLinkMetadata  siteName summary title openGraphType linkURL =
withObjCPtr siteName $ \raw_siteName ->
  withObjCPtr summary $ \raw_summary ->
    withObjCPtr title $ \raw_title ->
      withObjCPtr openGraphType $ \raw_openGraphType ->
        withObjCPtr linkURL $ \raw_linkURL ->
            sendMsg inMessageLinkMetadata (mkSelector "initWithSiteName:summary:title:openGraphType:linkURL:") (retPtr retVoid) [argPtr (castPtr raw_siteName :: Ptr ()), argPtr (castPtr raw_summary :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_openGraphType :: Ptr ()), argPtr (castPtr raw_linkURL :: Ptr ())] >>= ownedObject . castPtr

-- | @- siteName@
siteName :: IsINMessageLinkMetadata inMessageLinkMetadata => inMessageLinkMetadata -> IO (Id NSString)
siteName inMessageLinkMetadata  =
  sendMsg inMessageLinkMetadata (mkSelector "siteName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSiteName:@
setSiteName :: (IsINMessageLinkMetadata inMessageLinkMetadata, IsNSString value) => inMessageLinkMetadata -> value -> IO ()
setSiteName inMessageLinkMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg inMessageLinkMetadata (mkSelector "setSiteName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- summary@
summary :: IsINMessageLinkMetadata inMessageLinkMetadata => inMessageLinkMetadata -> IO (Id NSString)
summary inMessageLinkMetadata  =
  sendMsg inMessageLinkMetadata (mkSelector "summary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSummary:@
setSummary :: (IsINMessageLinkMetadata inMessageLinkMetadata, IsNSString value) => inMessageLinkMetadata -> value -> IO ()
setSummary inMessageLinkMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg inMessageLinkMetadata (mkSelector "setSummary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- title@
title :: IsINMessageLinkMetadata inMessageLinkMetadata => inMessageLinkMetadata -> IO (Id NSString)
title inMessageLinkMetadata  =
  sendMsg inMessageLinkMetadata (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsINMessageLinkMetadata inMessageLinkMetadata, IsNSString value) => inMessageLinkMetadata -> value -> IO ()
setTitle inMessageLinkMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg inMessageLinkMetadata (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- openGraphType@
openGraphType :: IsINMessageLinkMetadata inMessageLinkMetadata => inMessageLinkMetadata -> IO (Id NSString)
openGraphType inMessageLinkMetadata  =
  sendMsg inMessageLinkMetadata (mkSelector "openGraphType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOpenGraphType:@
setOpenGraphType :: (IsINMessageLinkMetadata inMessageLinkMetadata, IsNSString value) => inMessageLinkMetadata -> value -> IO ()
setOpenGraphType inMessageLinkMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg inMessageLinkMetadata (mkSelector "setOpenGraphType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- linkURL@
linkURL :: IsINMessageLinkMetadata inMessageLinkMetadata => inMessageLinkMetadata -> IO (Id NSURL)
linkURL inMessageLinkMetadata  =
  sendMsg inMessageLinkMetadata (mkSelector "linkURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLinkURL:@
setLinkURL :: (IsINMessageLinkMetadata inMessageLinkMetadata, IsNSURL value) => inMessageLinkMetadata -> value -> IO ()
setLinkURL inMessageLinkMetadata  value =
withObjCPtr value $ \raw_value ->
    sendMsg inMessageLinkMetadata (mkSelector "setLinkURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSiteName:summary:title:openGraphType:linkURL:@
initWithSiteName_summary_title_openGraphType_linkURLSelector :: Selector
initWithSiteName_summary_title_openGraphType_linkURLSelector = mkSelector "initWithSiteName:summary:title:openGraphType:linkURL:"

-- | @Selector@ for @siteName@
siteNameSelector :: Selector
siteNameSelector = mkSelector "siteName"

-- | @Selector@ for @setSiteName:@
setSiteNameSelector :: Selector
setSiteNameSelector = mkSelector "setSiteName:"

-- | @Selector@ for @summary@
summarySelector :: Selector
summarySelector = mkSelector "summary"

-- | @Selector@ for @setSummary:@
setSummarySelector :: Selector
setSummarySelector = mkSelector "setSummary:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @openGraphType@
openGraphTypeSelector :: Selector
openGraphTypeSelector = mkSelector "openGraphType"

-- | @Selector@ for @setOpenGraphType:@
setOpenGraphTypeSelector :: Selector
setOpenGraphTypeSelector = mkSelector "setOpenGraphType:"

-- | @Selector@ for @linkURL@
linkURLSelector :: Selector
linkURLSelector = mkSelector "linkURL"

-- | @Selector@ for @setLinkURL:@
setLinkURLSelector :: Selector
setLinkURLSelector = mkSelector "setLinkURL:"

