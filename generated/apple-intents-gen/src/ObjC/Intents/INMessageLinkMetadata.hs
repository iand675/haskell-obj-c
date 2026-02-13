{-# LANGUAGE DataKinds #-}
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
  , linkURLSelector
  , openGraphTypeSelector
  , setLinkURLSelector
  , setOpenGraphTypeSelector
  , setSiteNameSelector
  , setSummarySelector
  , setTitleSelector
  , siteNameSelector
  , summarySelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithSiteName:summary:title:openGraphType:linkURL:@
initWithSiteName_summary_title_openGraphType_linkURL :: (IsINMessageLinkMetadata inMessageLinkMetadata, IsNSString siteName, IsNSString summary, IsNSString title, IsNSString openGraphType, IsNSURL linkURL) => inMessageLinkMetadata -> siteName -> summary -> title -> openGraphType -> linkURL -> IO (Id INMessageLinkMetadata)
initWithSiteName_summary_title_openGraphType_linkURL inMessageLinkMetadata siteName summary title openGraphType linkURL =
  sendOwnedMessage inMessageLinkMetadata initWithSiteName_summary_title_openGraphType_linkURLSelector (toNSString siteName) (toNSString summary) (toNSString title) (toNSString openGraphType) (toNSURL linkURL)

-- | @- siteName@
siteName :: IsINMessageLinkMetadata inMessageLinkMetadata => inMessageLinkMetadata -> IO (Id NSString)
siteName inMessageLinkMetadata =
  sendMessage inMessageLinkMetadata siteNameSelector

-- | @- setSiteName:@
setSiteName :: (IsINMessageLinkMetadata inMessageLinkMetadata, IsNSString value) => inMessageLinkMetadata -> value -> IO ()
setSiteName inMessageLinkMetadata value =
  sendMessage inMessageLinkMetadata setSiteNameSelector (toNSString value)

-- | @- summary@
summary :: IsINMessageLinkMetadata inMessageLinkMetadata => inMessageLinkMetadata -> IO (Id NSString)
summary inMessageLinkMetadata =
  sendMessage inMessageLinkMetadata summarySelector

-- | @- setSummary:@
setSummary :: (IsINMessageLinkMetadata inMessageLinkMetadata, IsNSString value) => inMessageLinkMetadata -> value -> IO ()
setSummary inMessageLinkMetadata value =
  sendMessage inMessageLinkMetadata setSummarySelector (toNSString value)

-- | @- title@
title :: IsINMessageLinkMetadata inMessageLinkMetadata => inMessageLinkMetadata -> IO (Id NSString)
title inMessageLinkMetadata =
  sendMessage inMessageLinkMetadata titleSelector

-- | @- setTitle:@
setTitle :: (IsINMessageLinkMetadata inMessageLinkMetadata, IsNSString value) => inMessageLinkMetadata -> value -> IO ()
setTitle inMessageLinkMetadata value =
  sendMessage inMessageLinkMetadata setTitleSelector (toNSString value)

-- | @- openGraphType@
openGraphType :: IsINMessageLinkMetadata inMessageLinkMetadata => inMessageLinkMetadata -> IO (Id NSString)
openGraphType inMessageLinkMetadata =
  sendMessage inMessageLinkMetadata openGraphTypeSelector

-- | @- setOpenGraphType:@
setOpenGraphType :: (IsINMessageLinkMetadata inMessageLinkMetadata, IsNSString value) => inMessageLinkMetadata -> value -> IO ()
setOpenGraphType inMessageLinkMetadata value =
  sendMessage inMessageLinkMetadata setOpenGraphTypeSelector (toNSString value)

-- | @- linkURL@
linkURL :: IsINMessageLinkMetadata inMessageLinkMetadata => inMessageLinkMetadata -> IO (Id NSURL)
linkURL inMessageLinkMetadata =
  sendMessage inMessageLinkMetadata linkURLSelector

-- | @- setLinkURL:@
setLinkURL :: (IsINMessageLinkMetadata inMessageLinkMetadata, IsNSURL value) => inMessageLinkMetadata -> value -> IO ()
setLinkURL inMessageLinkMetadata value =
  sendMessage inMessageLinkMetadata setLinkURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSiteName:summary:title:openGraphType:linkURL:@
initWithSiteName_summary_title_openGraphType_linkURLSelector :: Selector '[Id NSString, Id NSString, Id NSString, Id NSString, Id NSURL] (Id INMessageLinkMetadata)
initWithSiteName_summary_title_openGraphType_linkURLSelector = mkSelector "initWithSiteName:summary:title:openGraphType:linkURL:"

-- | @Selector@ for @siteName@
siteNameSelector :: Selector '[] (Id NSString)
siteNameSelector = mkSelector "siteName"

-- | @Selector@ for @setSiteName:@
setSiteNameSelector :: Selector '[Id NSString] ()
setSiteNameSelector = mkSelector "setSiteName:"

-- | @Selector@ for @summary@
summarySelector :: Selector '[] (Id NSString)
summarySelector = mkSelector "summary"

-- | @Selector@ for @setSummary:@
setSummarySelector :: Selector '[Id NSString] ()
setSummarySelector = mkSelector "setSummary:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @openGraphType@
openGraphTypeSelector :: Selector '[] (Id NSString)
openGraphTypeSelector = mkSelector "openGraphType"

-- | @Selector@ for @setOpenGraphType:@
setOpenGraphTypeSelector :: Selector '[Id NSString] ()
setOpenGraphTypeSelector = mkSelector "setOpenGraphType:"

-- | @Selector@ for @linkURL@
linkURLSelector :: Selector '[] (Id NSURL)
linkURLSelector = mkSelector "linkURL"

-- | @Selector@ for @setLinkURL:@
setLinkURLSelector :: Selector '[Id NSURL] ()
setLinkURLSelector = mkSelector "setLinkURL:"

