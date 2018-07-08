module BigQuery where

{-# LANGUAGE OverloadedStrings #-}

import           LandsatModel

import           Control.Lens    ((^.), (^?))
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Text       (pack)
import           GHC.Generics    (Generic)

data JobReference = JobReference { projectId :: String
                                 , jobId     :: String } deriving (Eq, Show, Generic)

instance ToJSON JobReference
instance FromJSON JobReference

data QueryCreateResponse =
  QueryCreateResponse { id           :: String
                      , jobReference :: JobReference } deriving (Eq, Show, Generic)

data QueryDetailResponse =
  QueryDetailResponse { rows        :: [Value]
                      , schema      :: Value
                      , jobComplete :: Bool } deriving (Eq, Show, Generic)

instance FromJSON QueryDetailResponse

instance FromJSON QueryCreateResponse
