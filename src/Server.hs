module Server where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (fromGregorian, Day)
import GHC.Generics
import Servant

type LandsatAPI =
  "landsat"
  :> QueryParam "cloudCoverMin" Float
  :> QueryParam "cloudCoverMax" Float
  :> Get '[JSON] String

type UserAPI1 = "users" :> Get '[JSON] [User]

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registration_date :: Day
} deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

server1 :: Server UserAPI1
server1 = return users1

userAPI :: Proxy UserAPI1
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
application :: Application
application = serve userAPI server1
