-- :set -XDeriveGeneric -XGADTs -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XDeriveAnyClass
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Lens
import Data.Int
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite
import Database.SQLite.Simple

-- Part 1

-- define a table
data UserT f
  = User
  { _userEmail :: Columnar f Text,
    _userFirstName :: Columnar f Text,
    _userLastName :: Columnar f Text,
    _userPassword :: Columnar f Text
  }
  deriving (Generic)

-- Add some handy type synonyms
type User = UserT Identity

type UserId = PrimaryKey UserT Identity

-- :set -XStandaloneDeriving -XTypeSynonymInstances -XMultiParamTypeClasses
{--# LANGUAGE StandaloneDeriving #-}
{--# LANGUAGE TypeSynonymInstances #-}
{--# LANGUAGE MultiParamTypeClasses #-}

deriving instance Show User

deriving instance Eq User

-- doesn't work -> needs type annotations
-- User "john@example.com" "John" "Smith" "password!"

-- work
user :: User
user = User "john@example.com" "John" "Smith" "password!"

-- or with type annotation
-- user = User @Identity "john@example.com" "John" "Smith" "password!"

-- let beam know about the table
instance Beamable UserT

-- let it know about the primary key
instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = UserId . _userEmail

-- example of primary key
userKey = UserId "john@doe.org"

-- define the database, overriden in part 2
-- be is the backend, e.g., Sqlite, Postgres, etc.
-- data ShoppingCartDb f = ShoppingCartDb
--   {_shoppingCartUsers :: f (TableEntity UserT)}
--   deriving (Generic, Database be)

-- beam will derive the field names from the record field names
-- shoppingCartDb :: DatabaseSettings be ShoppingCartDb
-- shoppingCartDb = defaultDbSettings

-- create the db

-- $ sqlite3 shoppingcart1.db
-- SQLite version 3.14.0 2016-07-26 15:17:14
-- Enter ".help" for usage hints.
-- sqlite> CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));
-- sqlite>

part1 :: IO ()
part1 = do
  -- open the db
  -- import Database.SQLite.Simple
  conn <- open "shoppingcart1.db"

  -- insert some data
  runBeamSqliteDebug putStrLn {- for debug output -} conn $
    runInsert $
      insert (_shoppingCartUsers shoppingCartDb) $
        insertValues
          [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c" {- james -},
            User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -},
            User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
          ]

  -- query some data
  let allUsers = all_ (_shoppingCartUsers shoppingCartDb)

  runBeamSqliteDebug putStrLn conn $ do
    users <- runSelectReturningList $ select allUsers
    mapM_ (liftIO . putStrLn . show) users

  -- use order by
  let sortUsersByFirstName =
        orderBy_
          (\u -> (asc_ (_userFirstName u), desc_ (_userLastName u)))
          (all_ (_shoppingCartUsers shoppingCartDb))

  runBeamSqliteDebug putStrLn conn $ do
    users <- runSelectReturningList $ select sortUsersByFirstName
    mapM_ (liftIO . putStrLn . show) users

  -- use limit_ and offset_
  let boundedQuery =
        limit_ 1 $
          offset_ 1 $
            orderBy_ (asc_ . _userFirstName) $
              all_ (_shoppingCartUsers shoppingCartDb)

  runBeamSqliteDebug putStrLn conn $ do
    users <- runSelectReturningList (select boundedQuery)
    mapM_ (liftIO . putStrLn . show) users

  -- agregation example
  let userCount =
        aggregate_
          (\u -> as_ @Int32 countAll_) -- needs import Data.Int
          (all_ (_shoppingCartUsers shoppingCartDb))

  runBeamSqliteDebug putStrLn conn $ do
    Just c <- runSelectReturningOne $ select userCount
    liftIO $ putStrLn ("We have " ++ show c ++ " users in the database")

  -- more interresting aggregation example
  runBeamSqliteDebug putStrLn conn $
    runInsert $
      insert (_shoppingCartUsers shoppingCartDb) $
        insertValues
          [ User "james@pallo.com" "James" "Pallo" "b4cc344d25a2efe540adbf2678e2304c" {- james -},
            User "betty@sims.com" "Betty" "Sims" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -},
            User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c" {- james -},
            User "sam@sophitz.com" "Sam" "Sophitz" "332532dcfaa1cbf61e2a266bd723612c" {- sam -},
            User "sam@jely.com" "Sam" "Jely" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
          ]

  let numberOfUsersByName =
        aggregate_ (\u -> (group_ (_userFirstName u), as_ @Int32 countAll_)) $
          all_ (_shoppingCartUsers shoppingCartDb)

  runBeamSqliteDebug putStrLn conn $ do
    countedByName <- runSelectReturningList $ select numberOfUsersByName
    mapM_ (liftIO . putStrLn . show) countedByName

-- Part 2

-- add a related table
data AddressT f = Address
  { _addressId :: C f Int32,
    _addressLine1 :: C f Text,
    _addressLine2 :: C f (Maybe Text),
    _addressCity :: C f Text,
    _addressState :: C f Text,
    _addressZip :: C f Text,
    _addressForUser :: PrimaryKey UserT f
  }
  deriving (Generic, Beamable)

type Address = AddressT Identity

deriving instance Show (PrimaryKey UserT Identity)

deriving instance Show Address

instance Table AddressT where
  data PrimaryKey AddressT f = AddressId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = AddressId . _addressId

type AddressId = PrimaryKey AddressT Identity -- For convenience

-- The new shopping cart db with two tables
-- overriden in part 3
-- data ShoppingCartDb f = ShoppingCartDb
--   { _shoppingCartUsers :: f (TableEntity UserT),
--     _shoppingCartUserAddresses :: f (TableEntity AddressT)
--   }
--   deriving (Generic, Database be)
--
-- Rename some fields
-- shoppingCartDb :: DatabaseSettings be ShoppingCartDb
-- shoppingCartDb =
--   defaultDbSettings
--     `withDbModification` dbModification
--       { _shoppingCartUserAddresses =
--           setEntityName "addresses" -- rename table
--             <> modifyTableFields
--               tableModification
--                 { _addressLine1 = fieldNamed "address1",
--                   _addressLine2 = fieldNamed "address2"
--                 }
--       }

-- create lenses for fields with tableLens and bring them into scope
--
-- :set -XImpredicativeTypes -XNoMonomorphismRestriction
--
-- for some reason it needs type annotations

addressId :: Lens' (AddressT f) (Columnar f Int32)
Address (LensFor addressId) _ _ _ _ _ _ = tableLenses

addressLine1 :: Lens' (AddressT f) (Columnar f Text)
Address _ (LensFor addressLine1) _ _ _ _ _ = tableLenses

addressLine2 :: Lens' (AddressT f) (Columnar f (Maybe Text))
Address _ _ (LensFor addressLine2) _ _ _ _ = tableLenses

addressCity :: Lens' (AddressT f) (Columnar f Text)
Address _ _ _ (LensFor addressCity) _ _ _ = tableLenses

addressState :: Lens' (AddressT f) (Columnar f Text)
Address _ _ _ _ (LensFor addressState) _ _ = tableLenses

addressZip :: Lens' (AddressT f) (Columnar f Text)
Address _ _ _ _ _ (LensFor addressZip) _ = tableLenses

addressForUserId :: Lens' (AddressT f) (Columnar f Text)
Address _ _ _ _ _ _ (UserId (LensFor addressForUserId)) = tableLenses

userEmail :: Lens' (UserT f) (Columnar f Text)
User (LensFor userEmail) _ _ _ = tableLenses

userFirstName :: Lens' (UserT f) (Columnar f Text)
User _ (LensFor userFirstName) _ _ = tableLenses

userLastName :: Lens' (UserT f) (Columnar f Text)
User _ _ (LensFor userLastName) _ = tableLenses

userPassword :: Lens' (UserT f) (Columnar f Text)
User _ _ _ (LensFor userPassword) = tableLenses

-- and this one doesn't

-- overriden in part 3
-- ShoppingCartDb
--   (TableLens shoppingCartUsers)
--   (TableLens shoppingCartUserAddresses) =
--     dbLenses

-- working with relations

-- $ sqlite3 shoppingcart2.db
-- SQLite version 3.14.0 2016-07-26 15:17:14
-- Enter ".help" for usage hints.
-- sqlite> CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));
-- sqlite> CREATE TABLE addresses ( id INTEGER PRIMARY KEY, address1 VARCHAR NOT NULL, address2 VARCHAR, city VARCHAR NOT NULL, state VARCHAR NOT NULL, zip VARCHAR NOT NULL, for_user__email VARCHAR NOT NULL );

part2 :: IO ()
part2 = do
  -- open the db
  conn <- open "shoppingcart2.db"

  -- insert some users
  -- :set -XOverloadedStrings
  let james = User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c"
      betty = User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
      sam = User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c"

  runBeamSqliteDebug putStrLn conn $
    runInsert $
      insert (_shoppingCartUsers shoppingCartDb) $
        insertValues [james, betty, sam] -- here values are fully defined

  -- insert some addresses
  let addresses =
        [ Address
            default_ -- auto-incrementing id
            (val_ "123 Little Street")
            (val_ Nothing)
            (val_ "Boston")
            (val_ "MA")
            (val_ "12345")
            (pk james), -- primary key of james
          Address
            default_
            (val_ "222 Main Street")
            (val_ (Just "Ste 1"))
            (val_ "Houston")
            (val_ "TX")
            (val_ "8888")
            (pk betty),
          Address
            default_
            (val_ "9999 Residence Ave")
            (val_ Nothing)
            (val_ "Sugarland")
            (val_ "TX")
            (val_ "8989")
            (pk betty)
        ]

  runBeamSqliteDebug putStrLn conn $
    runInsert $
      insert (_shoppingCartUserAddresses shoppingCartDb) $
        insertExpressions addresses -- here the primary key needs to be auto-incremented

  -- get all the addresses
  addresses <-
    runBeamSqliteDebug putStrLn conn $
      runSelectReturningList $
        select (all_ (shoppingCartDb ^. shoppingCartUserAddresses))
  mapM_ print addresses

  -- demo of the query monad
  allPairs <- runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $
      select $ do
        user <- all_ (shoppingCartDb ^. shoppingCartUsers)
        address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
        return (user, address)

  mapM_ print allPairs

  -- with guards
  usersAndRelatedAddresses <-
    runBeamSqliteDebug putStrLn conn $
      runSelectReturningList $
        select $ do
          user <- all_ (shoppingCartDb ^. shoppingCartUsers)
          -- But could be
          -- user <- all_ (_shoppingCartUsers shoppingCartDb)
          address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
          guard_ (address ^. addressForUserId ==. user ^. userEmail)
          pure (user, address)

  mapM_ print usersAndRelatedAddresses

  -- now with references, better for joins with primary/foreign keys
  usersAndRelatedAddressesUsingReferences <-
    runBeamSqliteDebug putStrLn conn $
      runSelectReturningList $
        select $ do
          user <- all_ (shoppingCartDb ^. shoppingCartUsers)
          address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
          guard_ (_addressForUser address `references_` user)
          pure (user, address)

  mapM_ print usersAndRelatedAddressesUsingReferences

  -- even better with related_
  -- will introduce on in the query
  usersAndRelatedAddressesUsingRelated <-
    runBeamSqliteDebug putStrLn conn $
      runSelectReturningList $
        select $ do
          address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
          user <-
            -- related_ is inner join on foreign key
            related_
              (shoppingCartDb ^. shoppingCartUsers)
              (_addressForUser address)
          pure (user, address)

  mapM_ print usersAndRelatedAddressesUsingRelated

  -- get address from a user id
  let bettyId = UserId "betty@example.com" :: UserId

  bettysAddresses <-
    runBeamSqliteDebug putStrLn conn $
      runSelectReturningList $
        select $ do
          address <- all_ (shoppingCartDb ^. shoppingCartUserAddresses)
          guard_ (_addressForUser address ==. val_ bettyId)
          pure address

  mapM_ print bettysAddresses

  -- Updates

  -- Update with a full record

  -- for some reason cannot use james directly here
  let jamesUser :: User = james
  [james] <- runBeamSqliteDebug putStrLn conn $ do
    runUpdate $
      save
        (shoppingCartDb ^. shoppingCartUsers)
        (jamesUser {_userPassword = "52a516ca6df436828d9c0d26e31ef704"})

    runSelectReturningList $
      lookup_ (shoppingCartDb ^. shoppingCartUsers) (UserId "james@example.com")

  putStrLn ("James's new password is " ++ show (james ^. userPassword))

  -- Update with a partial record

  addresses <- runBeamSqliteDebug putStrLn conn $ do
    runUpdate $
      update
        (shoppingCartDb ^. shoppingCartUserAddresses)
        -- update clause
        ( \address ->
            mconcat
              -- I dont like this operator <-.
              [ address ^. addressCity <-. val_ "Sugarville",
                address ^. addressZip <-. val_ "12345"
              ]
        )
        -- where clause
        ( \address ->
            (address ^. addressCity ==. val_ "Sugarland")
              &&. (address ^. addressState ==. val_ "TX")
        )

    runSelectReturningList $ select $ all_ (shoppingCartDb ^. shoppingCartUserAddresses)

  mapM_ print addresses

  -- Deletions

  runBeamSqliteDebug putStrLn conn $
    runDelete $
      delete
        -- The table
        (shoppingCartDb ^. shoppingCartUserAddresses)
        ( \address ->
            (address ^. addressCity ==. "Houston")
              &&. (_addressForUser address `references_` betty)
        )

-- Part 3

-- the product table

data ProductT f = Product
  { _productId :: C f Int32,
    _productTitle :: C f Text,
    _productDescription :: C f Text,
    _productPrice :: C f Int32 {- Price in cents -}
  }
  deriving (Generic, Beamable)

type Product = ProductT Identity

deriving instance Show Product

instance Table ProductT where
  data PrimaryKey ProductT f = ProductId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = ProductId . _productId

-- the order table

deriving instance Show (PrimaryKey AddressT Identity)

data OrderT f = Order
  { _orderId :: Columnar f Int32,
    _orderDate :: Columnar f LocalTime,
    _orderForUser :: PrimaryKey UserT f,
    _orderShipToAddress :: PrimaryKey AddressT f,
    _orderShippingInfo :: PrimaryKey ShippingInfoT (Nullable f)
  }
  deriving (Generic, Beamable)

type Order = OrderT Identity

deriving instance Show Order

instance Table OrderT where
  data PrimaryKey OrderT f = OrderId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = OrderId . _orderId

data ShippingCarrier = USPS | FedEx | UPS | DHL
  deriving (Show, Read, Eq, Ord, Enum)

-- this type can be saved in the DB
-- need UndecidableInstances
instance (HasSqlValueSyntax be String) => HasSqlValueSyntax be ShippingCarrier where
  sqlValueSyntax = autoSqlValueSyntax

-- and retrieved from the DB
instance FromBackendRow Sqlite ShippingCarrier where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data ShippingInfoT f = ShippingInfo
  { _shippingInfoId :: Columnar f Int32,
    _shippingInfoCarrier :: Columnar f ShippingCarrier,
    _shippingInfoTrackingNumber :: Columnar f Text
  }
  deriving (Generic, Beamable)

type ShippingInfo = ShippingInfoT Identity

deriving instance Show ShippingInfo

instance Table ShippingInfoT where
  data PrimaryKey ShippingInfoT f = ShippingInfoId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = ShippingInfoId . _shippingInfoId

deriving instance Show (PrimaryKey ShippingInfoT (Nullable Identity))

-- the lines of the order

deriving instance Show (PrimaryKey OrderT Identity)

deriving instance Show (PrimaryKey ProductT Identity)

data LineItemT f = LineItem
  { _lineItemInOrder :: PrimaryKey OrderT f,
    _lineItemForProduct :: PrimaryKey ProductT f,
    _lineItemQuantity :: Columnar f Int32
  }
  deriving (Generic, Beamable)

type LineItem = LineItemT Identity

deriving instance Show LineItem

instance Table LineItemT where
  data PrimaryKey LineItemT f = LineItemId (PrimaryKey OrderT f) (PrimaryKey ProductT f)
    deriving (Generic, Beamable)

  -- Applicative instance of (->) is used here
  primaryKey = LineItemId <$> _lineItemInOrder <*> _lineItemForProduct

-- Some convenience lenses

lineItemQuantity :: Lens' (LineItemT f) (Columnar f Int32)
LineItem _ _ (LensFor lineItemQuantity) = tableLenses

productId :: Lens' (ProductT f) (Columnar f Int32)
Product (LensFor productId) _ _ _ = tableLenses

productTitle :: Lens' (ProductT f) (Columnar f Text)
Product _ (LensFor productTitle) _ _ = tableLenses

productDescription :: Lens' (ProductT f) (Columnar f Text)
Product _ _ (LensFor productDescription) _ = tableLenses

productPrice :: Lens' (ProductT f) (Columnar f Int32)
Product _ _ _ (LensFor productPrice) = tableLenses

-- DB definition
data ShoppingCartDb f = ShoppingCartDb
  { _shoppingCartUsers :: f (TableEntity UserT),
    _shoppingCartUserAddresses :: f (TableEntity AddressT),
    _shoppingCartProducts :: f (TableEntity ProductT),
    _shoppingCartOrders :: f (TableEntity OrderT),
    _shoppingCartShippingInfos :: f (TableEntity ShippingInfoT),
    _shoppingCartLineItems :: f (TableEntity LineItemT)
  }
  deriving (Generic, Database be)

ShoppingCartDb
  (TableLens shoppingCartUsers)
  (TableLens shoppingCartUserAddresses)
  (TableLens shoppingCartProducts)
  (TableLens shoppingCartOrders)
  (TableLens shoppingCartShippingInfos)
  (TableLens shoppingCartLineItems) = dbLenses

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb =
  defaultDbSettings
    `withDbModification` dbModification
      { _shoppingCartUserAddresses =
          setEntityName "addresses"
            <> modifyTableFields
              tableModification
                { _addressLine1 = "address1",
                  _addressLine2 = "address2"
                },
        _shoppingCartProducts = setEntityName "products",
        _shoppingCartOrders =
          setEntityName "orders"
            <> modifyTableFields
              tableModification
                { _orderShippingInfo = ShippingInfoId "shipping_info__id"
                },
        _shoppingCartShippingInfos =
          setEntityName "shipping_info"
            <> modifyTableFields
              tableModification
                { _shippingInfoId = "id",
                  _shippingInfoCarrier = "carrier",
                  _shippingInfoTrackingNumber = "tracking_number"
                },
        _shoppingCartLineItems = setEntityName "line_items"
      }

part3 :: IO ()
part3 = do
  conn <- open "shoppingcart3.db"

  -- create the schema
  execute_ conn "CREATE TABLE cart_users (email VARCHAR NOT NULL, first_name VARCHAR NOT NULL, last_name VARCHAR NOT NULL, password VARCHAR NOT NULL, PRIMARY KEY( email ));"
  execute_ conn "CREATE TABLE addresses ( id INTEGER PRIMARY KEY AUTOINCREMENT, address1 VARCHAR NOT NULL, address2 VARCHAR, city VARCHAR NOT NULL, state VARCHAR NOT NULL, zip VARCHAR NOT NULL, for_user__email VARCHAR NOT NULL );"
  execute_ conn "CREATE TABLE products ( id INTEGER PRIMARY KEY AUTOINCREMENT, title VARCHAR NOT NULL, description VARCHAR NOT NULL, price INT NOT NULL );"
  execute_ conn "CREATE TABLE orders ( id INTEGER PRIMARY KEY AUTOINCREMENT, date TIMESTAMP NOT NULL, for_user__email VARCHAR NOT NULL, ship_to_address__id INT NOT NULL, shipping_info__id INT);"
  execute_ conn "CREATE TABLE shipping_info ( id INTEGER PRIMARY KEY AUTOINCREMENT, carrier VARCHAR NOT NULL, tracking_number VARCHAR NOT NULL);"
  execute_ conn "CREATE TABLE line_items (item_in_order__id INTEGER NOT NULL, item_for_product__id INTEGER NOT NULL, item_quantity INTEGER NOT NULL)"

  let users@[james, betty, sam] =
        [ User
            "james@example.com"
            "James"
            "Smith"
            "b4cc344d25a2efe540adbf2678e2304c" {- james -},
          User
            "betty@example.com"
            "Betty"
            "Jones"
            "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -},
          User
            "sam@example.com"
            "Sam"
            "Taylor"
            "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
        ]
      addresses =
        [ Address
            default_
            (val_ "123 Little Street")
            (val_ Nothing)
            (val_ "Boston")
            (val_ "MA")
            (val_ "12345")
            (pk james),
          Address
            default_
            (val_ "222 Main Street")
            (val_ (Just "Ste 1"))
            (val_ "Houston")
            (val_ "TX")
            (val_ "8888")
            (pk betty),
          Address
            default_
            (val_ "9999 Residence Ave")
            (val_ Nothing)
            (val_ "Sugarland")
            (val_ "TX")
            (val_ "8989")
            (pk betty)
        ]

      products =
        [ Product
            default_
            (val_ "Red Ball")
            (val_ "A bright red, very spherical ball")
            (val_ 1000),
          Product
            default_
            (val_ "Math Textbook")
            (val_ "Contains a lot of important math theorems and formulae")
            (val_ 2500),
          Product
            default_
            (val_ "Intro to Haskell")
            (val_ "Learn the best programming language in the world")
            (val_ 3000),
          Product
            default_
            (val_ "Suitcase")
            "A hard durable suitcase"
            15000
        ]

  ( jamesAddress1,
    bettyAddress1,
    bettyAddress2,
    redBall,
    mathTextbook,
    introToHaskell,
    suitcase
    ) <-
    runBeamSqliteDebug putStrLn conn $ do
      runInsert $
        insert (shoppingCartDb ^. shoppingCartUsers) $
          insertValues users

      [jamesAddress1, bettyAddress1, bettyAddress2] <-
        runInsertReturningList $
          insertReturning (shoppingCartDb ^. shoppingCartUserAddresses) $
            insertExpressions addresses

      [redBall, mathTextbook, introToHaskell, suitcase] <-
        runInsertReturningList $
          insertReturning (shoppingCartDb ^. shoppingCartProducts) $
            insertExpressions products

      pure
        ( jamesAddress1,
          bettyAddress1,
          bettyAddress2,
          redBall,
          mathTextbook,
          introToHaskell,
          suitcase
        )

  bettyShippingInfo <-
    runBeamSqliteDebug putStrLn conn $ do
      [bettyShippingInfo] <-
        runInsertReturningList $
          insertReturning (shoppingCartDb ^. shoppingCartShippingInfos) $
            -- the defined instances HasSqlValueSyntax and FromBackendRow are used here
            insertExpressions [ShippingInfo default_ (val_ USPS) (val_ "12345790ABCDEFGHI")]
      pure bettyShippingInfo

  -- add some orders
  [jamesOrder1, bettyOrder1, jamesOrder2] <-
    runBeamSqliteDebug putStrLn conn $ do
      runInsertReturningList $
        insertReturning (shoppingCartDb ^. shoppingCartOrders) $
          insertExpressions $
            [ Order
                default_
                currentTimestamp_ -- current time autocomputed
                (val_ (pk james))
                (val_ (pk jamesAddress1))
                nothing_,
              Order
                default_
                currentTimestamp_
                (val_ (pk betty))
                (val_ (pk bettyAddress1))
                (just_ (val_ (pk bettyShippingInfo))),
              Order
                default_
                currentTimestamp_
                (val_ (pk james))
                (val_ (pk jamesAddress1))
                nothing_
            ]

  print jamesOrder1
  print bettyOrder1
  print jamesOrder2

  -- and some lines in the orders
  let lineItems =
        [ LineItem (pk jamesOrder1) (pk redBall) 10,
          LineItem (pk jamesOrder1) (pk mathTextbook) 1,
          LineItem (pk jamesOrder1) (pk introToHaskell) 4,
          LineItem (pk bettyOrder1) (pk mathTextbook) 3,
          LineItem (pk bettyOrder1) (pk introToHaskell) 3,
          LineItem (pk jamesOrder2) (pk mathTextbook) 1
        ]

  runBeamSqliteDebug putStrLn conn $ do
    runInsert $
      insert (shoppingCartDb ^. shoppingCartLineItems) $
        insertValues lineItems

  -- left joins
  -- users and their orders (can be none)
  usersAndOrders <-
    runBeamSqliteDebug putStrLn conn $
      runSelectReturningList $
        select $ do
          user <- all_ (shoppingCartDb ^. shoppingCartUsers)
          order <-
            leftJoin_
              (all_ (shoppingCartDb ^. shoppingCartOrders))
              (\order -> _orderForUser order `references_` user)
          pure (user, order)

  putStrLn "Users and their orders (if any):"
  mapM_ print usersAndOrders

  -- users without orders
  usersWithNoOrders <-
    runBeamSqliteDebug putStrLn conn $
      runSelectReturningList $
        select $ do
          user <- all_ (shoppingCartDb ^. shoppingCartUsers)
          order <-
            leftJoin_
              (all_ (shoppingCartDb ^. shoppingCartOrders))
              (\order -> _orderForUser order `references_` user)
          guard_ (isNothing_ order)
          pure user

  putStrLn "Users with no orders:"
  mapM_ print usersWithNoOrders

  -- same using exists
  usersWithNoOrders <-
    runBeamSqliteDebug putStrLn conn $
      runSelectReturningList $
        select $ do
          user <- all_ (shoppingCartDb ^. shoppingCartUsers)
          guard_
            ( not_
                ( exists_
                    ( filter_
                        (\order -> _orderForUser order `references_` user)
                        (all_ (shoppingCartDb ^. shoppingCartOrders))
                    )
                )
            )
          pure user

  putStrLn "Users with no orders (using exists_):"
  mapM_ print usersWithNoOrders

  ordersWithCostOrdered <-
    runBeamSqliteDebug putStrLn conn
      $ runSelectReturningList
      $ select
      $ orderBy_ (\(order, total) -> desc_ total)
      $ aggregate_
        ( \(order, lineItem, product) ->
            ( group_ order,
              sum_ (lineItem ^. lineItemQuantity * product ^. productPrice)
            )
        )
      $ do
        lineItem <- all_ (shoppingCartDb ^. shoppingCartLineItems)
        order <-
          related_
            (shoppingCartDb ^. shoppingCartOrders)
            (_lineItemInOrder lineItem)
        product <-
          related_
            (shoppingCartDb ^. shoppingCartProducts)
            (_lineItemForProduct lineItem)
        pure (order, lineItem, product)

  putStrLn "Orders with their total cost ordered by cost descending:"
  mapM_ print ordersWithCostOrdered

  -- total spend by the user
  allUsersAndTotals <-
    runBeamSqliteDebug putStrLn conn
      $ runSelectReturningList
      $ select
      $ orderBy_ (\(user, total) -> desc_ total)
      $ aggregate_
        ( \(user, lineItem, product) ->
            ( group_ user,
              sum_
                ( maybe_ 0 id (_lineItemQuantity lineItem)
                    * maybe_ 0 id (product ^. productPrice)
                )
            )
        )
      $ do
        user <- all_ (shoppingCartDb ^. shoppingCartUsers)
        order <-
          leftJoin_
            (all_ (shoppingCartDb ^. shoppingCartOrders))
            (\order -> _orderForUser order `references_` user)
        lineItem <-
          leftJoin_
            (all_ (shoppingCartDb ^. shoppingCartLineItems))
            ( \lineItem ->
                maybe_
                  (val_ False)
                  (\order -> _lineItemInOrder lineItem `references_` order)
                  order
            )
        product <-
          leftJoin_
            (all_ (shoppingCartDb ^. shoppingCartProducts))
            ( \product ->
                maybe_
                  (val_ False)
                  (\lineItem -> _lineItemForProduct lineItem `references_` product)
                  lineItem
            )
        pure (user, lineItem, product)

  putStrLn "All users and their total spend:"
  mapM_ print allUsersAndTotals

  -- better version without cases
  allUsersAndTotals2 <-
    runBeamSqliteDebug putStrLn conn
      $ runSelectReturningList
      $ select
      $ orderBy_ (\(user, total) -> desc_ total)
      $ aggregate_
        ( \(user, lineItem, product) ->
            ( group_ user,
              sum_
                ( maybe_
                    0
                    id
                    (_lineItemQuantity lineItem)
                    * maybe_ 0 id (product ^. productPrice)
                )
            )
        )
      $ do
        user <- all_ (shoppingCartDb ^. shoppingCartUsers)
        order <-
          leftJoin_
            (all_ (shoppingCartDb ^. shoppingCartOrders))
            (\order -> _orderForUser order `references_` user)
        lineItem <-
          -- alternative for not using maybe_
          leftJoin_'
            (all_ (shoppingCartDb ^. shoppingCartLineItems))
            (\lineItem -> just_ (_lineItemInOrder lineItem) ==?. pk order)
        product <-
          leftJoin_'
            (all_ (shoppingCartDb ^. shoppingCartProducts))
            (\product -> _lineItemForProduct lineItem ==?. just_ (pk product))
        pure (user, lineItem, product)

  putStrLn "All users and their total spend (better version):"
  mapM_ print allUsersAndTotals2

  -- queries with nullable foreign keys

  allUnshippedOrders <-
    runBeamSqliteDebug putStrLn conn $
      runSelectReturningList $
        select $
          filter_ (isNothing_ . _orderShippingInfo) $
            all_ (shoppingCartDb ^. shoppingCartOrders)

  mapM_ print allUnshippedOrders

  -- query with the NULL issue
  shippingInformationByUser <-
    runBeamSqliteDebug putStrLn conn
      $ runSelectReturningList
      $ select
      $ aggregate_
        ( \(user, order) ->
            let ShippingInfoId shippingInfoId = _orderShippingInfo order
             in ( group_ user,
                  as_ @Int32 $
                    count_
                      ( as_ @(Maybe Int32)
                          (maybe_ (just_ 1) (\_ -> nothing_) shippingInfoId)
                      ),
                  as_ @Int32 $ count_ shippingInfoId
                )
        )
      $ do
        user <- all_ (shoppingCartDb ^. shoppingCartUsers)
        order <-
          leftJoin_
            (all_ (shoppingCartDb ^. shoppingCartOrders))
            (\order -> _orderForUser order `references_` user)
        pure (user, order)

  mapM_ print shippingInformationByUser

  -- fixed version - automatic subselect creation

  shippingInformationByUser <-
    runBeamSqliteDebug putStrLn conn $
      runSelectReturningList $
        select $ do
          user <- all_ (shoppingCartDb ^. shoppingCartUsers)

          (userEmail, unshippedCount) <-
            aggregate_
              (\(userEmail, order) -> (group_ userEmail, as_ @Int32 countAll_))
              $ do
                user <- all_ (shoppingCartDb ^. shoppingCartUsers)
                order <-
                  leftJoin_
                    (all_ (shoppingCartDb ^. shoppingCartOrders))
                    ( \order ->
                        (_orderForUser order `references_` user)
                          &&. (isNothing_ (_orderShippingInfo order))
                    )
                pure (pk user, order)

          guard_ (userEmail `references_` user)

          (userEmail, shippedCount) <-
            aggregate_
              (\(userEmail, order) -> (group_ userEmail, as_ @Int32 countAll_))
              $ do
                user <- all_ (shoppingCartDb ^. shoppingCartUsers)
                order <-
                  leftJoin_
                    (all_ (shoppingCartDb ^. shoppingCartOrders))
                    ( \order ->
                        (_orderForUser order `references_` user)
                          &&. (isJust_ (_orderShippingInfo order))
                    )
                pure (pk user, order)
          guard_ (userEmail `references_` user)

          pure (user, unshippedCount, shippedCount)

  mapM_ print shippingInformationByUser

  -- other version - explicit subselects

  shippingInformationByUser <-
    runBeamSqliteDebug putStrLn conn $
      runSelectReturningList $
        select $ do
          user <- all_ (shoppingCartDb ^. shoppingCartUsers)

          (userEmail, unshippedCount) <-
            subselect_
              $ aggregate_
                (\(userEmail, order) -> (group_ userEmail, as_ @Int32 countAll_))
              $ do
                user <- all_ (shoppingCartDb ^. shoppingCartUsers)
                order <-
                  leftJoin_
                    (all_ (shoppingCartDb ^. shoppingCartOrders))
                    ( \order ->
                        (_orderForUser order `references_` user)
                          &&. (isNothing_ (_orderShippingInfo order))
                    )
                pure (pk user, order)

          guard_ (userEmail `references_` user)

          (userEmail, shippedCount) <-
            subselect_
              $ aggregate_
                (\(userEmail, order) -> (group_ userEmail, as_ @Int32 countAll_))
              $ do
                user <- all_ (shoppingCartDb ^. shoppingCartUsers)
                order <-
                  leftJoin_
                    (all_ (shoppingCartDb ^. shoppingCartOrders))
                    ( \order ->
                        (_orderForUser order `references_` user)
                          &&. (isJust_ (_orderShippingInfo order))
                    )
                pure (pk user, order)
          guard_ (userEmail `references_` user)

          pure (user, unshippedCount, shippedCount)

  mapM_ print shippingInformationByUser

  pure ()
