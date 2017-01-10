{-# LANGUAGE QuasiQuotes #-}
module Main (
  main
) where

import Data.ByteString.Char8 (unpack)
import Data.Char (toLower)
import Data.HashMap.Strict (fromListWith, toList)
import Data.Maybe (fromJust, fromMaybe)
import Data.Version (showVersion)
import LDAP.Init (ldapSimpleBind, ldapExternalSaslBind, ldapInitialize)
import LDAP.Modify (LDAPMod(..), LDAPModOp(..), ldapAdd, ldapDelete, ldapModify, list2ldm)
import LDAP.Search (LDAPScope(LdapScopeBase), SearchAttributes(LDAPAllUserAttrs), LDAPEntry(..), ldapSearch)
import LDAP.Types (LDAP)
import Paths_ldapply (version) -- from cabal
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (IOMode(ReadMode), hGetLine, hIsEOF, withFile)
import Text.InterpolatedString.Perl6 (qc)
import Text.LDIF.Parser (defaulLDIFConf, parseLDIFFile)
import Text.LDIF.Printer (dn2str)
import Text.LDIF.Types (Attribute(..), Value(..), Change(..), LDIF(..), LDIFRecord(..), Modify(..), reDN)
import qualified System.Console.Docopt.NoTH as O

{--
 TODO:
    1. Streaming from stdin (good for large amount of LDIF data)
--}

usage :: String
usage =
    "ldapply " ++ showVersion version ++
    " - LDIF dempotent apply tool" ++ [qc|

Usage:
  ldapply [options] LDIF...

Options:
  -H <ldapuri>       LDAP URL to connect to [default: ldapi:///]

  -x                 Use simple bind instead of default SASL External
  -D <binddn>        Use <binddn> for the distinguished name or authorization identity
  -w <passwd>        Use <passwd> as the password for simple bind
  -y <passwdfile>    Read password from <passwdfile>, only the first line is read

  -h, --help         Show this message

If option -w is given, -y is ignored.
|]


main :: IO ()
main = do
  doco <- O.parseUsageOrExit usage
  args <- O.parseArgsOrExit doco =<< getArgs
  if args `O.isPresent` O.longOption "help"
  then putStrLn $ O.usage doco
  else do
    let
      ldifs = O.getAllArgs args $ O.argument "LDIF"
      ldapUrl = fromJust $ O.getArg args $ O.shortOption 'H'
      simple = O.isPresent args $ O.shortOption 'x'
      binddn = fromMaybe "" $ O.getArg args $ O.shortOption 'D'
      passwd = O.getArg args $ O.shortOption 'w'
      passwdfile = O.getArg args $ O.shortOption 'y'
    ldap <- ldapInitialize ldapUrl
    if simple then simpleBind ldap binddn passwd passwdfile
              else ldapExternalSaslBind ldap binddn
    mapM_ (processLDIF ldap) ldifs


simpleBind :: LDAP -> String -> Maybe String -> Maybe FilePath -> IO ()
simpleBind ldap bdn (Just pwd)  _    = ldapSimpleBind ldap bdn pwd
simpleBind ldap bdn Nothing Nothing  = ldapSimpleBind ldap bdn ""
simpleBind ldap bdn Nothing (Just f) = do
  pwd <- withFile f ReadMode $ \h -> do
    empty <- hIsEOF h
    if empty then return "" else hGetLine h
  ldapSimpleBind ldap bdn pwd


processLDIF :: LDAP -> FilePath -> IO ()
processLDIF ldap f = do
  p <- parseLDIFFile defaulLDIFConf f
  case p of
    Left err          -> die $ show err
    Right (LDIF _ rs) -> mapM_ (apply ldap) rs


apply :: LDAP -> LDIFRecord -> IO ()
apply ldap rec = do
  putStrLn $ "looking for " ++ show (dn rec)
  entries <- ldapSearch ldap (Just $ dn rec) LdapScopeBase Nothing LDAPAllUserAttrs False
  case entries of
    []  -> do
      putStrLn $ "not found " ++ show (dn rec)
      update ldap Nothing rec
    [e] -> do
      putStrLn $ "found " ++ show (dn rec)
      update ldap (Just e) rec
    _   -> die $ "internal error: too many entries in response (only 1 or 0 expected): "
                 ++ show (length entries)


update :: LDAP -> Maybe LDAPEntry -> LDIFRecord -> IO ()
update _ Nothing (ChangeRecord _ ChangeDelete) = return ()

update _ Nothing rec@(ChangeRecord _ _) =
  die $ "cannot update non-existing entry " ++ show (dn rec)

update ldap (Just _) rec@(ChangeRecord _ ch)= do
  putStrLn $ "modifing " ++ show (dn rec)
  change ldap (dn rec) ch

update ldap Nothing rec@(ContentRecord _ av) = do
  putStrLn $ "adding " ++ show (dn rec)
  ldapAdd ldap (dn rec) . list2ldm LdapModAdd . collect $ av

update ldap (Just (LDAPEntry _ attrs)) rec@(ContentRecord _ av)= do
  putStrLn $ "replacing " ++ show (dn rec)
  ldapModify ldap (dn rec) (replace ++ delete)
  where
    replace = list2ldm LdapModReplace newAttrs
    delete = list2ldm LdapModDelete oldAttrs
    newAttrs = collect av
    oldAttrs = [ a | a@(v, _) <- attrs, notElem (low v) $ map fst newAttrs ]


change :: LDAP -> String -> Change -> IO ()
change _    _ ChangeModDN      = die "modrdn is not supported"
change ldap n (ChangeAdd av)   = ldapModify ldap n . list2ldm LdapModAdd . collect $ av
change ldap n (ChangeModify m) = ldapModify ldap n . map mod2mod $ m
change ldap n ChangeDelete     = ldapDelete ldap n

mod2mod :: Modify -> LDAPMod
mod2mod (ModAdd     a vv) = LDAPMod LdapModAdd     (attr2str a) (map val2str vv)
mod2mod (ModDelete  a vv) = LDAPMod LdapModDelete  (attr2str a) (map val2str vv)
mod2mod (ModReplace a vv) = LDAPMod LdapModReplace (attr2str a) (map val2str vv)

collect :: [(Attribute, Value)] -> [(String, [String])]
collect = toList . fromListWith (++) . map (\(a, v) -> (attr2str a, [val2str v]))

attr2str :: Attribute -> String
attr2str (Attribute a) = low . unpack $ a

val2str :: Value -> String
val2str (Value  v) = unpack v
val2str (ValueI v) = unpack v

dn :: LDIFRecord -> String
dn = unpack . dn2str . reDN

low :: String -> String
low = map toLower

