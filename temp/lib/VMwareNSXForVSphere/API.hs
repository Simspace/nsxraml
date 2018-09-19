{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=328 #-}

module VMwareNSXForVSphere.API
  -- * Client and Server
  ( ServerConfig(..)
  , VMwareNSXForVSphereBackend
  , createVMwareNSXForVSphereClient
  , runVMwareNSXForVSphereServer
  , runVMwareNSXForVSphereClient
  , runVMwareNSXForVSphereClientWithManager
  , VMwareNSXForVSphereClient
  -- ** Servant
  , VMwareNSXForVSphereAPI
  ) where

import VMwareNSXForVSphere.Types

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (ServantErr, serve)
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Servant.Client (Scheme(Http), ServantError, client)
import Servant.Common.BaseUrl (BaseUrl(..))
import Web.HttpApiData




-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either String b
lookupEither key assocs =
  case lookup key assocs of
    Nothing -> Left $ "Could not find parameter " <> (T.unpack key) <> " in form data"
    Just value ->
      case parseQueryParam value of
        Left result -> Left $ T.unpack result
        Right result -> Right $ result

-- | Servant type-level API, generated from the Swagger spec for VMwareNSXForVSphere.
type VMwareNSXForVSphereAPI
    =    "1.0" :> "appliance-management" :> "backuprestore" :> "backupsettings" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE10ApplianceManagementBackuprestoreBackupsettings' route
    :<|> "1.0" :> "appliance-management" :> "backuprestore" :> "backupsettings" :> "schedule" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE10ApplianceManagementBackuprestoreBackupsettingsSchedule' route
    :<|> "1.0" :> "appliance-management" :> "notifications" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE10ApplianceManagementNotifications' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "network" :> "dns" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE10ApplianceManagementSystemNetworkDns' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "syslogserver" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE10ApplianceManagementSystemSyslogserver' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "syslogservers" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE10ApplianceManagementSystemSyslogservers' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "timesettings" :> "ntp" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE10ApplianceManagementSystemTimesettingsNtp' route
    :<|> "1.0" :> "directory" :> "deleteDomain" :> Capture "ID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE10DirectoryDeleteDomainID' route
    :<|> "1.0" :> "directory" :> "deleteDomainRootDN" :> Capture "domainID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE10DirectoryDeleteDomainRootDNDomainID' route
    :<|> "1.0" :> "directory" :> "deleteEventLogServer" :> Capture "serverID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE10DirectoryDeleteEventLogServerServerID' route
    :<|> "1.0" :> "directory" :> "deleteLdapServer" :> Capture "serverID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE10DirectoryDeleteLdapServerServerID' route
    :<|> "1.0" :> "identity" :> "staticUserMappingsbyIP" :> Capture "IP" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE10IdentityStaticUserMappingsbyIPIP' route
    :<|> "1.0" :> "identity" :> "staticUserMappingsbyUser" :> Capture "userID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE10IdentityStaticUserMappingsbyUserUserID' route
    :<|> "2.0" :> "endpointsecurity" :> "activation" :> Capture "vendorID" Text :> Capture "altitude" Text :> Capture "moid" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20EndpointsecurityActivationVendorIDAltitudeMoid' route
    :<|> "2.0" :> "endpointsecurity" :> "registration" :> Capture "vendorID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20EndpointsecurityRegistrationVendorID' route
    :<|> "2.0" :> "endpointsecurity" :> "registration" :> Capture "vendorID" Text :> Capture "altitude" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20EndpointsecurityRegistrationVendorIDAltitude' route
    :<|> "2.0" :> "endpointsecurity" :> "registration" :> Capture "vendorID" Text :> Capture "altitude" Text :> "location" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20EndpointsecurityRegistrationVendorIDAltitudeLocation' route
    :<|> "2.0" :> "nwfabric" :> "clusters" :> Capture "clusterID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20NwfabricClustersClusterID' route
    :<|> "2.0" :> "nwfabric" :> "configure" :> ReqBody '[JSON] NwFabricConfig :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20NwfabricConfigure' route
    :<|> "2.0" :> "nwfabric" :> "hosts" :> Capture "hostID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20NwfabricHostsHostID' route
    :<|> "2.0" :> "services" :> "application" :> Capture "applicationId" Text :> QueryParam "force" Bool :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesApplicationApplicationId' route
    :<|> "2.0" :> "services" :> "applicationgroup" :> Capture "applicationgroupId" Text :> QueryParam "force" Bool :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesApplicationgroupApplicationgroupId' route
    :<|> "2.0" :> "services" :> "applicationgroup" :> Capture "applicationgroupId" Text :> "members" :> Capture "moref" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesApplicationgroupApplicationgroupIdMembersMoref' route
    :<|> "2.0" :> "services" :> "dashboard" :> "ui-views" :> "dashboard" :> "widgetconfigurations" :> Capture "widgetconfigurationId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesDashboardUiViewsDashboardWidgetconfigurationsWidgetconfigurationId' route
    :<|> "2.0" :> "services" :> "ipam" :> "pools" :> Capture "poolId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesIpamPoolsPoolId' route
    :<|> "2.0" :> "services" :> "ipam" :> "pools" :> Capture "poolId" Text :> "ipaddresses" :> Capture "ipAddress" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesIpamPoolsPoolIdIpaddressesIpAddress' route
    :<|> "2.0" :> "services" :> "ipset" :> Capture "ipsetId" Text :> QueryParam "force" Bool :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesIpsetIpsetId' route
    :<|> "2.0" :> "services" :> "macset" :> Capture "macsetId" Text :> QueryParam "force" Bool :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesMacsetMacsetId' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> Capture "ID" Text :> QueryParam "force" Bool :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesPolicySecuritypolicyID' route
    :<|> "2.0" :> "services" :> "securitygroup" :> Capture "objectId" Text :> QueryParam "force" Bool :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesSecuritygroupObjectId' route
    :<|> "2.0" :> "services" :> "securitygroup" :> Capture "objectId" Text :> "members" :> Capture "memberId" Text :> QueryParam "failIfAbsent" Bool :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesSecuritygroupObjectIdMembersMemberId' route
    :<|> "2.0" :> "services" :> "securitytags" :> "tag" :> Capture "tagId" Text :> QueryParam "force" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesSecuritytagsTagTagId' route
    :<|> "2.0" :> "services" :> "securitytags" :> "tag" :> Capture "tagId" Text :> "vm" :> Capture "vmId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesSecuritytagsTagTagIdVmVmId' route
    :<|> "2.0" :> "services" :> "snmp" :> "manager" :> Capture "managerId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesSnmpManagerManagerId' route
    :<|> "2.0" :> "services" :> "ssoconfig" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesSsoconfig' route
    :<|> "2.0" :> "services" :> "truststore" :> "config" :> Capture "certificateId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesTruststoreConfigCertificateId' route
    :<|> "2.0" :> "services" :> "truststore" :> "crl" :> Capture "crlId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesTruststoreCrlCrlId' route
    :<|> "2.0" :> "services" :> "usermgmt" :> "role" :> Capture "userId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesUsermgmtRoleUserId' route
    :<|> "2.0" :> "services" :> "usermgmt" :> "user" :> Capture "userId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20ServicesUsermgmtUserUserId' route
    :<|> "2.0" :> "si" :> "deploy" :> "cluster" :> Capture "clusterID" Text :> QueryParam "services" Text :> QueryParam "startTime" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20SiDeployClusterClusterID' route
    :<|> "2.0" :> "si" :> "deploy" :> "service" :> Capture "serviceID" Text :> QueryParam "clusters" Text :> QueryParam "startTime" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20SiDeployServiceServiceID' route
    :<|> "2.0" :> "techsupportbundle" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20Techsupportbundle' route
    :<|> "2.0" :> "universalsync" :> "configuration" :> "nsxmanagers" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20UniversalsyncConfigurationNsxmanagers' route
    :<|> "2.0" :> "universalsync" :> "configuration" :> "nsxmanagers" :> Capture "nsxManagerID" Text :> QueryParam "forceRemoval" Bool :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20UniversalsyncConfigurationNsxmanagersNsxManagerID' route
    :<|> "2.0" :> "vdn" :> "config" :> "multicasts" :> Capture "multicastAddresssRangeId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20VdnConfigMulticastsMulticastAddresssRangeId' route
    :<|> "2.0" :> "vdn" :> "config" :> "segments" :> Capture "segmentPoolId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20VdnConfigSegmentsSegmentPoolId' route
    :<|> "2.0" :> "vdn" :> "controller" :> Capture "controllerId" Text :> QueryParam "forceRemoval" Bool :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20VdnControllerControllerId' route
    :<|> "2.0" :> "vdn" :> "controller" :> Capture "controllerId" Text :> "syslog" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20VdnControllerControllerIdSyslog' route
    :<|> "2.0" :> "vdn" :> "hardwaregateway" :> "bindings" :> Capture "bindingId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20VdnHardwaregatewayBindingsBindingId' route
    :<|> "2.0" :> "vdn" :> "hardwaregateways" :> Capture "hardwareGatewayId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20VdnHardwaregatewaysHardwareGatewayId' route
    :<|> "2.0" :> "vdn" :> "scopes" :> Capture "scopeId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20VdnScopesScopeId' route
    :<|> "2.0" :> "vdn" :> "switches" :> Capture "vdsId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20VdnSwitchesVdsId' route
    :<|> "2.0" :> "vdn" :> "virtualwires" :> Capture "virtualWireID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE20VdnVirtualwiresVirtualWireID' route
    :<|> "2.1" :> "app" :> "excludelist" :> Capture "memberID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE21AppExcludelistMemberID' route
    :<|> "2.1" :> "app" :> "flow" :> Capture "contextId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE21AppFlowContextId' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeId' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "appliances" :> Capture "haIndex" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdAppliancesHaIndex' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "bridging" :> "config" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdBridgingConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dhcp" :> "config" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdDhcpConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dhcp" :> "config" :> "bindings" :> Capture "bindingID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdDhcpConfigBindingsBindingID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dhcp" :> "config" :> "ippools" :> Capture "poolID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdDhcpConfigIppoolsPoolID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dhcp" :> "config" :> "relay" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdDhcpConfigRelay' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dns" :> "config" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdDnsConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "firewall" :> "config" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdFirewallConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "firewall" :> "config" :> "rules" :> Capture "ruleId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdFirewallConfigRulesRuleId' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "highavailability" :> "config" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdHighavailabilityConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "interfaces" :> QueryParam "index" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdInterfaces' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "interfaces" :> Capture "index" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdInterfacesIndex' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "ipsec" :> "config" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdIpsecConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "l2vpn" :> "config" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdL2vpnConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdLoadbalancerConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "applicationprofiles" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdLoadbalancerConfigApplicationprofiles' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "applicationprofiles" :> Capture "appProfileID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdLoadbalancerConfigApplicationprofilesAppProfileID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "applicationrules" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdLoadbalancerConfigApplicationrules' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "applicationrules" :> Capture "appruleID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdLoadbalancerConfigApplicationrulesAppruleID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "monitors" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdLoadbalancerConfigMonitors' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "monitors" :> Capture "monitorID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdLoadbalancerConfigMonitorsMonitorID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "pools" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdLoadbalancerConfigPools' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "pools" :> Capture "poolID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdLoadbalancerConfigPoolsPoolID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "virtualservers" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdLoadbalancerConfigVirtualservers' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "virtualservers" :> Capture "virtualserverID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdLoadbalancerConfigVirtualserversVirtualserverID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "nat" :> "config" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdNatConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "nat" :> "config" :> "rules" :> Capture "ruleID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdNatConfigRulesRuleID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdRoutingConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> "bgp" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdRoutingConfigBgp' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> "ospf" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdRoutingConfigOspf' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> "static" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdRoutingConfigStatic' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "activesessions" :> Capture "sessionID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSslvpnActivesessionsSessionID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSslvpnConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "auth" :> "localserver" :> "users" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSslvpnConfigAuthLocalserverUsers' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "auth" :> "localserver" :> "users" :> Capture "userID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSslvpnConfigAuthLocalserverUsersUserID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "installpackages" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "installpackages" :> Capture "packageID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackagesPackageID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "ippools" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "ippools" :> Capture "ippoolID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppoolsIppoolID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "privatenetworks" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "privatenetworks" :> Capture "networkID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworksNetworkID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "script" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSslvpnConfigScript' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "script" :> Capture "fileID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSslvpnConfigScriptFileID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "syslog" :> "config" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSyslogConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "systemcontrol" :> "config" :> QueryParam "rebootNow" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdSystemcontrolConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "tunnels" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdTunnels' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "tunnels" :> Capture "tunnelId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdTunnelsTunnelId' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "vnics" :> Capture "index" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdVnicsIndex' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "vnics" :> Capture "parentVnicIndex" Text :> "subinterfaces" :> Capture "subInterfaceIndex" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40EdgesEdgeIdVnicsParentVnicIndexSubinterfacesSubInterfaceIndex' route
    :<|> "4.0" :> "firewall" :> "config" :> "sections" :> QueryParam "action" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40FirewallConfigSections' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40FirewallGlobalroot0Config' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "ipfix" :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40FirewallGlobalroot0ConfigIpfix' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer2sections" :> Capture "sectionId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40FirewallGlobalroot0ConfigLayer2sectionsSectionId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer2sections" :> Capture "sectionId" Text :> "rules" :> Capture "ruleId" Text :> Header "If-Match" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRulesRuleId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3redirectsections" :> Capture "section" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40FirewallGlobalroot0ConfigLayer3redirectsectionsSection' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3redirectsections" :> Capture "section" Text :> "rules" :> Capture "ruleID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRulesRuleID' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3sections" :> Capture "sectionId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40FirewallGlobalroot0ConfigLayer3sectionsSectionId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3sections" :> Capture "sectionId" Text :> "rules" :> Capture "ruleId" Text :> Header "If-Match" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRulesRuleId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "drafts" :> Capture "draftID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40FirewallGlobalroot0DraftsDraftID' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "timeouts" :> Capture "configId" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40FirewallGlobalroot0TimeoutsConfigId' route
    :<|> "4.0" :> "services" :> "spoofguard" :> "policies" :> Capture "policyID" Text :> Verb 'DELETE 200 '[JSON] () -- 'dELETE40ServicesSpoofguardPoliciesPolicyID' route
    :<|> "1.0" :> "appliance-management" :> "backuprestore" :> "backups" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementBackuprestoreBackups' route
    :<|> "1.0" :> "appliance-management" :> "backuprestore" :> "backupsettings" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementBackuprestoreBackupsettings' route
    :<|> "1.0" :> "appliance-management" :> "certificatemanager" :> "certificates" :> "nsx" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementCertificatemanagerCertificatesNsx' route
    :<|> "1.0" :> "appliance-management" :> "certificatemanager" :> "csr" :> "nsx" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementCertificatemanagerCsrNsx' route
    :<|> "1.0" :> "appliance-management" :> "components" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementComponents' route
    :<|> "1.0" :> "appliance-management" :> "components" :> "component" :> Capture "componentID" Text :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementComponentsComponentComponentID' route
    :<|> "1.0" :> "appliance-management" :> "components" :> "component" :> Capture "componentID" Text :> "dependencies" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementComponentsComponentComponentIDDependencies' route
    :<|> "1.0" :> "appliance-management" :> "components" :> "component" :> Capture "componentID" Text :> "dependents" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementComponentsComponentComponentIDDependents' route
    :<|> "1.0" :> "appliance-management" :> "components" :> "component" :> Capture "componentID" Text :> "status" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementComponentsComponentComponentIDStatus' route
    :<|> "1.0" :> "appliance-management" :> "global" :> "info" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementGlobalInfo' route
    :<|> "1.0" :> "appliance-management" :> "notifications" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementNotifications' route
    :<|> "1.0" :> "appliance-management" :> "summary" :> "components" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSummaryComponents' route
    :<|> "1.0" :> "appliance-management" :> "summary" :> "system" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSummarySystem' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "cpuinfo" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSystemCpuinfo' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "cpuinfo" :> "details" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSystemCpuinfoDetails' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "locale" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSystemLocale' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "meminfo" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSystemMeminfo' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "network" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSystemNetwork' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "securitysettings" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSystemSecuritysettings' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "storageinfo" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSystemStorageinfo' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "syslogserver" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSystemSyslogserver' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "syslogservers" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSystemSyslogservers' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "timesettings" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSystemTimesettings' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "tlssettings" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSystemTlssettings' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "uptime" :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementSystemUptime' route
    :<|> "1.0" :> "appliance-management" :> "techsupportlogs" :> Capture "filename" Text :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementTechsupportlogsFilename' route
    :<|> "1.0" :> "appliance-management" :> "upgrade" :> "information" :> Capture "componentID" Text :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementUpgradeInformationComponentID' route
    :<|> "1.0" :> "appliance-management" :> "upgrade" :> "status" :> Capture "componentID" Text :> Verb 'GET 200 '[JSON] () -- 'gET10ApplianceManagementUpgradeStatusComponentID' route
    :<|> "1.0" :> "directory" :> "listDomains" :> Verb 'GET 200 '[JSON] () -- 'gET10DirectoryListDomains' route
    :<|> "1.0" :> "directory" :> "listEventLogServersForDomain" :> Capture "domainID" Text :> Verb 'GET 200 '[JSON] () -- 'gET10DirectoryListEventLogServersForDomainDomainID' route
    :<|> "1.0" :> "directory" :> "listLdapServersForDomain" :> Capture "domainID" Text :> Verb 'GET 200 '[JSON] () -- 'gET10DirectoryListLdapServersForDomainDomainID' route
    :<|> "1.0" :> "eventcontrol" :> "config" :> "vm" :> Capture "vmID" Text :> Verb 'GET 200 '[JSON] () -- 'gET10EventcontrolConfigVmVmID' route
    :<|> "1.0" :> "identity" :> "directoryGroupsForUser" :> QueryParam "loginName" Text :> QueryParam "domainName" Text :> ReqBody '[JSON] Text :> Verb 'GET 200 '[JSON] () -- 'gET10IdentityDirectoryGroupsForUser' route
    :<|> "1.0" :> "identity" :> "hostIpMapping" :> Verb 'GET 200 '[JSON] () -- 'gET10IdentityHostIpMapping' route
    :<|> "1.0" :> "identity" :> "ipToUserMapping" :> Verb 'GET 200 '[JSON] () -- 'gET10IdentityIpToUserMapping' route
    :<|> "1.0" :> "identity" :> "staticUserMappings" :> Verb 'GET 200 '[JSON] () -- 'gET10IdentityStaticUserMappings' route
    :<|> "1.0" :> "identity" :> "staticUserMappingsbyIP" :> Capture "IP" Text :> Verb 'GET 200 '[JSON] () -- 'gET10IdentityStaticUserMappingsbyIPIP' route
    :<|> "1.0" :> "identity" :> "staticUserMappingsbyUser" :> Capture "userID" Text :> Verb 'GET 200 '[JSON] () -- 'gET10IdentityStaticUserMappingsbyUserUserID' route
    :<|> "1.0" :> "identity" :> "userIpMapping" :> Verb 'GET 200 '[JSON] () -- 'gET10IdentityUserIpMapping' route
    :<|> "1.0" :> "telemetry" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET10TelemetryConfig' route
    :<|> "1.0" :> "telemetry" :> "proxy" :> Verb 'GET 200 '[JSON] () -- 'gET10TelemetryProxy' route
    :<|> "2.0" :> "auditlog" :> QueryParam "startIndex" Text :> QueryParam "pageSize" Text :> QueryParam "sortOrderAscending" Text :> QueryParam "sortBy" Text :> Verb 'GET 200 '[JSON] () -- 'gET20Auditlog' route
    :<|> "2.0" :> "capacity-parameters" :> "report" :> Verb 'GET 200 '[JSON] () -- 'gET20CapacityParametersReport' route
    :<|> "2.0" :> "capacity-parameters" :> "thresholds" :> Verb 'GET 200 '[JSON] () -- 'gET20CapacityParametersThresholds' route
    :<|> "2.0" :> "eam" :> "status" :> Verb 'GET 200 '[JSON] () -- 'gET20EamStatus' route
    :<|> "2.0" :> "endpointsecurity" :> "activation" :> QueryParam "hostId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20EndpointsecurityActivation' route
    :<|> "2.0" :> "endpointsecurity" :> "activation" :> Capture "vendorID" Text :> Capture "altitude" Text :> Capture "moid" Text :> Verb 'GET 200 '[JSON] () -- 'gET20EndpointsecurityActivationVendorIDAltitudeMoid' route
    :<|> "2.0" :> "endpointsecurity" :> "activation" :> Capture "vendorID" Text :> Capture "solutionID" Text :> Verb 'GET 200 '[JSON] () -- 'gET20EndpointsecurityActivationVendorIDSolutionID' route
    :<|> "2.0" :> "endpointsecurity" :> "registration" :> Capture "vendorID" Text :> Verb 'GET 200 '[JSON] () -- 'gET20EndpointsecurityRegistrationVendorID' route
    :<|> "2.0" :> "endpointsecurity" :> "registration" :> Capture "vendorID" Text :> Capture "altitude" Text :> Verb 'GET 200 '[JSON] () -- 'gET20EndpointsecurityRegistrationVendorIDAltitude' route
    :<|> "2.0" :> "endpointsecurity" :> "registration" :> Capture "vendorID" Text :> Capture "altitude" Text :> "location" :> Verb 'GET 200 '[JSON] () -- 'gET20EndpointsecurityRegistrationVendorIDAltitudeLocation' route
    :<|> "2.0" :> "endpointsecurity" :> "registration" :> Capture "vendorID" Text :> "solutions" :> Verb 'GET 200 '[JSON] () -- 'gET20EndpointsecurityRegistrationVendorIDSolutions' route
    :<|> "2.0" :> "endpointsecurity" :> "registration" :> "vendors" :> Verb 'GET 200 '[JSON] () -- 'gET20EndpointsecurityRegistrationVendors' route
    :<|> "2.0" :> "endpointsecurity" :> "usvmstats" :> "usvmhealththresholds" :> Verb 'GET 200 '[JSON] () -- 'gET20EndpointsecurityUsvmstatsUsvmhealththresholds' route
    :<|> "2.0" :> "hostevents" :> Verb 'GET 200 '[JSON] () -- 'gET20Hostevents' route
    :<|> "2.0" :> "nwfabric" :> "clusters" :> Capture "clusterID" Text :> Verb 'GET 200 '[JSON] () -- 'gET20NwfabricClustersClusterID' route
    :<|> "2.0" :> "nwfabric" :> "features" :> Verb 'GET 200 '[JSON] () -- 'gET20NwfabricFeatures' route
    :<|> "2.0" :> "nwfabric" :> "hosts" :> Capture "hostID" Text :> Verb 'GET 200 '[JSON] () -- 'gET20NwfabricHostsHostID' route
    :<|> "2.0" :> "nwfabric" :> "status" :> QueryParam "resource" Text :> Verb 'GET 200 '[JSON] () -- 'gET20NwfabricStatus' route
    :<|> "2.0" :> "nwfabric" :> "status" :> "alleligible" :> Capture "resourceType" Text :> Verb 'GET 200 '[JSON] () -- 'gET20NwfabricStatusAlleligibleResourceType' route
    :<|> "2.0" :> "nwfabric" :> "status" :> "child" :> Capture "parentResourceID" Text :> Verb 'GET 200 '[JSON] () -- 'gET20NwfabricStatusChildParentResourceID' route
    :<|> "2.0" :> "services" :> "alarms" :> Capture "sourceId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesAlarmsSourceId' route
    :<|> "2.0" :> "services" :> "application" :> Capture "applicationId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesApplicationApplicationId' route
    :<|> "2.0" :> "services" :> "application" :> "scope" :> Capture "scopeId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesApplicationScopeScopeId' route
    :<|> "2.0" :> "services" :> "applicationgroup" :> Capture "applicationgroupId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesApplicationgroupApplicationgroupId' route
    :<|> "2.0" :> "services" :> "applicationgroup" :> "scope" :> Capture "scopeId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesApplicationgroupScopeScopeId' route
    :<|> "2.0" :> "services" :> "applicationgroup" :> "scope" :> Capture "scopeId" Text :> "members" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesApplicationgroupScopeScopeIdMembers' route
    :<|> "2.0" :> "services" :> "auth" :> "tokenexpiration" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesAuthTokenexpiration' route
    :<|> "2.0" :> "services" :> "configuration" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesConfiguration' route
    :<|> "2.0" :> "services" :> "dashboard" :> "ui-views" :> "dashboard" :> "widgetconfigurations" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesDashboardUiViewsDashboardWidgetconfigurations' route
    :<|> "2.0" :> "services" :> "dashboard" :> "ui-views" :> "dashboard" :> "widgetconfigurations" :> Capture "widgetconfigurationId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesDashboardUiViewsDashboardWidgetconfigurationsWidgetconfigurationId' route
    :<|> "2.0" :> "services" :> "housekeeping" :> "management" :> "index_maintenance" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesHousekeepingManagementIndexMaintenance' route
    :<|> "2.0" :> "services" :> "ipam" :> "pools" :> Capture "poolId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesIpamPoolsPoolId' route
    :<|> "2.0" :> "services" :> "ipam" :> "pools" :> Capture "poolId" Text :> "ipaddresses" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesIpamPoolsPoolIdIpaddresses' route
    :<|> "2.0" :> "services" :> "ipam" :> "pools" :> "scope" :> Capture "scopeId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesIpamPoolsScopeScopeId' route
    :<|> "2.0" :> "services" :> "ipset" :> Capture "ipsetId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesIpsetIpsetId' route
    :<|> "2.0" :> "services" :> "ipset" :> "scope" :> Capture "scopeMoref" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesIpsetScopeScopeMoref' route
    :<|> "2.0" :> "services" :> "licensing" :> "capacityusage" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesLicensingCapacityusage' route
    :<|> "2.0" :> "services" :> "licensing" :> "status" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesLicensingStatus' route
    :<|> "2.0" :> "services" :> "macset" :> Capture "macsetId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesMacsetMacsetId' route
    :<|> "2.0" :> "services" :> "macset" :> "scope" :> Capture "scopeId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesMacsetScopeScopeId' route
    :<|> "2.0" :> "services" :> "policy" :> "securityaction" :> Capture "category" Text :> "virtualmachines" :> QueryParam "attributeKey" Text :> QueryParam "attributeValue" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicySecurityactionCategoryVirtualmachines' route
    :<|> "2.0" :> "services" :> "policy" :> "securitygroup" :> Capture "ID" Text :> "securityactions" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicySecuritygroupIDSecurityactions' route
    :<|> "2.0" :> "services" :> "policy" :> "securitygroup" :> Capture "ID" Text :> "securitypolicies" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicySecuritygroupIDSecuritypolicies' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> "alarms" :> "all" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicySecuritypolicyAlarmsAll' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> "all" :> QueryParam "startIndex" Text :> QueryParam "pageSize" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicySecuritypolicyAll' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> "hierarchy" :> QueryParam "policyIds" Text :> QueryParam "prefix" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicySecuritypolicyHierarchy' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> Capture "ID" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicySecuritypolicyID' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> Capture "ID" Text :> "securityactions" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicySecuritypolicyIDSecurityactions' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> "maxprecedence" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicySecuritypolicyMaxprecedence' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> "serviceprovider" :> "firewall" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicySecuritypolicyServiceproviderFirewall' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> "status" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicySecuritypolicyStatus' route
    :<|> "2.0" :> "services" :> "policy" :> "serviceprovider" :> "firewall" :> ReqBody '[JSON] Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicyServiceproviderFirewall' route
    :<|> "2.0" :> "services" :> "policy" :> "serviceprovider" :> "firewall" :> "info" :> QueryParam "key" Text :> QueryParam "value" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicyServiceproviderFirewallInfo' route
    :<|> "2.0" :> "services" :> "policy" :> "virtualmachine" :> Capture "ID" Text :> "securityactions" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesPolicyVirtualmachineIDSecurityactions' route
    :<|> "2.0" :> "services" :> "securitygroup" :> "internal" :> "scope" :> Capture "scopeId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritygroupInternalScopeScopeId' route
    :<|> "2.0" :> "services" :> "securitygroup" :> "lookup" :> "ipaddress" :> Capture "ipAddress" Text :> ReqBody '[JSON] Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritygroupLookupIpaddressIpAddress' route
    :<|> "2.0" :> "services" :> "securitygroup" :> "lookup" :> "virtualmachine" :> Capture "virtualMachineId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritygroupLookupVirtualmachineVirtualMachineId' route
    :<|> "2.0" :> "services" :> "securitygroup" :> Capture "objectId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritygroupObjectId' route
    :<|> "2.0" :> "services" :> "securitygroup" :> Capture "objectId" Text :> "translation" :> "ipaddresses" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritygroupObjectIdTranslationIpaddresses' route
    :<|> "2.0" :> "services" :> "securitygroup" :> Capture "objectId" Text :> "translation" :> "macaddresses" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritygroupObjectIdTranslationMacaddresses' route
    :<|> "2.0" :> "services" :> "securitygroup" :> Capture "objectId" Text :> "translation" :> "virtualmachines" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritygroupObjectIdTranslationVirtualmachines' route
    :<|> "2.0" :> "services" :> "securitygroup" :> Capture "objectId" Text :> "translation" :> "vnics" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritygroupObjectIdTranslationVnics' route
    :<|> "2.0" :> "services" :> "securitygroup" :> "scope" :> Capture "scopeId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritygroupScopeScopeId' route
    :<|> "2.0" :> "services" :> "securitygroup" :> "scope" :> Capture "scopeId" Text :> "memberTypes" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritygroupScopeScopeIdMemberTypes' route
    :<|> "2.0" :> "services" :> "securitygroup" :> "scope" :> Capture "scopeId" Text :> "members" :> Capture "memberType" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritygroupScopeScopeIdMembersMemberType' route
    :<|> "2.0" :> "services" :> "securitytags" :> "selection-criteria" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritytagsSelectionCriteria' route
    :<|> "2.0" :> "services" :> "securitytags" :> "tag" :> QueryParam "isUniversal" Bool :> QueryParam "startIndex" Text :> QueryParam "pageSize" Text :> QueryParam "sortOrderAscending" Text :> QueryParam "sortBy" Text :> QueryParam "filterBy" Text :> QueryParam "filtervalue" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritytagsTag' route
    :<|> "2.0" :> "services" :> "securitytags" :> "tag" :> Capture "tagId" Text :> "vm" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritytagsTagTagIdVm' route
    :<|> "2.0" :> "services" :> "securitytags" :> "tag" :> Capture "tagId" Text :> "vmDetail" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritytagsTagTagIdVmDetail' route
    :<|> "2.0" :> "services" :> "securitytags" :> "vm" :> Capture "vmId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSecuritytagsVmVmId' route
    :<|> "2.0" :> "services" :> "snmp" :> "manager" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSnmpManager' route
    :<|> "2.0" :> "services" :> "snmp" :> "manager" :> Capture "managerId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSnmpManagerManagerId' route
    :<|> "2.0" :> "services" :> "snmp" :> "status" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSnmpStatus' route
    :<|> "2.0" :> "services" :> "snmp" :> "trap" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSnmpTrap' route
    :<|> "2.0" :> "services" :> "snmp" :> "trap" :> Capture "oid" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSnmpTrapOid' route
    :<|> "2.0" :> "services" :> "ssoconfig" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSsoconfig' route
    :<|> "2.0" :> "services" :> "ssoconfig" :> "status" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSsoconfigStatus' route
    :<|> "2.0" :> "services" :> "systemalarms" :> QueryParam "sortBy" Text :> QueryParam "pageSize" Int :> QueryParam "sortOrderAscending" Bool :> QueryParam "startIndex" Int :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSystemalarms' route
    :<|> "2.0" :> "services" :> "systemalarms" :> Capture "alarmId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesSystemalarmsAlarmId' route
    :<|> "2.0" :> "services" :> "taskservice" :> "job" :> QueryParam "startIndex" Int :> QueryParam "pageSize" Int :> QueryParam "sortBy" Text :> QueryParam "sortOrderAscending" Bool :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesTaskserviceJob' route
    :<|> "2.0" :> "services" :> "taskservice" :> "job" :> Capture "jobId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesTaskserviceJobJobId' route
    :<|> "2.0" :> "services" :> "translation" :> "virtualmachine" :> Capture "vmId" Text :> "ipaddresses" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesTranslationVirtualmachineVmIdIpaddresses' route
    :<|> "2.0" :> "services" :> "truststore" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesTruststoreConfig' route
    :<|> "2.0" :> "services" :> "truststore" :> "config" :> Capture "certificateId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesTruststoreConfigCertificateId' route
    :<|> "2.0" :> "services" :> "truststore" :> "config" :> "scope" :> Capture "scopeId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesTruststoreConfigScopeScopeId' route
    :<|> "2.0" :> "services" :> "truststore" :> "crl" :> Capture "crlId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesTruststoreCrlCrlId' route
    :<|> "2.0" :> "services" :> "truststore" :> "crl" :> "scope" :> Capture "scopeId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesTruststoreCrlScopeScopeId' route
    :<|> "2.0" :> "services" :> "truststore" :> "csr" :> Capture "csrId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesTruststoreCsrCsrId' route
    :<|> "2.0" :> "services" :> "truststore" :> "csr" :> "scope" :> Capture "scopeId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesTruststoreCsrScopeScopeId' route
    :<|> "2.0" :> "services" :> "usermgmt" :> "role" :> Capture "userId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesUsermgmtRoleUserId' route
    :<|> "2.0" :> "services" :> "usermgmt" :> "roles" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesUsermgmtRoles' route
    :<|> "2.0" :> "services" :> "usermgmt" :> "scopingobjects" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesUsermgmtScopingobjects' route
    :<|> "2.0" :> "services" :> "usermgmt" :> "user" :> Capture "userId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesUsermgmtUserUserId' route
    :<|> "2.0" :> "services" :> "usermgmt" :> "users" :> "vsm" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesUsermgmtUsersVsm' route
    :<|> "2.0" :> "services" :> "vcconfig" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesVcconfig' route
    :<|> "2.0" :> "services" :> "vcconfig" :> "status" :> Verb 'GET 200 '[JSON] () -- 'gET20ServicesVcconfigStatus' route
    :<|> "2.0" :> "si" :> "agent" :> Capture "agentID" Text :> Verb 'GET 200 '[JSON] () -- 'gET20SiAgentAgentID' route
    :<|> "2.0" :> "si" :> "deploy" :> "cluster" :> Capture "clusterID" Text :> Verb 'GET 200 '[JSON] () -- 'gET20SiDeployClusterClusterID' route
    :<|> "2.0" :> "si" :> "deploy" :> "cluster" :> Capture "clusterID" Text :> "service" :> Capture "serviceID" Text :> Verb 'GET 200 '[JSON] () -- 'gET20SiDeployClusterClusterIDServiceServiceID' route
    :<|> "2.0" :> "si" :> "deploy" :> "service" :> Capture "serviceID" Text :> Verb 'GET 200 '[JSON] () -- 'gET20SiDeployServiceServiceID' route
    :<|> "2.0" :> "si" :> "deploy" :> "service" :> Capture "serviceID" Text :> "dependsOn" :> Verb 'GET 200 '[JSON] () -- 'gET20SiDeployServiceServiceIDDependsOn' route
    :<|> "2.0" :> "si" :> "deployment" :> Capture "deploymentunitID" Text :> "agents" :> Verb 'GET 200 '[JSON] () -- 'gET20SiDeploymentDeploymentunitIDAgents' route
    :<|> "2.0" :> "si" :> "fabric" :> "sync" :> "conflicts" :> Verb 'GET 200 '[JSON] () -- 'gET20SiFabricSyncConflicts' route
    :<|> "2.0" :> "si" :> "host" :> Capture "hostID" Text :> "agents" :> Verb 'GET 200 '[JSON] () -- 'gET20SiHostHostIDAgents' route
    :<|> "2.0" :> "system-monitor" :> "cpuusage" :> "details" :> Verb 'GET 200 '[JSON] () -- 'gET20SystemMonitorCpuusageDetails' route
    :<|> "2.0" :> "system-monitor" :> "cpuusage" :> "indicator" :> Verb 'GET 200 '[JSON] () -- 'gET20SystemMonitorCpuusageIndicator' route
    :<|> "2.0" :> "systemevent" :> QueryParam "startIndex" Text :> QueryParam "pageSize" Text :> ReqBody '[JSON] Text :> Verb 'GET 200 '[JSON] () -- 'gET20Systemevent' route
    :<|> "2.0" :> "techsupportbundle" :> Capture "filename" Text :> Verb 'GET 200 '[JSON] () -- 'gET20TechsupportbundleFilename' route
    :<|> "2.0" :> "techsupportbundle" :> "status" :> ReqBody '[JSON] Text :> Verb 'GET 200 '[JSON] () -- 'gET20TechsupportbundleStatus' route
    :<|> "2.0" :> "universalsync" :> "configuration" :> "nsxmanagers" :> Verb 'GET 200 '[JSON] () -- 'gET20UniversalsyncConfigurationNsxmanagers' route
    :<|> "2.0" :> "universalsync" :> "configuration" :> "nsxmanagers" :> Capture "nsxManagerID" Text :> Verb 'GET 200 '[JSON] () -- 'gET20UniversalsyncConfigurationNsxmanagersNsxManagerID' route
    :<|> "2.0" :> "universalsync" :> "configuration" :> "role" :> Verb 'GET 200 '[JSON] () -- 'gET20UniversalsyncConfigurationRole' route
    :<|> "2.0" :> "universalsync" :> "entitystatus" :> QueryParam "objectType" Text :> QueryParam "objectId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20UniversalsyncEntitystatus' route
    :<|> "2.0" :> "universalsync" :> "status" :> Verb 'GET 200 '[JSON] () -- 'gET20UniversalsyncStatus' route
    :<|> "2.0" :> "vdn" :> "bfd" :> "configuration" :> "global" :> ReqBody '[JSON] Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnBfdConfigurationGlobal' route
    :<|> "2.0" :> "vdn" :> "cdo" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnCdo' route
    :<|> "2.0" :> "vdn" :> "config" :> "multicasts" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnConfigMulticasts' route
    :<|> "2.0" :> "vdn" :> "config" :> "multicasts" :> Capture "multicastAddresssRangeId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnConfigMulticastsMulticastAddresssRangeId' route
    :<|> "2.0" :> "vdn" :> "config" :> "resources" :> "allocated" :> QueryParam "type" Text :> QueryParam "pagesize" Text :> QueryParam "startindex" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnConfigResourcesAllocated' route
    :<|> "2.0" :> "vdn" :> "config" :> "segments" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnConfigSegments' route
    :<|> "2.0" :> "vdn" :> "config" :> "segments" :> Capture "segmentPoolId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnConfigSegmentsSegmentPoolId' route
    :<|> "2.0" :> "vdn" :> "config" :> "vxlan" :> "udp" :> "port" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnConfigVxlanUdpPort' route
    :<|> "2.0" :> "vdn" :> "config" :> "vxlan" :> "udp" :> "port" :> "taskStatus" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnConfigVxlanUdpPortTaskStatus' route
    :<|> "2.0" :> "vdn" :> "controller" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnController' route
    :<|> "2.0" :> "vdn" :> "controller" :> "cluster" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnControllerCluster' route
    :<|> "2.0" :> "vdn" :> "controller" :> "cluster" :> "ntp" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnControllerClusterNtp' route
    :<|> "2.0" :> "vdn" :> "controller" :> Capture "controllerId" Text :> "snapshot" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnControllerControllerIdSnapshot' route
    :<|> "2.0" :> "vdn" :> "controller" :> Capture "controllerId" Text :> "syslog" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnControllerControllerIdSyslog' route
    :<|> "2.0" :> "vdn" :> "controller" :> Capture "controllerId" Text :> "systemStats" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnControllerControllerIdSystemStats' route
    :<|> "2.0" :> "vdn" :> "controller" :> Capture "controllerId" Text :> "techsupportlogs" :> Header "Content-type" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnControllerControllerIdTechsupportlogs' route
    :<|> "2.0" :> "vdn" :> "controller" :> "progress" :> Capture "jobId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnControllerProgressJobId' route
    :<|> "2.0" :> "vdn" :> "controller" :> "synchronize" :> "status" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnControllerSynchronizeStatus' route
    :<|> "2.0" :> "vdn" :> "controller" :> "upgrade-available" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnControllerUpgradeAvailable' route
    :<|> "2.0" :> "vdn" :> "hardwaregateway" :> "bfd" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHardwaregatewayBfdConfig' route
    :<|> "2.0" :> "vdn" :> "hardwaregateway" :> "bfd" :> "status" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHardwaregatewayBfdStatus' route
    :<|> "2.0" :> "vdn" :> "hardwaregateway" :> "bindings" :> QueryParam "hardwareGatewayId" Text :> QueryParam "vni" Int :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHardwaregatewayBindings' route
    :<|> "2.0" :> "vdn" :> "hardwaregateway" :> "bindings" :> Capture "bindingId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHardwaregatewayBindingsBindingId' route
    :<|> "2.0" :> "vdn" :> "hardwaregateway" :> "bindings" :> Capture "bindingId" Text :> "statistic" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHardwaregatewayBindingsBindingIdStatistic' route
    :<|> "2.0" :> "vdn" :> "hardwaregateways" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHardwaregateways' route
    :<|> "2.0" :> "vdn" :> "hardwaregateways" :> Capture "hardwareGatewayId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHardwaregatewaysHardwareGatewayId' route
    :<|> "2.0" :> "vdn" :> "hardwaregateways" :> Capture "hardwareGatewayId" Text :> "switches" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHardwaregatewaysHardwareGatewayIdSwitches' route
    :<|> "2.0" :> "vdn" :> "hardwaregateways" :> Capture "hardwareGatewayId" Text :> "switches" :> Capture "switchName" Text :> "switchports" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHardwaregatewaysHardwareGatewayIdSwitchesSwitchNameSwitchports' route
    :<|> "2.0" :> "vdn" :> "hardwaregateways" :> "replicationcluster" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHardwaregatewaysReplicationcluster' route
    :<|> "2.0" :> "vdn" :> "host" :> Capture "hostId" Text :> "remote-host-status" :> QueryParam "source" Text :> QueryParam "tunnel_status" Text :> QueryParam "bfd_diagnostic_code" Text :> ReqBody '[JSON] Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHostHostIdRemoteHostStatus' route
    :<|> "2.0" :> "vdn" :> "host" :> Capture "hostId" Text :> "status" :> QueryParam "source" Text :> QueryParam "status" Text :> QueryParam "host_id" Text :> ReqBody '[JSON] Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHostHostIdStatus' route
    :<|> "2.0" :> "vdn" :> "host" :> Capture "hostId" Text :> "tunnel" :> ReqBody '[JSON] Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHostHostIdTunnel' route
    :<|> "2.0" :> "vdn" :> "host" :> "status" :> QueryParam "source" Text :> QueryParam "status" Text :> QueryParam "host_id" Text :> ReqBody '[JSON] Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnHostStatus' route
    :<|> "2.0" :> "vdn" :> "inventory" :> "host" :> Capture "hostId" Text :> "connection" :> "status" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnInventoryHostHostIdConnectionStatus' route
    :<|> "2.0" :> "vdn" :> "inventory" :> "hosts" :> "connection" :> "status" :> QueryParam "hostId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnInventoryHostsConnectionStatus' route
    :<|> "2.0" :> "vdn" :> "pnic-check" :> "configuration" :> "global" :> ReqBody '[JSON] Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnPnicCheckConfigurationGlobal' route
    :<|> "2.0" :> "vdn" :> "scopes" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnScopes' route
    :<|> "2.0" :> "vdn" :> "scopes" :> Capture "scopeId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnScopesScopeId' route
    :<|> "2.0" :> "vdn" :> "scopes" :> Capture "scopeId" Text :> "virtualwires" :> QueryParam "startindex" Text :> QueryParam "pagesize" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnScopesScopeIdVirtualwires' route
    :<|> "2.0" :> "vdn" :> "switches" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnSwitches' route
    :<|> "2.0" :> "vdn" :> "switches" :> "datacenter" :> Capture "datacenterID" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnSwitchesDatacenterDatacenterID' route
    :<|> "2.0" :> "vdn" :> "switches" :> Capture "vdsId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnSwitchesVdsId' route
    :<|> "2.0" :> "vdn" :> "traceflow" :> Capture "traceflowId" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnTraceflowTraceflowId' route
    :<|> "2.0" :> "vdn" :> "traceflow" :> Capture "traceflowId" Text :> "observations" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnTraceflowTraceflowIdObservations' route
    :<|> "2.0" :> "vdn" :> "virtualwires" :> QueryParam "startindex" Text :> QueryParam "pagesize" Text :> QueryParam "name" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnVirtualwires' route
    :<|> "2.0" :> "vdn" :> "virtualwires" :> Capture "virtualWireID" Text :> Verb 'GET 200 '[JSON] () -- 'gET20VdnVirtualwiresVirtualWireID' route
    :<|> "2.0" :> "vdn" :> "virtualwires" :> Capture "virtualWireID" Text :> "hardwaregateways" :> Verb 'GET 200 '[JSON] () -- 'gET20VdnVirtualwiresVirtualWireIDHardwaregateways' route
    :<|> "2.0" :> "xvs" :> "networks" :> Capture "ID" Text :> "features" :> Verb 'GET 200 '[JSON] () -- 'gET20XvsNetworksIDFeatures' route
    :<|> "2.1" :> "app" :> "excludelist" :> QueryParam "listSystemResources" Bool :> Verb 'GET 200 '[JSON] () -- 'gET21AppExcludelist' route
    :<|> "2.1" :> "app" :> "flow" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET21AppFlowConfig' route
    :<|> "2.1" :> "app" :> "flow" :> "flowstats" :> QueryParam "contextId" Text :> QueryParam "flowType" Text :> QueryParam "startTime" Text :> QueryParam "endTime" Text :> QueryParam "startIndex" Int :> QueryParam "pageSize" Int :> Verb 'GET 200 '[JSON] () -- 'gET21AppFlowFlowstats' route
    :<|> "2.1" :> "app" :> "flow" :> "flowstats" :> "info" :> Verb 'GET 200 '[JSON] () -- 'gET21AppFlowFlowstatsInfo' route
    :<|> "3.0" :> "ai" :> "app" :> Verb 'GET 200 '[JSON] () -- 'gET30AiApp' route
    :<|> "3.0" :> "ai" :> "app" :> Capture "appID" Text :> Verb 'GET 200 '[JSON] () -- 'gET30AiAppAppID' route
    :<|> "3.0" :> "ai" :> "desktoppool" :> Verb 'GET 200 '[JSON] () -- 'gET30AiDesktoppool' route
    :<|> "3.0" :> "ai" :> "desktoppool" :> Capture "desktoppoolID" Text :> Verb 'GET 200 '[JSON] () -- 'gET30AiDesktoppoolDesktoppoolID' route
    :<|> "3.0" :> "ai" :> "directorygroup" :> Verb 'GET 200 '[JSON] () -- 'gET30AiDirectorygroup' route
    :<|> "3.0" :> "ai" :> "directorygroup" :> Capture "directorygroupID" Text :> Verb 'GET 200 '[JSON] () -- 'gET30AiDirectorygroupDirectorygroupID' route
    :<|> "3.0" :> "ai" :> "directorygroup" :> "user" :> Capture "userID" Text :> Verb 'GET 200 '[JSON] () -- 'gET30AiDirectorygroupUserUserID' route
    :<|> "3.0" :> "ai" :> "host" :> Verb 'GET 200 '[JSON] () -- 'gET30AiHost' route
    :<|> "3.0" :> "ai" :> "host" :> Capture "hostID" Text :> Verb 'GET 200 '[JSON] () -- 'gET30AiHostHostID' route
    :<|> "3.0" :> "ai" :> "records" :> QueryParam "query" Text :> QueryParam "interval" Text :> QueryParam "stime" Text :> QueryParam "etime" Text :> QueryParam "param" Text :> QueryParam "pagesize" Text :> QueryParam "startindex" Text :> Verb 'GET 200 '[JSON] () -- 'gET30AiRecords' route
    :<|> "3.0" :> "ai" :> "securitygroup" :> Verb 'GET 200 '[JSON] () -- 'gET30AiSecuritygroup' route
    :<|> "3.0" :> "ai" :> "securitygroup" :> Capture "secgroupID" Text :> Verb 'GET 200 '[JSON] () -- 'gET30AiSecuritygroupSecgroupID' route
    :<|> "3.0" :> "ai" :> "user" :> Capture "userID" Text :> Verb 'GET 200 '[JSON] () -- 'gET30AiUserUserID' route
    :<|> "3.0" :> "ai" :> "userdetails" :> QueryParam "query" Text :> QueryParam "interval" Text :> QueryParam "stime" Text :> QueryParam "etime" Text :> QueryParam "param" Text :> QueryParam "pagesize" Text :> QueryParam "startindex" Text :> Verb 'GET 200 '[JSON] () -- 'gET30AiUserdetails' route
    :<|> "3.0" :> "ai" :> "vm" :> Verb 'GET 200 '[JSON] () -- 'gET30AiVm' route
    :<|> "3.0" :> "ai" :> "vm" :> Capture "vmID" Text :> Verb 'GET 200 '[JSON] () -- 'gET30AiVmVmID' route
    :<|> "4.0" :> "edgePublish" :> "tuningConfiguration" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgePublishTuningConfiguration' route
    :<|> "4.0" :> "edges" :> QueryParam "datacenter" Text :> QueryParam "tenant" Text :> QueryParam "pg" Text :> Verb 'GET 200 '[JSON] () -- 'gET40Edges' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> QueryParam "isUniversal" Bool :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeId' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "appliances" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdAppliances' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "appliances" :> Capture "haIndex" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdAppliancesHaIndex' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "autoconfiguration" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdAutoconfiguration' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "bridging" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdBridgingConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dhcp" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdDhcpConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dhcp" :> "config" :> "bindings" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdDhcpConfigBindings' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dhcp" :> "config" :> "bindings" :> Capture "bindingID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdDhcpConfigBindingsBindingID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dhcp" :> "config" :> "relay" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdDhcpConfigRelay' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dhcp" :> "leaseInfo" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdDhcpLeaseInfo' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dns" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdDnsConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dns" :> "statistics" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdDnsStatistics' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "firewall" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdFirewallConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "firewall" :> "config" :> "defaultpolicy" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdFirewallConfigDefaultpolicy' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "firewall" :> "config" :> "global" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdFirewallConfigGlobal' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "firewall" :> "config" :> "rules" :> Capture "ruleId" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdFirewallConfigRulesRuleId' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "firewall" :> "statistics" :> Capture "ruleId" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdFirewallStatisticsRuleId' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "healthsummary" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdHealthsummary' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "highavailability" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdHighavailabilityConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "interfaces" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdInterfaces' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "interfaces" :> Capture "index" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdInterfacesIndex' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "ipsec" :> "config" :> QueryParam "showSensitiveData" Bool :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdIpsecConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "ipsec" :> "statistics" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdIpsecStatistics' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "l2vpn" :> "config" :> QueryParam "showSensitiveData" Bool :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdL2vpnConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "l2vpn" :> "config" :> "statistics" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdL2vpnConfigStatistics' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdLoadbalancerConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "applicationprofiles" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdLoadbalancerConfigApplicationprofiles' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "applicationprofiles" :> Capture "appProfileID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdLoadbalancerConfigApplicationprofilesAppProfileID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "applicationrules" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdLoadbalancerConfigApplicationrules' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "applicationrules" :> Capture "appruleID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdLoadbalancerConfigApplicationrulesAppruleID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "monitors" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdLoadbalancerConfigMonitors' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "monitors" :> Capture "monitorID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdLoadbalancerConfigMonitorsMonitorID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "pools" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdLoadbalancerConfigPools' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "pools" :> Capture "poolID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdLoadbalancerConfigPoolsPoolID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "virtualservers" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdLoadbalancerConfigVirtualservers' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "virtualservers" :> Capture "virtualserverID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdLoadbalancerConfigVirtualserversVirtualserverID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "statistics" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdLoadbalancerStatistics' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "mgmtinterface" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdMgmtinterface' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "nat" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdNatConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> QueryParam "le" Double :> QueryParam "ge" Double :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdRoutingConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> "bgp" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdRoutingConfigBgp' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> "global" :> QueryParam "le" Double :> QueryParam "ge" Double :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdRoutingConfigGlobal' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> "ospf" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdRoutingConfigOspf' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> "static" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdRoutingConfigStatic' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "activesessions" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnActivesessions' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "advancedconfig" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigAdvancedconfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "auth" :> "localserver" :> "users" :> Capture "userID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigAuthLocalserverUsersUserID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "auth" :> "settings" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigAuthSettings' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "clientconfig" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionClientconfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "installpackages" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "installpackages" :> Capture "packageID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackagesPackageID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "ippools" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "ippools" :> Capture "ippoolID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppoolsIppoolID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "privatenetworks" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "privatenetworks" :> Capture "networkID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworksNetworkID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "layout" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigLayout' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "script" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigScript' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "script" :> Capture "fileID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigScriptFileID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "server" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSslvpnConfigServer' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "statistics" :> "dashboard" :> "firewall" :> QueryParam "interval" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdStatisticsDashboardFirewall' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "statistics" :> "dashboard" :> "interface" :> QueryParam "interval" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdStatisticsDashboardInterface' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "statistics" :> "dashboard" :> "ipsec" :> QueryParam "interval" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdStatisticsDashboardIpsec' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "statistics" :> "dashboard" :> "sslvpn" :> QueryParam "interval" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdStatisticsDashboardSslvpn' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "statistics" :> "interfaces" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdStatisticsInterfaces' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "statistics" :> "interfaces" :> "internal" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdStatisticsInterfacesInternal' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "statistics" :> "interfaces" :> "uplink" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdStatisticsInterfacesUplink' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "status" :> QueryParam "getlatest" Bool :> QueryParam "detailed" Bool :> QueryParam "preRulesStatus" Bool :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdStatus' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "summary" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSummary' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "syslog" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSyslogConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "systemcontrol" :> "config" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdSystemcontrolConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "techsupportlogs" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdTechsupportlogs' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "tunnels" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdTunnels' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "tunnels" :> Capture "tunnelId" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdTunnelsTunnelId' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "vnics" :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdVnics' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "vnics" :> Capture "index" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdVnicsIndex' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "vnics" :> Capture "parentVnicIndex" Text :> "subinterfaces" :> Capture "subInterfaceIndex" Text :> ReqBody '[JSON] Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesEdgeIdVnicsParentVnicIndexSubinterfacesSubInterfaceIndex' route
    :<|> "4.0" :> "edges" :> "jobs" :> QueryParam "status" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesJobs' route
    :<|> "4.0" :> "edges" :> "jobs" :> Capture "jobId" Text :> Verb 'GET 200 '[JSON] () -- 'gET40EdgesJobsJobId' route
    :<|> "4.0" :> "firewall" :> "config" :> "globalconfiguration" :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallConfigGlobalconfiguration' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> QueryParam "ruleType" Text :> QueryParam "source" Text :> QueryParam "destination" Text :> QueryParam "ruleId" Text :> QueryParam "comment" Text :> QueryParam "name" Text :> QueryParam "siProfile" Text :> QueryParam "edgeId" Text :> QueryParam "action" Text :> QueryParam "sectionName" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0Config' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "ipfix" :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0ConfigIpfix' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer2sections" :> QueryParam "name" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0ConfigLayer2sections' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer2sections" :> Capture "sectionId" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0ConfigLayer2sectionsSectionId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer2sections" :> Capture "sectionId" Text :> "rules" :> Capture "ruleId" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRulesRuleId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3redirect" :> "profiles" :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0ConfigLayer3redirectProfiles' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3redirectsections" :> Capture "section" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0ConfigLayer3redirectsectionsSection' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3redirectsections" :> Capture "section" Text :> "rules" :> Capture "ruleID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRulesRuleID' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3sections" :> QueryParam "name" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0ConfigLayer3sections' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3sections" :> Capture "sectionId" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0ConfigLayer3sectionsSectionId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3sections" :> Capture "sectionId" Text :> "rules" :> Capture "ruleId" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRulesRuleId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "defaultconfig" :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0Defaultconfig' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "drafts" :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0Drafts' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "drafts" :> Capture "draftID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0DraftsDraftID' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "drafts" :> Capture "draftID" Text :> "action" :> "export" :> QueryParam "getLatestForUniversal" Bool :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0DraftsDraftIDActionExport' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "state" :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0State' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "status" :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0Status' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "status" :> "layer2sections" :> Capture "sectionID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0StatusLayer2sectionsSectionID' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "status" :> "layer3sections" :> Capture "sectionID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0StatusLayer3sectionsSectionID' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "timeouts" :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0Timeouts' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "timeouts" :> Capture "configId" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallGlobalroot0TimeoutsConfigId' route
    :<|> "4.0" :> "firewall" :> "stats" :> "eventthresholds" :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallStatsEventthresholds' route
    :<|> "4.0" :> "firewall" :> "stats" :> "thresholds" :> "host" :> Capture "hostId" Text :> QueryParam "type" Text :> QueryParam "thresholdValue" Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallStatsThresholdsHostHostId' route
    :<|> "4.0" :> "firewall" :> "stats" :> "thresholds" :> "types" :> ReqBody '[JSON] Text :> Verb 'GET 200 '[JSON] () -- 'gET40FirewallStatsThresholdsTypes' route
    :<|> "4.0" :> "services" :> "spoofguard" :> "policies" :> Verb 'GET 200 '[JSON] () -- 'gET40ServicesSpoofguardPolicies' route
    :<|> "4.0" :> "services" :> "spoofguard" :> "policies" :> Capture "policyID" Text :> Verb 'GET 200 '[JSON] () -- 'gET40ServicesSpoofguardPoliciesPolicyID' route
    :<|> "4.0" :> "services" :> "spoofguard" :> Capture "policyID" Text :> QueryParam "list" Text :> Verb 'GET 200 '[JSON] () -- 'gET40ServicesSpoofguardPolicyID' route
    :<|> "1.0" :> "appliance-management" :> "backuprestore" :> "backup" :> Header "Content-Type" Text :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementBackuprestoreBackup' route
    :<|> "1.0" :> "appliance-management" :> "backuprestore" :> "restore" :> QueryParam "restoreFile" Text :> QueryParam "forceRestore" Bool :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementBackuprestoreRestore' route
    :<|> "1.0" :> "appliance-management" :> "certificatemanager" :> "csr" :> "nsx" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementCertificatemanagerCsrNsx' route
    :<|> "1.0" :> "appliance-management" :> "certificatemanager" :> "pkcs12keystore" :> "nsx" :> QueryParam "password" Text :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementCertificatemanagerPkcs12keystoreNsx' route
    :<|> "1.0" :> "appliance-management" :> "certificatemanager" :> "uploadchain" :> "nsx" :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementCertificatemanagerUploadchainNsx' route
    :<|> "1.0" :> "appliance-management" :> "components" :> "component" :> "APPMGMT" :> "restart" :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementComponentsComponentAPPMGMTRestart' route
    :<|> "1.0" :> "appliance-management" :> "components" :> "component" :> Capture "componentID" Text :> "toggleStatus" :> Capture "command" Text :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementComponentsComponentComponentIDToggleStatusCommand' route
    :<|> "1.0" :> "appliance-management" :> "notifications" :> Capture "ID" Text :> "acknowledge" :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementNotificationsIDAcknowledge' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "restart" :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementSystemRestart' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "securitysettings" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementSystemSecuritysettings' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "tlssettings" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementSystemTlssettings' route
    :<|> "1.0" :> "appliance-management" :> "techsupportlogs" :> Capture "componentID" Text :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementTechsupportlogsComponentID' route
    :<|> "1.0" :> "appliance-management" :> "upgrade" :> "start" :> Capture "componentID" Text :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementUpgradeStartComponentID' route
    :<|> "1.0" :> "appliance-management" :> "upgrade" :> "uploadbundle" :> Capture "componentID" Text :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementUpgradeUploadbundleComponentID' route
    :<|> "1.0" :> "appliance-management" :> "upgrade" :> "uploadbundlefromurl" :> Verb 'POST 200 '[JSON] () -- 'pOST10ApplianceManagementUpgradeUploadbundlefromurl' route
    :<|> "1.0" :> "directory" :> "ldapSyncSettings" :> QueryParam "deltaSyncIntervalInMin" Text :> QueryParam "fullSyncCronExpr" Text :> QueryParam "enableFullSync" Text :> QueryParam "restartSync" Text :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST10DirectoryLdapSyncSettings' route
    :<|> "1.0" :> "directory" :> "updateDomain" :> ReqBody '[JSON] DomainCreate :> Verb 'POST 200 '[JSON] () -- 'pOST10DirectoryUpdateDomain' route
    :<|> "1.0" :> "directory" :> "updateEventLogServer" :> ReqBody '[JSON] ELogServerCreate :> Verb 'POST 200 '[JSON] () -- 'pOST10DirectoryUpdateEventLogServer' route
    :<|> "1.0" :> "directory" :> "updateLdapServer" :> ReqBody '[JSON] LdapServerCreate :> Verb 'POST 200 '[JSON] () -- 'pOST10DirectoryUpdateLdapServer' route
    :<|> "1.0" :> "directory" :> "verifyRootDn" :> QueryParam "domainName" Text :> QueryParam "baseDN" Text :> QueryParam "rootDnlists" Text :> QueryParam "hostName" Text :> QueryParam "port" Text :> QueryParam "username" Text :> QueryParam "password" Text :> QueryParam "protocol" Text :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST10DirectoryVerifyRootDn' route
    :<|> "1.0" :> "eventcontrol" :> "eventcontrol-root" :> "request" :> ReqBody '[JSON] DataCollectionKillSwitchToggle :> Verb 'POST 200 '[JSON] () -- 'pOST10EventcontrolEventcontrolRootRequest' route
    :<|> "1.0" :> "eventcontrol" :> "vm" :> Capture "vmID" Text :> "request" :> ReqBody '[JSON] DataCollectionVMCreate :> Verb 'POST 200 '[JSON] () -- 'pOST10EventcontrolVmVmIDRequest' route
    :<|> "1.0" :> "identity" :> "staticUserMapping" :> Capture "userID" Text :> Capture "IP" Text :> Verb 'POST 200 '[JSON] () -- 'pOST10IdentityStaticUserMappingUserIDIP' route
    :<|> "1.0" :> "nsx" :> "cli" :> QueryParam "action" Text :> ReqBody '[JSON] NsxCliExecute :> Header "Accept" Text :> Verb 'POST 200 '[JSON] () -- 'pOST10NsxCli' route
    :<|> "1.0" :> "sam" :> "syslog" :> "disable" :> Verb 'POST 200 '[JSON] () -- 'pOST10SamSyslogDisable' route
    :<|> "1.0" :> "sam" :> "syslog" :> "enable" :> Verb 'POST 200 '[JSON] () -- 'pOST10SamSyslogEnable' route
    :<|> "2.0" :> "endpointsecurity" :> "activation" :> Capture "vendorID" Text :> Capture "altitude" Text :> ReqBody '[JSON] VShieldSolutionActivate :> Verb 'POST 200 '[JSON] () -- 'pOST20EndpointsecurityActivationVendorIDAltitude' route
    :<|> "2.0" :> "endpointsecurity" :> "registration" :> ReqBody '[JSON] VShieldVendorCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20EndpointsecurityRegistration' route
    :<|> "2.0" :> "endpointsecurity" :> "registration" :> Capture "vendorID" Text :> ReqBody '[JSON] VShieldSolutionCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20EndpointsecurityRegistrationVendorID' route
    :<|> "2.0" :> "endpointsecurity" :> "registration" :> Capture "vendorID" Text :> Capture "altitude" Text :> "location" :> ReqBody '[JSON] SolutionIPPortSet :> Verb 'POST 200 '[JSON] () -- 'pOST20EndpointsecurityRegistrationVendorIDAltitudeLocation' route
    :<|> "2.0" :> "hostevents" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20Hostevents' route
    :<|> "2.0" :> "nwfabric" :> "configure" :> QueryParam "action" Text :> ReqBody '[JSON] NwFabricConfig :> Verb 'POST 200 '[JSON] () -- 'pOST20NwfabricConfigure' route
    :<|> "2.0" :> "services" :> "alarms" :> Capture "sourceId" Text :> QueryParam "action" Text :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesAlarmsSourceId' route
    :<|> "2.0" :> "services" :> "application" :> Capture "scopeId" Text :> ReqBody '[JSON] ServicesScopeCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesApplicationScopeId' route
    :<|> "2.0" :> "services" :> "applicationgroup" :> Capture "scopeId" Text :> ReqBody '[JSON] ServiceGroupsCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesApplicationgroupScopeId' route
    :<|> "2.0" :> "services" :> "auth" :> "token" :> QueryParam "expiresInMinutes" Text :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesAuthToken' route
    :<|> "2.0" :> "services" :> "auth" :> "tokeninvalidation" :> QueryParam "userId" Text :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesAuthTokeninvalidation' route
    :<|> "2.0" :> "services" :> "dashboard" :> "ui-views" :> "dashboard" :> "widgetconfigurations" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesDashboardUiViewsDashboardWidgetconfigurations' route
    :<|> "2.0" :> "services" :> "housekeeping" :> "management" :> "index_maintenance" :> QueryParam "action" Text :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesHousekeepingManagementIndexMaintenance' route
    :<|> "2.0" :> "services" :> "ipam" :> "pools" :> Capture "poolId" Text :> "ipaddresses" :> ReqBody '[JSON] IpAddressRequest :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesIpamPoolsPoolIdIpaddresses' route
    :<|> "2.0" :> "services" :> "ipam" :> "pools" :> "scope" :> Capture "scopeId" Text :> ReqBody '[JSON] IpPool :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesIpamPoolsScopeScopeId' route
    :<|> "2.0" :> "services" :> "ipset" :> Capture "scopeMoref" Text :> ReqBody '[JSON] IpsetCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesIpsetScopeMoref' route
    :<|> "2.0" :> "services" :> "macset" :> "scope" :> Capture "scopeId" Text :> ReqBody '[JSON] MacSetCreateUpdate :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesMacsetScopeScopeId' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> ReqBody '[JSON] SecurityPolicyCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesPolicySecuritypolicy' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> "hierarchy" :> QueryParam "suffix" Text :> ReqBody '[JSON] HierarchyCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesPolicySecuritypolicyHierarchy' route
    :<|> "2.0" :> "services" :> "securitygroup" :> "bulk" :> Capture "scopeId" Text :> ReqBody '[JSON] SecGroupBulkCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesSecuritygroupBulkScopeId' route
    :<|> "2.0" :> "services" :> "securitygroup" :> Capture "scopeId" Text :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesSecuritygroupScopeId' route
    :<|> "2.0" :> "services" :> "securitytags" :> "tag" :> ReqBody '[JSON] SecurityTagCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesSecuritytagsTag' route
    :<|> "2.0" :> "services" :> "securitytags" :> "tag" :> Capture "tagId" Text :> "vm" :> QueryParam "action" Text :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesSecuritytagsTagTagIdVm' route
    :<|> "2.0" :> "services" :> "securitytags" :> "vm" :> Capture "vmId" Text :> QueryParam "action" Text :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesSecuritytagsVmVmId' route
    :<|> "2.0" :> "services" :> "snmp" :> "manager" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesSnmpManager' route
    :<|> "2.0" :> "services" :> "ssoconfig" :> ReqBody '[JSON] SsoConfig :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesSsoconfig' route
    :<|> "2.0" :> "services" :> "systemalarms" :> Capture "alarmId" Text :> QueryParam "action" Text :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesSystemalarmsAlarmId' route
    :<|> "2.0" :> "services" :> "truststore" :> "certificate" :> QueryParam "csrId" Text :> ReqBody '[JSON] CertificateCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesTruststoreCertificate' route
    :<|> "2.0" :> "services" :> "truststore" :> "config" :> Capture "scopeId" Text :> ReqBody '[JSON] CertificateSelfSignedCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesTruststoreConfigScopeId' route
    :<|> "2.0" :> "services" :> "truststore" :> "crl" :> Capture "scopeId" Text :> ReqBody '[JSON] CrlCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesTruststoreCrlScopeId' route
    :<|> "2.0" :> "services" :> "truststore" :> "csr" :> Capture "scopeId" Text :> ReqBody '[JSON] CsrCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesTruststoreCsrScopeId' route
    :<|> "2.0" :> "services" :> "usermgmt" :> "role" :> Capture "userId" Text :> QueryParam "isGroup" Bool :> ReqBody '[JSON] UserRoleMgmtCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesUsermgmtRoleUserId' route
    :<|> "2.0" :> "services" :> "vcconfig" :> "connectionstatus" :> QueryParam "action" Text :> Verb 'POST 200 '[JSON] () -- 'pOST20ServicesVcconfigConnectionstatus' route
    :<|> "2.0" :> "si" :> "deploy" :> QueryParam "startTime" Text :> ReqBody '[JSON] SecurityFabricCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20SiDeploy' route
    :<|> "2.0" :> "techsupportbundle" :> QueryParam "generate" Text :> QueryParam "cancel" Text :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20Techsupportbundle' route
    :<|> "2.0" :> "universalsync" :> "configuration" :> "nsxmanagers" :> ReqBody '[JSON] UniversalSyncConfigurationNsxManagersCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20UniversalsyncConfigurationNsxmanagers' route
    :<|> "2.0" :> "universalsync" :> "configuration" :> "role" :> QueryParam "action" Text :> Verb 'POST 200 '[JSON] () -- 'pOST20UniversalsyncConfigurationRole' route
    :<|> "2.0" :> "universalsync" :> "sync" :> QueryParam "action" Text :> Verb 'POST 200 '[JSON] () -- 'pOST20UniversalsyncSync' route
    :<|> "2.0" :> "vdn" :> "cdo" :> QueryParam "action" Text :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnCdo' route
    :<|> "2.0" :> "vdn" :> "config" :> "host" :> Capture "hostId" Text :> "vxlan" :> "vteps" :> QueryParam "action" Text :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnConfigHostHostIdVxlanVteps' route
    :<|> "2.0" :> "vdn" :> "config" :> "multicasts" :> QueryParam "isUniversal" Bool :> ReqBody '[JSON] VdnMulticast :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnConfigMulticasts' route
    :<|> "2.0" :> "vdn" :> "config" :> "segments" :> QueryParam "isUniversal" Bool :> ReqBody '[JSON] VdnSegment :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnConfigSegments' route
    :<|> "2.0" :> "vdn" :> "config" :> "vxlan" :> "udp" :> "port" :> "resume" :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnConfigVxlanUdpPortResume' route
    :<|> "2.0" :> "vdn" :> "controller" :> ReqBody '[JSON] Controller :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnController' route
    :<|> "2.0" :> "vdn" :> "controller" :> Capture "controllerId" Text :> QueryParam "action" Text :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnControllerControllerId' route
    :<|> "2.0" :> "vdn" :> "controller" :> Capture "controllerId" Text :> "syslog" :> ReqBody '[JSON] ControllerSyslog :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnControllerControllerIdSyslog' route
    :<|> "2.0" :> "vdn" :> "hardwaregateway" :> "bindings" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnHardwaregatewayBindings' route
    :<|> "2.0" :> "vdn" :> "hardwaregateway" :> "bindings" :> "manage" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnHardwaregatewayBindingsManage' route
    :<|> "2.0" :> "vdn" :> "hardwaregateways" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnHardwaregateways' route
    :<|> "2.0" :> "vdn" :> "scopes" :> QueryParam "isUniversal" Bool :> ReqBody '[JSON] VdnScopeCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnScopes' route
    :<|> "2.0" :> "vdn" :> "scopes" :> Capture "scopeId" Text :> QueryParam "action" Text :> ReqBody '[JSON] VdnScopeEdit :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnScopesScopeId' route
    :<|> "2.0" :> "vdn" :> "scopes" :> Capture "scopeId" Text :> "cdo" :> QueryParam "action" Text :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnScopesScopeIdCdo' route
    :<|> "2.0" :> "vdn" :> "scopes" :> Capture "scopeId" Text :> "conn-check" :> "multicast" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnScopesScopeIdConnCheckMulticast' route
    :<|> "2.0" :> "vdn" :> "scopes" :> Capture "scopeId" Text :> "virtualwires" :> ReqBody '[JSON] LogicalSwitchCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnScopesScopeIdVirtualwires' route
    :<|> "2.0" :> "vdn" :> "switches" :> ReqBody '[JSON] VdsContext :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnSwitches' route
    :<|> "2.0" :> "vdn" :> "traceflow" :> ReqBody '[JSON] TraceflowCreate :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnTraceflow' route
    :<|> "2.0" :> "vdn" :> "virtualwires" :> Capture "virtualWireID" Text :> "backing" :> QueryParam "action" Text :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnVirtualwiresVirtualWireIDBacking' route
    :<|> "2.0" :> "vdn" :> "virtualwires" :> Capture "virtualWireID" Text :> "conn-check" :> "multicast" :> ReqBody '[JSON] LogicalSwitchConnCheck :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnVirtualwiresVirtualWireIDConnCheckMulticast' route
    :<|> "2.0" :> "vdn" :> "virtualwires" :> Capture "virtualWireID" Text :> "conn-check" :> "p2p" :> ReqBody '[JSON] LogicalSwitchPing :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnVirtualwiresVirtualWireIDConnCheckP2p' route
    :<|> "2.0" :> "vdn" :> "virtualwires" :> Capture "virtualWireID" Text :> "hardwaregateways" :> Capture "hardwareGatewayBindingId" Text :> QueryParam "action" Text :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnVirtualwiresVirtualWireIDHardwaregatewaysHardwareGatewayBindingId' route
    :<|> "2.0" :> "vdn" :> "virtualwires" :> "vm" :> "vnic" :> ReqBody '[JSON] LogicalSwitchVmAttach :> Verb 'POST 200 '[JSON] () -- 'pOST20VdnVirtualwiresVmVnic' route
    :<|> "4.0" :> "edges" :> QueryParam "isUniversal" Bool :> ReqBody '[JSON] NsxEdgesCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40Edges' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> QueryParam "action" Text :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeId' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "aesni" :> QueryParam "enable" Bool :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdAesni' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "appliances" :> QueryParam "size" Text :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdAppliances' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "appliances" :> Capture "haIndex" Text :> QueryParam "action" Text :> ReqBody '[JSON] Text :> Header "Accept" Text :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdAppliancesHaIndex' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "cliremoteaccess" :> QueryParam "enable" Bool :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdCliremoteaccess' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "coredump" :> QueryParam "enable" Bool :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdCoredump' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dhcp" :> "config" :> "bindings" :> ReqBody '[JSON] DhcpStaticBindingCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdDhcpConfigBindings' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dhcp" :> "config" :> "ippools" :> ReqBody '[JSON] DhcpPoolCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdDhcpConfigIppools' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "fips" :> QueryParam "enable" Bool :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdFips' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "firewall" :> "config" :> "rules" :> QueryParam "aboveRuleId" Text :> ReqBody '[JSON] FirewallRulesCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdFirewallConfigRules' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "interfaces" :> QueryParam "action" Text :> ReqBody '[JSON] InterfacesCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdInterfaces' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "l2vpn" :> "config" :> QueryParam "enableService" Bool :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdL2vpnConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "acceleration" :> QueryParam "enable" Bool :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdLoadbalancerAcceleration' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "applicationprofiles" :> ReqBody '[JSON] ApplicationProfilesCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdLoadbalancerConfigApplicationprofiles' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "applicationrules" :> ReqBody '[JSON] AppRulesCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdLoadbalancerConfigApplicationrules' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "members" :> Capture "memberID" Text :> QueryParam "enable" Bool :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdLoadbalancerConfigMembersMemberID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "monitors" :> ReqBody '[JSON] LbMonitorsCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdLoadbalancerConfigMonitors' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "pools" :> ReqBody '[JSON] PoolsCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdLoadbalancerConfigPools' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "virtualservers" :> ReqBody '[JSON] VirtualServersCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdLoadbalancerConfigVirtualservers' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "logging" :> QueryParam "level" Text :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdLogging' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "nat" :> "config" :> "rules" :> QueryParam "aboveRuleId" Text :> ReqBody '[JSON] EdgeNatRulesCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdNatConfigRules' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> Capture "parentVnicIndex" Text :> "subinterfaces" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdParentVnicIndexSubinterfaces' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> QueryParam "enableService" Bool :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdSslvpnConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "auth" :> "localserver" :> "users" :> ReqBody '[JSON] UsersCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdSslvpnConfigAuthLocalserverUsers' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "auth" :> "settings" :> "rsaconfigfile" :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdSslvpnConfigAuthSettingsRsaconfigfile' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "installpackages" :> ReqBody '[JSON] InstallPackagesCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "ippools" :> ReqBody '[JSON] NetExtipPoolsCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "privatenetworks" :> ReqBody '[JSON] PrivateNetworksCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "layout" :> "images" :> Capture "imageType" Text :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdSslvpnConfigLayoutImagesImageType' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "script" :> ReqBody '[JSON] ScriptCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdSslvpnConfigScript' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "script" :> "file" :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdSslvpnConfigScriptFile' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "tunnels" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdTunnels' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "vnics" :> QueryParam "action" Text :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST40EdgesEdgeIdVnics' route
    :<|> "4.0" :> "firewall" :> "forceSync" :> Capture "ID" Text :> Verb 'POST 200 '[JSON] () -- 'pOST40FirewallForceSyncID' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer2sections" :> QueryParam "operation" Text :> QueryParam "anchorId" Text :> ReqBody '[JSON] DfwSection :> Verb 'POST 200 '[JSON] () -- 'pOST40FirewallGlobalroot0ConfigLayer2sections' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer2sections" :> Capture "sectionId" Text :> QueryParam "action" Text :> QueryParam "operation" Text :> QueryParam "anchorId" Text :> ReqBody '[JSON] Text :> Header "If-Match" Text :> Verb 'POST 200 '[JSON] () -- 'pOST40FirewallGlobalroot0ConfigLayer2sectionsSectionId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer2sections" :> Capture "sectionId" Text :> "rules" :> ReqBody '[JSON] DfwRule :> Header "If-Match" Text :> Verb 'POST 200 '[JSON] () -- 'pOST40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRules' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3redirectsections" :> ReqBody '[JSON] Layer3RedirectSectionsCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40FirewallGlobalroot0ConfigLayer3redirectsections' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3redirectsections" :> Capture "section" Text :> "rules" :> ReqBody '[JSON] RulesCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRules' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3sections" :> QueryParam "operation" Text :> QueryParam "anchorId" Text :> ReqBody '[JSON] DfwSection :> Verb 'POST 200 '[JSON] () -- 'pOST40FirewallGlobalroot0ConfigLayer3sections' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3sections" :> Capture "sectionId" Text :> QueryParam "action" Text :> QueryParam "operation" Text :> QueryParam "anchorId" Text :> ReqBody '[JSON] Text :> Header "If-Match" Text :> Verb 'POST 200 '[JSON] () -- 'pOST40FirewallGlobalroot0ConfigLayer3sectionsSectionId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3sections" :> Capture "sectionId" Text :> "rules" :> ReqBody '[JSON] DfwRule :> Header "If-Match" Text :> Verb 'POST 200 '[JSON] () -- 'pOST40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRules' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "drafts" :> ReqBody '[JSON] DfwDraftsCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40FirewallGlobalroot0Drafts' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "drafts" :> "action" :> "import" :> ReqBody '[JSON] DfwConfigImport :> Verb 'POST 200 '[JSON] () -- 'pOST40FirewallGlobalroot0DraftsActionImport' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "timeouts" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST40FirewallGlobalroot0Timeouts' route
    :<|> "4.0" :> "firewall" :> "objects" :> "status" :> "vm" :> Capture "vm_ID" Text :> "containers" :> ReqBody '[JSON] Text :> Verb 'POST 200 '[JSON] () -- 'pOST40FirewallObjectsStatusVmVmIDContainers' route
    :<|> "4.0" :> "services" :> "spoofguard" :> "policies" :> ReqBody '[JSON] SpoofGuardPoliciesCreate :> Verb 'POST 200 '[JSON] () -- 'pOST40ServicesSpoofguardPolicies' route
    :<|> "4.0" :> "services" :> "spoofguard" :> Capture "policyID" Text :> QueryParam "vnicId" Text :> QueryParam "action" Text :> ReqBody '[JSON] SpoofGuardPolicyApprove :> Verb 'POST 200 '[JSON] () -- 'pOST40ServicesSpoofguardPolicyID' route
    :<|> "1.0" :> "appliance-management" :> "backuprestore" :> "backupsettings" :> ReqBody '[JSON] ApplianceMgrBackupSettingsUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT10ApplianceManagementBackuprestoreBackupsettings' route
    :<|> "1.0" :> "appliance-management" :> "backuprestore" :> "backupsettings" :> "excludedata" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT10ApplianceManagementBackuprestoreBackupsettingsExcludedata' route
    :<|> "1.0" :> "appliance-management" :> "backuprestore" :> "backupsettings" :> "ftpsettings" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT10ApplianceManagementBackuprestoreBackupsettingsFtpsettings' route
    :<|> "1.0" :> "appliance-management" :> "backuprestore" :> "backupsettings" :> "schedule" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT10ApplianceManagementBackuprestoreBackupsettingsSchedule' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "locale" :> ReqBody '[JSON] SystemLocaleUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT10ApplianceManagementSystemLocale' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "network" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT10ApplianceManagementSystemNetwork' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "network" :> "dns" :> ReqBody '[JSON] ApplianceDnsClientUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT10ApplianceManagementSystemNetworkDns' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "syslogserver" :> ReqBody '[JSON] SystemSyslogServerUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT10ApplianceManagementSystemSyslogserver' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "syslogservers" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT10ApplianceManagementSystemSyslogservers' route
    :<|> "1.0" :> "appliance-management" :> "system" :> "timesettings" :> ReqBody '[JSON] SystemTimeUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT10ApplianceManagementSystemTimesettings' route
    :<|> "1.0" :> "directory" :> "deltaSync" :> Capture "domainID" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT10DirectoryDeltaSyncDomainID' route
    :<|> "1.0" :> "directory" :> "fullSync" :> Capture "domainID" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT10DirectoryFullSyncDomainID' route
    :<|> "1.0" :> "telemetry" :> "config" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT10TelemetryConfig' route
    :<|> "1.0" :> "telemetry" :> "proxy" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT10TelemetryProxy' route
    :<|> "2.0" :> "capacity-parameters" :> "thresholds" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20CapacityParametersThresholds' route
    :<|> "2.0" :> "endpointsecurity" :> "usvmstats" :> "usvmhealththresholds" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20EndpointsecurityUsvmstatsUsvmhealththresholds' route
    :<|> "2.0" :> "nwfabric" :> "clusters" :> Capture "clusterID" Text :> ReqBody '[JSON] NwfabricClustersUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20NwfabricClustersClusterID' route
    :<|> "2.0" :> "nwfabric" :> "configure" :> ReqBody '[JSON] NwFabricConfig :> Verb 'PUT 200 '[JSON] () -- 'pUT20NwfabricConfigure' route
    :<|> "2.0" :> "nwfabric" :> "hosts" :> Capture "hostID" Text :> ReqBody '[JSON] NwfabricHostsUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20NwfabricHostsHostID' route
    :<|> "2.0" :> "services" :> "application" :> Capture "applicationId" Text :> ReqBody '[JSON] ServiceUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesApplicationApplicationId' route
    :<|> "2.0" :> "services" :> "applicationgroup" :> Capture "applicationgroupId" Text :> ReqBody '[JSON] ServiceGroupUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesApplicationgroupApplicationgroupId' route
    :<|> "2.0" :> "services" :> "applicationgroup" :> Capture "applicationgroupId" Text :> "members" :> Capture "moref" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesApplicationgroupApplicationgroupIdMembersMoref' route
    :<|> "2.0" :> "services" :> "auth" :> "tokenexpiration" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesAuthTokenexpiration' route
    :<|> "2.0" :> "services" :> "configuration" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesConfiguration' route
    :<|> "2.0" :> "services" :> "dashboard" :> "ui-views" :> "dashboard" :> "widgetconfigurations" :> Capture "widgetconfigurationId" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesDashboardUiViewsDashboardWidgetconfigurationsWidgetconfigurationId' route
    :<|> "2.0" :> "services" :> "housekeeping" :> "management" :> "index_maintenance" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesHousekeepingManagementIndexMaintenance' route
    :<|> "2.0" :> "services" :> "ipam" :> "pools" :> Capture "poolId" Text :> ReqBody '[JSON] IpPoolUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesIpamPoolsPoolId' route
    :<|> "2.0" :> "services" :> "ipset" :> Capture "ipsetId" Text :> ReqBody '[JSON] IpsetUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesIpsetIpsetId' route
    :<|> "2.0" :> "services" :> "macset" :> Capture "macsetId" Text :> ReqBody '[JSON] MacSetCreateUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesMacsetMacsetId' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> Capture "ID" Text :> ReqBody '[JSON] SecurityPolicyIDUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesPolicySecuritypolicyID' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> Capture "ID" Text :> "sgbinding" :> Capture "securityGroupId" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesPolicySecuritypolicyIDSgbindingSecurityGroupId' route
    :<|> "2.0" :> "services" :> "policy" :> "securitypolicy" :> "serviceprovider" :> "firewall" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesPolicySecuritypolicyServiceproviderFirewall' route
    :<|> "2.0" :> "services" :> "securitygroup" :> "bulk" :> Capture "objectId" Text :> ReqBody '[JSON] SecGroupBulkUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesSecuritygroupBulkObjectId' route
    :<|> "2.0" :> "services" :> "securitygroup" :> Capture "objectId" Text :> ReqBody '[JSON] SecGroupObjectUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesSecuritygroupObjectId' route
    :<|> "2.0" :> "services" :> "securitygroup" :> Capture "objectId" Text :> "members" :> Capture "memberId" Text :> QueryParam "failIfExists" Bool :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesSecuritygroupObjectIdMembersMemberId' route
    :<|> "2.0" :> "services" :> "securitytags" :> "selection-criteria" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesSecuritytagsSelectionCriteria' route
    :<|> "2.0" :> "services" :> "securitytags" :> "tag" :> Capture "tagId" Text :> "vm" :> Capture "vmId" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesSecuritytagsTagTagIdVmVmId' route
    :<|> "2.0" :> "services" :> "snmp" :> "manager" :> Capture "managerId" Text :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesSnmpManagerManagerId' route
    :<|> "2.0" :> "services" :> "snmp" :> "status" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesSnmpStatus' route
    :<|> "2.0" :> "services" :> "snmp" :> "trap" :> Capture "oid" Text :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesSnmpTrapOid' route
    :<|> "2.0" :> "services" :> "truststore" :> "config" :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesTruststoreConfig' route
    :<|> "2.0" :> "services" :> "truststore" :> "csr" :> Capture "csrId" Text :> QueryParam "noOfDays" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesTruststoreCsrCsrId' route
    :<|> "2.0" :> "services" :> "usermgmt" :> "role" :> Capture "userId" Text :> ReqBody '[JSON] UserRoleMgmtUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesUsermgmtRoleUserId' route
    :<|> "2.0" :> "services" :> "usermgmt" :> "user" :> Capture "userId" Text :> "enablestate" :> Capture "value" Int :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesUsermgmtUserUserIdEnablestateValue' route
    :<|> "2.0" :> "services" :> "vcconfig" :> ReqBody '[JSON] VcConfig :> Verb 'PUT 200 '[JSON] () -- 'pUT20ServicesVcconfig' route
    :<|> "2.0" :> "si" :> "deploy" :> QueryParam "startTime" Text :> ReqBody '[JSON] ServiceUpgrade :> Verb 'PUT 200 '[JSON] () -- 'pUT20SiDeploy' route
    :<|> "2.0" :> "si" :> "fabric" :> "sync" :> "conflicts" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20SiFabricSyncConflicts' route
    :<|> "2.0" :> "universalsync" :> "configuration" :> "nsxmanagers" :> Capture "nsxManagerID" Text :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20UniversalsyncConfigurationNsxmanagersNsxManagerID' route
    :<|> "2.0" :> "vdn" :> "bfd" :> "configuration" :> "global" :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnBfdConfigurationGlobal' route
    :<|> "2.0" :> "vdn" :> "config" :> "multicasts" :> Capture "multicastAddresssRangeId" Text :> ReqBody '[JSON] VdnMulticastUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnConfigMulticastsMulticastAddresssRangeId' route
    :<|> "2.0" :> "vdn" :> "config" :> "segments" :> Capture "segmentPoolId" Text :> ReqBody '[JSON] VdnSegmentUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnConfigSegmentsSegmentPoolId' route
    :<|> "2.0" :> "vdn" :> "config" :> "vxlan" :> "udp" :> "port" :> Capture "portNumber" Text :> QueryParam "force" Bool :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnConfigVxlanUdpPortPortNumber' route
    :<|> "2.0" :> "vdn" :> "controller" :> "cluster" :> ReqBody '[JSON] ClusterUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnControllerCluster' route
    :<|> "2.0" :> "vdn" :> "controller" :> "cluster" :> "ntp" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnControllerClusterNtp' route
    :<|> "2.0" :> "vdn" :> "controller" :> Capture "controllerId" Text :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnControllerControllerId' route
    :<|> "2.0" :> "vdn" :> "controller" :> "credential" :> ReqBody '[JSON] NsxControllerPasswordUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnControllerCredential' route
    :<|> "2.0" :> "vdn" :> "controller" :> "synchronize" :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnControllerSynchronize' route
    :<|> "2.0" :> "vdn" :> "hardwaregateway" :> "bfd" :> "config" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnHardwaregatewayBfdConfig' route
    :<|> "2.0" :> "vdn" :> "hardwaregateway" :> "bindings" :> Capture "bindingId" Text :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnHardwaregatewayBindingsBindingId' route
    :<|> "2.0" :> "vdn" :> "hardwaregateways" :> Capture "hardwareGatewayId" Text :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnHardwaregatewaysHardwareGatewayId' route
    :<|> "2.0" :> "vdn" :> "hardwaregateways" :> "replicationcluster" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnHardwaregatewaysReplicationcluster' route
    :<|> "2.0" :> "vdn" :> "pnic-check" :> "configuration" :> "global" :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnPnicCheckConfigurationGlobal' route
    :<|> "2.0" :> "vdn" :> "scopes" :> Capture "scopeId" Text :> "attributes" :> ReqBody '[JSON] VdnScopeUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnScopesScopeIdAttributes' route
    :<|> "2.0" :> "vdn" :> "virtualwires" :> Capture "virtualWireID" Text :> ReqBody '[JSON] LogicalSwitchUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20VdnVirtualwiresVirtualWireID' route
    :<|> "2.0" :> "xvs" :> "networks" :> Capture "ID" Text :> "features" :> ReqBody '[JSON] ArpMACUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT20XvsNetworksIDFeatures' route
    :<|> "2.1" :> "app" :> "excludelist" :> Capture "memberID" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT21AppExcludelistMemberID' route
    :<|> "2.1" :> "app" :> "flow" :> "config" :> ReqBody '[JSON] FlowsExcludeCreate :> Verb 'PUT 200 '[JSON] () -- 'pUT21AppFlowConfig' route
    :<|> "4.0" :> "edgePublish" :> "tuningConfiguration" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgePublishTuningConfiguration' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> ReqBody '[JSON] NsxEdgeUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeId' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "appliances" :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdAppliances' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "appliances" :> Capture "haIndex" Text :> ReqBody '[JSON] ApplianceUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdAppliancesHaIndex' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "autoconfiguration" :> ReqBody '[JSON] AutoConfigUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdAutoconfiguration' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "bridging" :> "config" :> ReqBody '[JSON] BridingUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdBridgingConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "clisettings" :> ReqBody '[JSON] CliSettingsUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdClisettings' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dhcp" :> "config" :> ReqBody '[JSON] DhcpUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdDhcpConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dhcp" :> "config" :> "relay" :> ReqBody '[JSON] DhcpRelayUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdDhcpConfigRelay' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dns" :> "config" :> ReqBody '[JSON] EdgeDnsUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdDnsConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "dnsclient" :> ReqBody '[JSON] EdgeDnsClientUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdDnsclient' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "firewall" :> "config" :> ReqBody '[JSON] NsxEdgeFirewallConfigUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdFirewallConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "firewall" :> "config" :> "defaultpolicy" :> ReqBody '[JSON] DefaultFirewallPolicyUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdFirewallConfigDefaultpolicy' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "firewall" :> "config" :> "global" :> ReqBody '[JSON] GlobalFirewallConfigUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdFirewallConfigGlobal' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "firewall" :> "config" :> "rules" :> Capture "ruleId" Text :> ReqBody '[JSON] FirewallRuleUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdFirewallConfigRulesRuleId' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "highavailability" :> "config" :> ReqBody '[JSON] HighAvailabilityCreate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdHighavailabilityConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "interfaces" :> Capture "index" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdInterfacesIndex' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "ipsec" :> "config" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdIpsecConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "l2vpn" :> "config" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdL2vpnConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> ReqBody '[JSON] LoadBalancerConfig :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdLoadbalancerConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "applicationprofiles" :> Capture "appProfileID" Text :> ReqBody '[JSON] ApplicationProfileUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdLoadbalancerConfigApplicationprofilesAppProfileID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "applicationrules" :> Capture "appruleID" Text :> ReqBody '[JSON] AppRuleUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdLoadbalancerConfigApplicationrulesAppruleID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "monitors" :> Capture "monitorID" Text :> ReqBody '[JSON] LbMonitorUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdLoadbalancerConfigMonitorsMonitorID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "loadbalancer" :> "config" :> "pools" :> Capture "poolID" Text :> ReqBody '[JSON] PoolUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdLoadbalancerConfigPoolsPoolID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "mgmtinterface" :> ReqBody '[JSON] MgmtInterfaceUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdMgmtinterface' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "nat" :> "config" :> ReqBody '[JSON] EdgeNatConfig :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdNatConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "nat" :> "config" :> "rules" :> Capture "ruleID" Text :> ReqBody '[JSON] EdgeNatRuleUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdNatConfigRulesRuleID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> QueryParam "le" Double :> QueryParam "ge" Double :> ReqBody '[JSON] RoutingConfigUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdRoutingConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> "bgp" :> ReqBody '[JSON] RoutingBGPUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdRoutingConfigBgp' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> "global" :> QueryParam "le" Double :> QueryParam "ge" Double :> ReqBody '[JSON] RoutingGlobalConfigUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdRoutingConfigGlobal' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> "ospf" :> ReqBody '[JSON] RoutingOSPFUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdRoutingConfigOspf' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "routing" :> "config" :> "static" :> ReqBody '[JSON] RoutingConfigStaticUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdRoutingConfigStatic' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "auth" :> "localusers" :> "users" :> ReqBody '[JSON] AllUsersUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnAuthLocalusersUsers' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "advancedconfig" :> ReqBody '[JSON] AdvancedConfigUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigAdvancedconfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "auth" :> "localserver" :> "users" :> ReqBody '[JSON] UsersUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigAuthLocalserverUsers' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "auth" :> "settings" :> ReqBody '[JSON] AuthSettingsUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigAuthSettings' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "clientconfig" :> ReqBody '[JSON] ClientConfigUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionClientconfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "installpackages" :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "installpackages" :> Capture "packageID" Text :> ReqBody '[JSON] InstallPackageUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackagesPackageID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "ippools" :> ReqBody '[JSON] NetExtipPoolsUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "ippools" :> Capture "ippoolID" Text :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppoolsIppoolID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "privatenetworks" :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "client" :> "networkextension" :> "privatenetworks" :> Capture "networkID" Text :> ReqBody '[JSON] PrivateNetworkUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworksNetworkID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "layout" :> ReqBody '[JSON] LayoutUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigLayout' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "script" :> ReqBody '[JSON] ScriptUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigScript' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "script" :> Capture "fileID" Text :> ReqBody '[JSON] ScriptFileIDUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigScriptFileID' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "sslvpn" :> "config" :> "server" :> ReqBody '[JSON] ServerSettingsUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSslvpnConfigServer' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "syslog" :> "config" :> ReqBody '[JSON] SyslogUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSyslogConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "systemcontrol" :> "config" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdSystemcontrolConfig' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "tunnels" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdTunnels' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "tunnels" :> Capture "tunnelId" Text :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdTunnelsTunnelId' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "vnics" :> Capture "index" Text :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdVnicsIndex' route
    :<|> "4.0" :> "edges" :> Capture "edgeId" Text :> "vnics" :> Capture "parentVnicIndex" Text :> "subinterfaces" :> Capture "subInterfaceIndex" Text :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40EdgesEdgeIdVnicsParentVnicIndexSubinterfacesSubInterfaceIndex' route
    :<|> "4.0" :> "firewall" :> "config" :> "globalconfiguration" :> ReqBody '[JSON] DfwPerformanceUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallConfigGlobalconfiguration' route
    :<|> "4.0" :> "firewall" :> Capture "domainID" Text :> "enable" :> Capture "truefalse" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallDomainIDEnableTruefalse' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> ReqBody '[JSON] Text :> Header "If-Match" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallGlobalroot0Config' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "ipfix" :> ReqBody '[JSON] DfwIPFixUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallGlobalroot0ConfigIpfix' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer2sections" :> Capture "sectionId" Text :> ReqBody '[JSON] Text :> Header "If-Match" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallGlobalroot0ConfigLayer2sectionsSectionId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer2sections" :> Capture "sectionId" Text :> "rules" :> Capture "ruleId" Text :> ReqBody '[JSON] Text :> Header "If-Match" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRulesRuleId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3redirectsections" :> Capture "section" Text :> ReqBody '[JSON] Layer3RedirectSectionUpdate :> Header "If-Match" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallGlobalroot0ConfigLayer3redirectsectionsSection' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3redirectsections" :> Capture "section" Text :> "rules" :> Capture "ruleID" Text :> ReqBody '[JSON] RuleUpdate :> Header "If-Match" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRulesRuleID' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3sections" :> Capture "sectionId" Text :> ReqBody '[JSON] Text :> Header "If-Match" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallGlobalroot0ConfigLayer3sectionsSectionId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "config" :> "layer3sections" :> Capture "sectionId" Text :> "rules" :> Capture "ruleId" Text :> ReqBody '[JSON] Text :> Header "If-Match" Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRulesRuleId' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "drafts" :> Capture "draftID" Text :> ReqBody '[JSON] DfwDraftUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallGlobalroot0DraftsDraftID' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "state" :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallGlobalroot0State' route
    :<|> "4.0" :> "firewall" :> "globalroot-0" :> "timeouts" :> Capture "configId" Text :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallGlobalroot0TimeoutsConfigId' route
    :<|> "4.0" :> "firewall" :> "stats" :> "eventthresholds" :> ReqBody '[JSON] DfwThresholdsUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallStatsEventthresholds' route
    :<|> "4.0" :> "firewall" :> "stats" :> "thresholds" :> ReqBody '[JSON] Text :> Verb 'PUT 200 '[JSON] () -- 'pUT40FirewallStatsThresholds' route
    :<|> "4.0" :> "services" :> "spoofguard" :> "policies" :> Capture "policyID" Text :> ReqBody '[JSON] SpoofGuardPolicyUpdate :> Verb 'PUT 200 '[JSON] () -- 'pUT40ServicesSpoofguardPoliciesPolicyID' route

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String  -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Backend for VMwareNSXForVSphere.
-- The backend can be used both for the client and the server. The client generated from the VMwareNSXForVSphere Swagger spec
-- is a backend that executes actions by sending HTTP requests (see @createVMwareNSXForVSphereClient@). Alternatively, provided
-- a backend, the API can be served using @runVMwareNSXForVSphereServer@.
data VMwareNSXForVSphereBackend m = VMwareNSXForVSphereBackend
  { dELETE10ApplianceManagementBackuprestoreBackupsettings :: m (){- ^ Delete appliance manager backup configuration. -}
  , dELETE10ApplianceManagementBackuprestoreBackupsettingsSchedule :: m (){- ^ Delete backup schedule. -}
  , dELETE10ApplianceManagementNotifications :: m (){- ^ Delete all notifications. -}
  , dELETE10ApplianceManagementSystemNetworkDns :: m (){- ^ Delete DNS server configuration. -}
  , dELETE10ApplianceManagementSystemSyslogserver :: m (){- ^ Deletes all the syslog servers. -}
  , dELETE10ApplianceManagementSystemSyslogservers :: m (){- ^ Deletes all the syslog servers. Same as *DELETE /system/syslogserver* API.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , dELETE10ApplianceManagementSystemTimesettingsNtp :: m (){- ^ Delete NTP server. -}
  , dELETE10DirectoryDeleteDomainID :: Text -> m (){- ^ Delete domain. -}
  , dELETE10DirectoryDeleteDomainRootDNDomainID :: Text -> m (){- ^ Delete individual root distinguished name under which each domain sub-tree synchronization is not to be executed.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , dELETE10DirectoryDeleteEventLogServerServerID :: Text -> m (){- ^ Delete EventLog server. -}
  , dELETE10DirectoryDeleteLdapServerServerID :: Text -> m (){- ^ Delete LDAP server. -}
  , dELETE10IdentityStaticUserMappingsbyIPIP :: Text -> m (){- ^ Delete static user IP mapping for specified IP. -}
  , dELETE10IdentityStaticUserMappingsbyUserUserID :: Text -> m (){- ^ Delete static user IP mapping for specified user. -}
  , dELETE20EndpointsecurityActivationVendorIDAltitudeMoid :: Text -> Text -> Text -> m (){- ^ Deactivate an endpoint protection solution on a host. -}
  , dELETE20EndpointsecurityRegistrationVendorID :: Text -> m (){- ^ Unregister a Guest Introspection vendor. -}
  , dELETE20EndpointsecurityRegistrationVendorIDAltitude :: Text -> Text -> m (){- ^ Unregister an endpoint protection solution. -}
  , dELETE20EndpointsecurityRegistrationVendorIDAltitudeLocation :: Text -> Text -> m (){- ^ Unset the IP address and port for an endpoint protection solution.  -}
  , dELETE20NwfabricClustersClusterID :: Text -> m (){- ^ Delete locale ID for the specified cluster. -}
  , dELETE20NwfabricConfigure :: NwFabricConfig -> m (){- ^ Remove VXLAN or network virtualization components.  Removing network virtualization components removes previously installed VIBs, tears down NSX Manager to ESXi messaging, and removes any other network fabric dependent features such as logical switches. If a feature such as logical switches is being used in your environment, this call fails.  Removing VXLAN does not remove the network virtualization components from the cluster.  | Name | Comments | |------|----------| |**resourceId** | vCenter MOB ID of cluster. For example, domain-7.| |**featureId** | Feature to act upon. Omit for network virtualization components operations. Use *com.vmware.vshield.vsm.vxlan* for VXLAN operations.|  ### Remove Network Virtualization Components  ``` <nwFabricFeatureConfig>   <resourceConfig>     <resourceId>CLUSTER MOID</resourceId>   </resourceConfig> </nwFabricFeatureConfig> ```  ### Remove VXLAN  ``` <nwFabricFeatureConfig>   <featureId>com.vmware.vshield.vsm.vxlan</featureId>   <resourceConfig>     <resourceId>CLUSTER MOID</resourceId>    </resourceConfig> </nwFabricFeatureConfig> ```  ### Remove VXLAN with vDS context  ``` <nwFabricFeatureConfig>   <featureId>com.vmware.vshield.vsm.vxlan</featureId>   <resourceConfig>     <resourceId>CLUSTER MOID</resourceId>     <configSpec class=\"map\">       <entry>         <keyclass=\"java.lang.String\">vxlan</key>         <valueclass=\"java.lang.String\">cascadeDeleteVdsContext</value>       </entry>     </configSpec>   </resourceConfig> </nwFabricFeatureConfig> ```  -}
  , dELETE20NwfabricHostsHostID :: Text -> m (){- ^ Delete the locale ID for the specified host. -}
  , dELETE20ServicesApplicationApplicationId :: Text -> Maybe Bool -> m (){- ^ Delete the specified service.  -}
  , dELETE20ServicesApplicationgroupApplicationgroupId :: Text -> Maybe Bool -> m (){- ^ Delete the specified service group (application group) from a scope.  -}
  , dELETE20ServicesApplicationgroupApplicationgroupIdMembersMoref :: Text -> Text -> m (){- ^ Delete a member from the service group. -}
  , dELETE20ServicesDashboardUiViewsDashboardWidgetconfigurationsWidgetconfigurationId :: Text -> m (){- ^ Deletes a specific widget on the dashboard.                           -}
  , dELETE20ServicesIpamPoolsPoolId :: Text -> m (){- ^ Delete an IP pool. -}
  , dELETE20ServicesIpamPoolsPoolIdIpaddressesIpAddress :: Text -> Text -> m (){- ^ Release an IP address allocation in the pool. -}
  , dELETE20ServicesIpsetIpsetId :: Text -> Maybe Bool -> m (){- ^ Delete an IP set. -}
  , dELETE20ServicesMacsetMacsetId :: Text -> Maybe Bool -> m (){- ^ Delete a MAC address set. -}
  , dELETE20ServicesPolicySecuritypolicyID :: Text -> Maybe Bool -> m (){- ^ Delete a security policy.  When you delete a security policy, its child security policies and all the actions in it are deleted as well.  -}
  , dELETE20ServicesSecuritygroupObjectId :: Text -> Maybe Bool -> m (){- ^ Delete an existing security group.  If *force=true* is specified, the object is deleted even if used in other configurations, such as firewall rules. If *force=true* is not specified, the object is deleted only if it is not used by other configuration; otherwise the delete fails.  -}
  , dELETE20ServicesSecuritygroupObjectIdMembersMemberId :: Text -> Text -> Maybe Bool -> m (){- ^ Delete member from the specified security group. -}
  , dELETE20ServicesSecuritytagsTagTagId :: Text -> Maybe Text -> m (){- ^ Delete the specified security tag. -}
  , dELETE20ServicesSecuritytagsTagTagIdVmVmId :: Text -> Text -> m (){- ^ Detach a security tag from the specified virtual machine.  -}
  , dELETE20ServicesSnmpManagerManagerId :: Text -> m (){- ^ Delete an SNMP manager configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , dELETE20ServicesSsoconfig :: m (){- ^ Deletes the NSX Manager SSO Configuration. -}
  , dELETE20ServicesTruststoreConfigCertificateId :: Text -> m (){- ^ Delete the specified certificate. -}
  , dELETE20ServicesTruststoreCrlCrlId :: Text -> m (){- ^ Delete the specified certificate revocation list (CRL). -}
  , dELETE20ServicesUsermgmtRoleUserId :: Text -> m (){- ^ Delete the role assignment for specified vCenter user. Once this role is deleted, the user is removed from NSX Manager. You cannot delete the role for a local user.  -}
  , dELETE20ServicesUsermgmtUserUserId :: Text -> m (){- ^ Remove the NSX role for a vCenter user. -}
  , dELETE20SiDeployClusterClusterID :: Text -> Maybe Text -> Maybe Text -> m (){- ^ Uninstall a service. Fails if you try to remove a service that another service depends on.  In order to uninstall services in any order, set parameter ignoreDependency to true.  -}
  , dELETE20SiDeployServiceServiceID :: Text -> Maybe Text -> Maybe Text -> m (){- ^ Uninstall specified service from specified clusters. -}
  , dELETE20Techsupportbundle :: m (){- ^ Deletes the support bundle. -}
  , dELETE20UniversalsyncConfigurationNsxmanagers :: m (){- ^ Delete secondary NSX manager configuration. -}
  , dELETE20UniversalsyncConfigurationNsxmanagersNsxManagerID :: Text -> Maybe Bool -> m (){- ^ Delete the specified secondary NSX Manager. -}
  , dELETE20VdnConfigMulticastsMulticastAddresssRangeId :: Text -> m (){- ^ Delete the specified multicast address range.  If the multicast address range is universal you must send the API request to the primary NSX Manager.  -}
  , dELETE20VdnConfigSegmentsSegmentPoolId :: Text -> m (){- ^ Delete the specified segment ID pool.  If the segment ID pool is universal you must send the API request to the primary NSX Manager.  -}
  , dELETE20VdnControllerControllerId :: Text -> Maybe Bool -> m (){- ^ Delete the NSX controller.  -}
  , dELETE20VdnControllerControllerIdSyslog :: Text -> m (){- ^ Deletes syslog exporter on the specified controller node.  -}
  , dELETE20VdnHardwaregatewayBindingsBindingId :: Text -> m (){- ^ Delete the specified hardware gateway binding.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , dELETE20VdnHardwaregatewaysHardwareGatewayId :: Text -> m (){- ^ Delete the specified hardware gateway.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , dELETE20VdnScopesScopeId :: Text -> m (){- ^ Delete the specified transport zone.  -}
  , dELETE20VdnSwitchesVdsId :: Text -> m (){- ^ Delete the specified vSphere Distributed Switch.  -}
  , dELETE20VdnVirtualwiresVirtualWireID :: Text -> m (){- ^ Delete the specified logical switch. -}
  , dELETE21AppExcludelistMemberID :: Text -> m (){- ^ Delete a vm from exclusion list. -}
  , dELETE21AppFlowContextId :: Text -> m (){- ^ Delete flow records for the specified context. -}
  , dELETE40EdgesEdgeId :: Text -> m (){- ^ Delete specified NSX Edge configuration. Associated appliances are also deleted.  -}
  , dELETE40EdgesEdgeIdAppliancesHaIndex :: Text -> Text -> m (){- ^ Delete the appliance -}
  , dELETE40EdgesEdgeIdBridgingConfig :: Text -> m (){- ^ Delete bridges. -}
  , dELETE40EdgesEdgeIdDhcpConfig :: Text -> m (){- ^ Delete the DHCP configuration, restoring it to factory default.  -}
  , dELETE40EdgesEdgeIdDhcpConfigBindingsBindingID :: Text -> Text -> m (){- ^ Delete the specified static binding. -}
  , dELETE40EdgesEdgeIdDhcpConfigIppoolsPoolID :: Text -> Text -> m (){- ^ Delete a pool specified by pool ID -}
  , dELETE40EdgesEdgeIdDhcpConfigRelay :: Text -> m (){- ^ Delete DHCP relay configuration. -}
  , dELETE40EdgesEdgeIdDnsConfig :: Text -> m (){- ^ Delete DNS configuration -}
  , dELETE40EdgesEdgeIdFirewallConfig :: Text -> m (){- ^ Delete NSX Edge firewall configuration. -}
  , dELETE40EdgesEdgeIdFirewallConfigRulesRuleId :: Text -> Text -> m (){- ^ Delete firewall rule -}
  , dELETE40EdgesEdgeIdHighavailabilityConfig :: Text -> m (){- ^ NSX Manager deletes the standby appliance and removes the HA config from the active appliance. You can also delete the HA configuration by using a PUT call with empty &lt;highAvailability /&gt; or with &lt;highAvailability&gt;&lt;enabled&gt;false&lt;/enabled&gt;&lt;/highAvailability&gt;.  -}
  , dELETE40EdgesEdgeIdInterfaces :: Text -> Maybe Text -> m (){- ^ Delete all interfaces on the logical router. -}
  , dELETE40EdgesEdgeIdInterfacesIndex :: Text -> Text -> m (){- ^ Delete interface configuration and reset to factory default.  -}
  , dELETE40EdgesEdgeIdIpsecConfig :: Text -> m (){- ^ Delete the IPsec configuration. -}
  , dELETE40EdgesEdgeIdL2vpnConfig :: Text -> m (){- ^ Delete the L2 VPN configuration.  -}
  , dELETE40EdgesEdgeIdLoadbalancerConfig :: Text -> m (){- ^ Delete load balancer configuration. -}
  , dELETE40EdgesEdgeIdLoadbalancerConfigApplicationprofiles :: Text -> m (){- ^ Delete all application profiles on the specified Edge. -}
  , dELETE40EdgesEdgeIdLoadbalancerConfigApplicationprofilesAppProfileID :: Text -> Text -> m (){- ^ Delete an application profile. -}
  , dELETE40EdgesEdgeIdLoadbalancerConfigApplicationrules :: Text -> m (){- ^ Delete all application rules. -}
  , dELETE40EdgesEdgeIdLoadbalancerConfigApplicationrulesAppruleID :: Text -> Text -> m (){- ^ Delete an application rule. -}
  , dELETE40EdgesEdgeIdLoadbalancerConfigMonitors :: Text -> m (){- ^ Delete all load balancer monitors. -}
  , dELETE40EdgesEdgeIdLoadbalancerConfigMonitorsMonitorID :: Text -> Text -> m (){- ^ Delete a load balancer monitor. -}
  , dELETE40EdgesEdgeIdLoadbalancerConfigPools :: Text -> m (){- ^ Delete all server pools configured on the specified NSX Edge.  -}
  , dELETE40EdgesEdgeIdLoadbalancerConfigPoolsPoolID :: Text -> Text -> m (){- ^ Delete the specified server pool. -}
  , dELETE40EdgesEdgeIdLoadbalancerConfigVirtualservers :: Text -> m (){- ^ Delete all virtual servers. -}
  , dELETE40EdgesEdgeIdLoadbalancerConfigVirtualserversVirtualserverID :: Text -> Text -> m (){- ^ Delete the specified virtual server. -}
  , dELETE40EdgesEdgeIdNatConfig :: Text -> m (){- ^ Delete all NAT rules for the specified NSX Edge. The auto plumbed rules continue to exist.  -}
  , dELETE40EdgesEdgeIdNatConfigRulesRuleID :: Text -> Text -> m (){- ^ Delete the specified NAT rule. -}
  , dELETE40EdgesEdgeIdRoutingConfig :: Text -> m (){- ^ Delete the routing config stored in the NSX Manager database and the default routes from the specified NSX Edge appliance.  -}
  , dELETE40EdgesEdgeIdRoutingConfigBgp :: Text -> m (){- ^ Delete BGP Routing -}
  , dELETE40EdgesEdgeIdRoutingConfigOspf :: Text -> m (){- ^ Delete OSPF routing. -}
  , dELETE40EdgesEdgeIdRoutingConfigStatic :: Text -> m (){- ^ Delete both static and default routing config stored in the NSX Manager database.  -}
  , dELETE40EdgesEdgeIdSslvpnActivesessionsSessionID :: Text -> Text -> m (){- ^ Disconnect an active client. -}
  , dELETE40EdgesEdgeIdSslvpnConfig :: Text -> m (){- ^ Delete the SSL VPN configurations on the Edge. -}
  , dELETE40EdgesEdgeIdSslvpnConfigAuthLocalserverUsers :: Text -> m (){- ^ Delete all users on the specifed SSL VPN instance -}
  , dELETE40EdgesEdgeIdSslvpnConfigAuthLocalserverUsersUserID :: Text -> Text -> m (){- ^ Delete the specified user. -}
  , dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages :: Text -> m (){- ^ Delete all client installation packages. -}
  , dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackagesPackageID :: Text -> Text -> m (){- ^ Delete the specified installation package. -}
  , dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools :: Text -> m (){- ^ Delete all IP pools configured on SSL VPN -}
  , dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppoolsIppoolID :: Text -> Text -> m (){- ^ Delete the specified IP pool. -}
  , dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks :: Text -> m (){- ^ Delete all private networks from the SSL VPN instance. -}
  , dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworksNetworkID :: Text -> Text -> m (){- ^ Delete private network -}
  , dELETE40EdgesEdgeIdSslvpnConfigScript :: Text -> m (){- ^ Delete all script configurations -}
  , dELETE40EdgesEdgeIdSslvpnConfigScriptFileID :: Text -> Text -> m (){- ^ Delete script parameters. -}
  , dELETE40EdgesEdgeIdSyslogConfig :: Text -> m (){- ^ Delete syslog servers. -}
  , dELETE40EdgesEdgeIdSystemcontrolConfig :: Text -> Maybe Text -> m (){- ^ Delete all NSX Edge system control configuration.  Deleting the system control configuration requires a reboot of the NSX Edge appliance.  -}
  , dELETE40EdgesEdgeIdTunnels :: Text -> m (){- ^ Delete all configured tunnels on this Edge Services Gateway.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , dELETE40EdgesEdgeIdTunnelsTunnelId :: Text -> Text -> m (){- ^ Delete the specified tunnel.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , dELETE40EdgesEdgeIdVnicsIndex :: Text -> Text -> m (){- ^ Delete interface -}
  , dELETE40EdgesEdgeIdVnicsParentVnicIndexSubinterfacesSubInterfaceIndex :: Text -> Text -> Text -> m (){- ^ Delete a sub-interface.   **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , dELETE40FirewallConfigSections :: Maybe Text -> m (){- ^ Delete the universal sections when NSX Manager is in transit mode.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.   -}
  , dELETE40FirewallGlobalroot0Config :: m (){- ^ Restores default configuration, which means one defaultLayer3 section with three default allow rules and one defaultLayer2Section with one default allow rule.  -}
  , dELETE40FirewallGlobalroot0ConfigIpfix :: m (){- ^ Deleting IPFIX configuration resets the configuration to default values.  -}
  , dELETE40FirewallGlobalroot0ConfigLayer2sectionsSectionId :: Text -> m (){- ^ Delete the specified layer 2 section and its contents.  If the default layer 2 firewall section is selected, the request is rejected. See `GET /api/4.0/firewall/globalroot-0/defaultconfig` for information on resetting the default firewall section.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method updated. When deleting the default firewall rule section, the method previously removed all rules except for the default rule. The method now returns status 400 and the message `Cannot delete default section <sectionId>`.  -}
  , dELETE40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRulesRuleId :: Text -> Text -> Maybe Text -> m (){- ^ Delete the specified distributed firewall rule. -}
  , dELETE40FirewallGlobalroot0ConfigLayer3redirectsectionsSection :: Text -> m (){- ^ Delete specified L3 redirect section -}
  , dELETE40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRulesRuleID :: Text -> Text -> m (){- ^ Delete specified L3 redirect rule -}
  , dELETE40FirewallGlobalroot0ConfigLayer3sectionsSectionId :: Text -> m (){- ^ Delete the specified layer 3 distributed firewall section.  If the default layer 3 firewall section is selected, the request is rejected. See `GET /api/4.0/firewall/globalroot-0/defaultconfig` for information on resetting the default firewall section.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method updated. When deleting the default firewall rule section, the method previously removed all rules except for the default rule. The method now returns status 400 and the message `Cannot delete default section <sectionId>`.  -}
  , dELETE40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRulesRuleId :: Text -> Text -> Maybe Text -> m (){- ^ Delete the specified distributed firewall rule. -}
  , dELETE40FirewallGlobalroot0DraftsDraftID :: Text -> m (){- ^ Delete a configuration. -}
  , dELETE40FirewallGlobalroot0TimeoutsConfigId :: Text -> m (){- ^ Delete the specified Distributed Firewall session timer configuration.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , dELETE40ServicesSpoofguardPoliciesPolicyID :: Text -> m (){- ^ Delete the specified SpoofGuard policy. -}
  , gET10ApplianceManagementBackuprestoreBackups :: m (){- ^ Retrieve list of all backups available at configured backup location.  -}
  , gET10ApplianceManagementBackuprestoreBackupsettings :: m (){- ^ Retrieve backup settings. -}
  , gET10ApplianceManagementCertificatemanagerCertificatesNsx :: m (){- ^ Retrieve certificate information from NSX Manager.  -}
  , gET10ApplianceManagementCertificatemanagerCsrNsx :: m (){- ^ Retrieve generated certificate signing request (CSR).  -}
  , gET10ApplianceManagementComponents :: m (){- ^ Retrieve all appliance manager components. -}
  , gET10ApplianceManagementComponentsComponentComponentID :: Text -> m (){- ^ Retrieve details for the specified component. -}
  , gET10ApplianceManagementComponentsComponentComponentIDDependencies :: Text -> m (){- ^ Retrieve dependency details for the specified component. -}
  , gET10ApplianceManagementComponentsComponentComponentIDDependents :: Text -> m (){- ^ Retrieve dependents for the specified component. -}
  , gET10ApplianceManagementComponentsComponentComponentIDStatus :: Text -> m (){- ^ Retrieve current status for the specified component. -}
  , gET10ApplianceManagementGlobalInfo :: m (){- ^ Retrieve global information containing version information as well as current logged in user.  -}
  , gET10ApplianceManagementNotifications :: m (){- ^ Retrieve all system generated notifications. -}
  , gET10ApplianceManagementSummaryComponents :: m (){- ^ Retrieve summary of all available components and their status info.  -}
  , gET10ApplianceManagementSummarySystem :: m (){- ^ Retrieve system summary info such as address, DNS name, version, CPU, memory and storage.  -}
  , gET10ApplianceManagementSystemCpuinfo :: m (){- ^ Retrieve NSX Manager Appliance CPU information.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. Added **cpuUsageIndicator** parameter.  -}
  , gET10ApplianceManagementSystemCpuinfoDetails :: m (){- ^ Retrieve details about CPU utilization for the NSX Manager Appliance.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET10ApplianceManagementSystemLocale :: m (){- ^ Retrieve locale info. -}
  , gET10ApplianceManagementSystemMeminfo :: m (){- ^ Retrieve NSX Manager memory information. -}
  , gET10ApplianceManagementSystemNetwork :: m (){- ^ Retrieve network information for the NSX Manager appliance. i.e. host name, IP address, DNS settings.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. New parameter *dynamicIPAddress* added. The parameter tells whether the IP address of the NSX Appliance Manager is dynamically allocated or not. If *dynamicIPAddress* parameter is *true*, then the Unconfigure ipv4/ipv6 button on the UI is disabled.  -}
  , gET10ApplianceManagementSystemSecuritysettings :: m (){- ^ Retrieve the NSX Manager FIPS and TLS settings.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , gET10ApplianceManagementSystemStorageinfo :: m (){- ^ Retrieve NSX Manager storage information. -}
  , gET10ApplianceManagementSystemSyslogserver :: m (){- ^ Retrieves only the first syslog server among the servers configured. -}
  , gET10ApplianceManagementSystemSyslogservers :: m (){- ^ Retrieves all syslog servers configured on the NSX Manager.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET10ApplianceManagementSystemTimesettings :: m (){- ^ Retrieve time settings, like timezone or current date and time with NTP server, if configured.  -}
  , gET10ApplianceManagementSystemTlssettings :: m (){- ^ Retrieve TLS settings.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET10ApplianceManagementSystemUptime :: m (){- ^ Retrieve NSX Manager uptime information.  **Example response:** ``` 25 days, 22 hours, 11 minutes ```  -}
  , gET10ApplianceManagementTechsupportlogsFilename :: Text -> m (){- ^ Download tech support logs. -}
  , gET10ApplianceManagementUpgradeInformationComponentID :: Text -> m (){- ^ Once you have uploaded an upgrade bundle, you must retrieve information about the upgrade. This request contains pre-upgrade validation warnings and error messages, along with pre-upgrade questions.   You use the **preUpgradeQuestionsAnswers** section with the addition of your answers to create the request body for the `POST /api/1.0/appliance-management/upgrade/start/{componentID}` request to start the backup.  See *Start the NSX Manager Upgrade* for more information.  -}
  , gET10ApplianceManagementUpgradeStatusComponentID :: Text -> m (){- ^ Query upgrade status. -}
  , gET10DirectoryListDomains :: m (){- ^ Retrieve all agent discovered (or configured) LDAP domains. -}
  , gET10DirectoryListEventLogServersForDomainDomainID :: Text -> m (){- ^ Query EventLog servers for a domain. -}
  , gET10DirectoryListLdapServersForDomainDomainID :: Text -> m (){- ^ Query LDAP servers for a domain. -}
  , gET10EventcontrolConfigVmVmID :: Text -> m (){- ^ Retrieve per VM configuration for data collection.  -}
  , gET10IdentityDirectoryGroupsForUser :: Maybe Text -> Maybe Text -> Text -> m (){- ^ Query set of Windows Domain Groups (AD Groups) to which the specified user belongs.  -}
  , gET10IdentityHostIpMapping :: m (){- ^ Query host-to-ip mapping list from database. -}
  , gET10IdentityIpToUserMapping :: m (){- ^ Retrieve set of users associated with a given set of IP addresses during a specified time period. Since more than one user can be associated with a single IP address during the specified time period, each IP address can be associated with zero or more (i.e a SET of) users.  -}
  , gET10IdentityStaticUserMappings :: m (){- ^ Query static user IP mapping list. -}
  , gET10IdentityStaticUserMappingsbyIPIP :: Text -> m (){- ^ Query static user IP mapping for specified IP. -}
  , gET10IdentityStaticUserMappingsbyUserUserID :: Text -> m (){- ^ Query static user IP mapping for specified user. -}
  , gET10IdentityUserIpMapping :: m (){- ^ Query user-to-ip mapping list from database. -}
  , gET10TelemetryConfig :: m (){- ^ Retrieve the CEIP configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.   -}
  , gET10TelemetryProxy :: m (){- ^ Retrieve the NSX Manager proxy settings for CEIP.  **Method history:**  Release | Modification --------|------------- 6.3.3 | Method introduced.   -}
  , gET20Auditlog :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m (){- ^ Get NSX Manager audit logs. -}
  , gET20CapacityParametersReport :: m (){- ^ The output displays scale summary, current scale value, supported system scale value,  threshold value, and the percentage usage for each parameter.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20CapacityParametersThresholds :: m (){- ^ Retrieves the global threshold for the system. The System Scale dashboard on UI displays warning when the  threshold value is reached.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20EamStatus :: m (){- ^ Retrieve EAM status from vCenter.  You can verify the status is UP before proceeding with an NSX install or upgrade.  **Method history:**  Release | Modification --------|------------- 6.3.5 | Method introduced.  -}
  , gET20EndpointsecurityActivation :: Maybe Text -> m (){- ^ Retrieve activation information for all activated security VMs on the specified host.  -}
  , gET20EndpointsecurityActivationVendorIDAltitudeMoid :: Text -> Text -> Text -> m (){- ^ Retrieve the endpoint protection solution activation status, either true (activated) or false (not activated). -}
  , gET20EndpointsecurityActivationVendorIDSolutionID :: Text -> Text -> m (){- ^ Retrieve a list of activated security VMs for an endpoint protection solution.  -}
  , gET20EndpointsecurityRegistrationVendorID :: Text -> m (){- ^ Retrieve registration information for a Guest Introspection vendor. -}
  , gET20EndpointsecurityRegistrationVendorIDAltitude :: Text -> Text -> m (){- ^ Get registration information for an endpoint protection solution. -}
  , gET20EndpointsecurityRegistrationVendorIDAltitudeLocation :: Text -> Text -> m (){- ^ Get the IP address and port on the vNIC host for an endpoint protection solution.  -}
  , gET20EndpointsecurityRegistrationVendorIDSolutions :: Text -> m (){- ^ Get registration information for all endpoint protection solutions for a Guest Introspection vendor.  -}
  , gET20EndpointsecurityRegistrationVendors :: m (){- ^ Retrieve the list of all registered Guest Introspection vendors. -}
  , gET20EndpointsecurityUsvmstatsUsvmhealththresholds :: m (){- ^ Retrieve Guest Introspection service VM CPU and memory usage thresholds.  **Method history:**  Release | Modification --------|------------- 6.3.5 | Method introduced.  -}
  , gET20Hostevents :: m (){- ^ Retrieve configuration of host event notifications.    **Method history:**    Release | Modification   --------|------------- 6.4.0 |  Method added.  -}
  , gET20NwfabricClustersClusterID :: Text -> m (){- ^ Retrieve the locale ID for the specified cluster. -}
  , gET20NwfabricFeatures :: m (){- ^ Retrieves all network fabric features available on the cluster. Multiple **featureInfo** sections may be returned.  -}
  , gET20NwfabricHostsHostID :: Text -> m (){- ^ Retrieve the locale ID for the specified host. -}
  , gET20NwfabricStatus :: Maybe Text -> m (){- ^ Retrieve the network fabric status of the specified resource.  -}
  , gET20NwfabricStatusAlleligibleResourceType :: Text -> m (){- ^ Retrieve status of resources by criterion.  -}
  , gET20NwfabricStatusChildParentResourceID :: Text -> m (){- ^ Retrieve the network fabric status of child resources of the specified resource.  -}
  , gET20ServicesAlarmsSourceId :: Text -> m (){- ^ Retrive all alarms from the specified source.  -}
  , gET20ServicesApplicationApplicationId :: Text -> m (){- ^ Retrieve details about the specified service.   -}
  , gET20ServicesApplicationScopeScopeId :: Text -> m (){- ^ Retrieve services that have been created on the specified scope. -}
  , gET20ServicesApplicationgroupApplicationgroupId :: Text -> m (){- ^ Retrieve details about the specified service group. -}
  , gET20ServicesApplicationgroupScopeScopeId :: Text -> m (){- ^ Retrieve a list of service groups that have been created on the scope.  -}
  , gET20ServicesApplicationgroupScopeScopeIdMembers :: Text -> m (){- ^ Get a list of member elements that can be added to the service groups created on a particular scope.  -}
  , gET20ServicesAuthTokenexpiration :: m (){- ^ Retrieve the default token expiry time.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.   -}
  , gET20ServicesConfiguration :: m (){- ^ Get the configuration details for the High CPU Usage Reporting Tool. -}
  , gET20ServicesDashboardUiViewsDashboardWidgetconfigurations :: m (){- ^ Retrieves configuration details for all the widgets available on dashboard.   **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20ServicesDashboardUiViewsDashboardWidgetconfigurationsWidgetconfigurationId :: Text -> m (){- ^ Retrieves the configuration details about a specific widget on the dashboard.   **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20ServicesHousekeepingManagementIndexMaintenance :: m (){- ^ Retrieve the default settings for the index maintenance activities.  **Method history:**  Release | Modification --------|------------- 6.3.3 | Method introduced.  -}
  , gET20ServicesIpamPoolsPoolId :: Text -> m (){- ^ Retrieve details about a specific IP pool. -}
  , gET20ServicesIpamPoolsPoolIdIpaddresses :: Text -> m (){- ^ Retrieves all allocated IP addresses from the specified pool.  -}
  , gET20ServicesIpamPoolsScopeScopeId :: Text -> m (){- ^ Retrieves all IP pools on the specified scope where the **scopeId** is the reference to the desired scope. An example of the **scopeID** is *globalroot-0*.  -}
  , gET20ServicesIpsetIpsetId :: Text -> m (){- ^ Retrieve an individual IP set. -}
  , gET20ServicesIpsetScopeScopeMoref :: Text -> m (){- ^ Retrieve all configured IPSets -}
  , gET20ServicesLicensingCapacityusage :: m (){- ^ Retrieve capacity usage information on the usage of CPUs, VMs and concurrent users for the distributed firewall and VXLAN.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20ServicesLicensingStatus :: m (){- ^ Retrieve details about the assigned NSX license.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20ServicesMacsetMacsetId :: Text -> m (){- ^ Retrieve details about a MAC address set. -}
  , gET20ServicesMacsetScopeScopeId :: Text -> m (){- ^ List MAC address sets on the specified scope. -}
  , gET20ServicesPolicySecurityactionCategoryVirtualmachines :: Text -> Maybe Text -> Maybe Text -> m (){- ^ Retrieve all VirtualMachine objects on which security action of a given category and attribute has been applied.  -}
  , gET20ServicesPolicySecuritygroupIDSecurityactions :: Text -> m (){- ^ Retrieve all security actions applicable on a security group for all ExecutionOrderCategories. The list is sorted based on the weight of security actions in descending order.  The **isActive** tag indicates if a securityaction will be applied (by the enforcement engine) on the security group.  -}
  , gET20ServicesPolicySecuritygroupIDSecuritypolicies :: Text -> m (){- ^ Retrieve security policies mapped to a security group.  The list is sorted based on the precedence of security policy precedence in descending order. The security policy with the highest precedence (highest numeric value) is the first entry (index = 0) in the list.  -}
  , gET20ServicesPolicySecuritypolicyAlarmsAll :: m (){- ^ Retrieve all system alarms that are raised at Service Composer level and policy level.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20ServicesPolicySecuritypolicyAll :: Maybe Text -> Maybe Text -> m (){- ^ Retrieve information for all security policies.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. Output is now paginated. **pageSize** and **startIndex** query parameters added.  -}
  , gET20ServicesPolicySecuritypolicyHierarchy :: Maybe Text -> Maybe Text -> m (){- ^ Export a Service Composer configuration (along with the security groups to which the security policies are mapped). You can save the response to a file.  The saved configuration can be used as a backup for situations where you may accidentally delete a policy configuration, or it can be exported for use in another NSX Manager environment.  If a prefix is specified, it is added before the names of the security policy, security action, and security group objects in the exported XML. The prefix can thus be used to indicate the remote source from where the hierarchy was exported.  -}
  , gET20ServicesPolicySecuritypolicyID :: Text -> m (){- ^ Retrieve security policy information. To view all security policies, specify *all* as the security policy ID.  -}
  , gET20ServicesPolicySecuritypolicyIDSecurityactions :: Text -> m (){- ^ Retrieve all security actions applicable on a security policy.  This list includes security actions from associated parent security policies, if any. Security actions per Execution Order Category are sorted based on the weight of security actions in descending order.  -}
  , gET20ServicesPolicySecuritypolicyMaxprecedence :: m (){- ^ Retrieve the highest precedence (or weight) of the Service Composer security policies.  The response body contains only the maximum precedence.  Example:  ``` 6300 ```  -}
  , gET20ServicesPolicySecuritypolicyServiceproviderFirewall :: m (){- ^ Retrieve the Service Composer firewall applied to setting.  -}
  , gET20ServicesPolicySecuritypolicyStatus :: m (){- ^ Retrieve the consolidated status of Service Composer.  The possible return of value for status are: *in_sync*, *in_progress*, *out_of_sync*, and *pending*.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20ServicesPolicyServiceproviderFirewall :: Text -> m (){- ^ **Deprecated.** Use `GET /api/2.0/services/serviceprovider/firewall/info` instead.  You can also use `GET /api/2.0/services/policy/securitypolicy/status/` to retrieve the sync status of Service Composer firewall with Distributed Firewall.  This GET method can perform certain functions, depending on the request body provided. **Note:** Some REST clients do not allow you to specify a request body with a GET request.  **Method history:**    Release | Modification   --------|-------------   6.2.3 | Method updated and some functions deprecated. Changing auto save draft with the **autoSaveDraft** parameter is deprecated, and will be removed in a future release.  <br>The default setting of **autoSaveDraft** is changed from *true* to *false*.<br>Method to check if Service Composer and Distributed Firewall are in sync is deprecated, and will be removed in a future release. Use `GET /api/2.0/services/policy/securitypolicy/status/` instead.   6.4.0 | All functions deprecated. Use `GET /api/2.0/services/serviceprovider/firewall/info` instead.  -}
  , gET20ServicesPolicyServiceproviderFirewallInfo :: Maybe Text -> Maybe Text -> m (){- ^ If Service Composer goes out of sync with Distributed Firewall, you must re-synchronize Service Composer rules with firewall rules. If Service Composer stays out of sync, firewall configuration may not stay enforced as expected.  Using query parameters, you can get the sync status, force a sync, and retrieve or update the auto save draft propertly.   You can also use `GET /api/2.0/services/policy/securitypolicy/status/` to retrieve the sync status of Service Composer firewall with distributed Firewall.  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20ServicesPolicyVirtualmachineIDSecurityactions :: Text -> m (){- ^ You can retrieve the security actions applicable on a virtual machine for all ExecutionOrderCategories. The list of SecurityActions per ExecutionOrderCategory is sorted based on the weight of security actions in descending order. The **isActive** tag indicates whether a security action will be applied (by the enforcement engine) on the virtual machine.  -}
  , gET20ServicesSecuritygroupInternalScopeScopeId :: Text -> m (){- ^ Retrieve all internal security groups on the NSX Manager. These are used  internally by the system and should not be created or modified by end users.  -}
  , gET20ServicesSecuritygroupLookupIpaddressIpAddress :: Text -> Text -> m (){- ^ Retrieve all the security groups that contain the specified IP address.  -}
  , gET20ServicesSecuritygroupLookupVirtualmachineVirtualMachineId :: Text -> m (){- ^ Retrieves the collection of security groups to which a virtual machine is a direct or indirect member. Indirect membership involves nesting of security groups.  -}
  , gET20ServicesSecuritygroupObjectId :: Text -> m (){- ^ Retrieve all members of the specified security group. -}
  , gET20ServicesSecuritygroupObjectIdTranslationIpaddresses :: Text -> m (){- ^ Retrieve list of IP addresses that belong to a specific security group.  -}
  , gET20ServicesSecuritygroupObjectIdTranslationMacaddresses :: Text -> m (){- ^ Retrieve list of MAC addresses that belong to a specific security group.  -}
  , gET20ServicesSecuritygroupObjectIdTranslationVirtualmachines :: Text -> m (){- ^ Retrieve effective membership of a security group in terms of virtual machines. The effective membership is calculated using all the three membership components of a security group - static include, static exclude, and dynamic using the following formula:    Effective membership virtual machines = [ (VMs resulting from static include component + VMs resulting from dynamic component) - (VMs resulting from static exclude component) ]  -}
  , gET20ServicesSecuritygroupObjectIdTranslationVnics :: Text -> m (){- ^ Retrieve list of vNICs that belong to a specific security group.  -}
  , gET20ServicesSecuritygroupScopeScopeId :: Text -> m (){- ^ List all the security groups created on a specific scope. -}
  , gET20ServicesSecuritygroupScopeScopeIdMemberTypes :: Text -> m (){- ^ Retrieve a list of valid elements that can be added to a security group.  -}
  , gET20ServicesSecuritygroupScopeScopeIdMembersMemberType :: Text -> Text -> m (){- ^ Retrieve members of a specific type in the specified scope. -}
  , gET20ServicesSecuritytagsSelectionCriteria :: m (){- ^ Retrieve unique ID section criteria configuration.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , gET20ServicesSecuritytagsTag :: Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m (){- ^ Retrieve all security tags.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method updated. Added **isUniversal** query parameter to filter universal security tags. 6.3.3 | Method updated. Output is now paginated. **startIndex**, **pageSize**, **sortOrderAscending**, **sortBy**, **filterBy**, and **filterValue** query parameters added.  -}
  , gET20ServicesSecuritytagsTagTagIdVm :: Text -> m (){- ^ Retrieve the list of VMs that have the specified tag attached to them.  -}
  , gET20ServicesSecuritytagsTagTagIdVmDetail :: Text -> m (){- ^ Retrieve details about the VMs that are attached to the specified security tag.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , gET20ServicesSecuritytagsVmVmId :: Text -> m (){- ^ Retrieve all security tags associated with the specified virtual machine.  -}
  , gET20ServicesSnmpManager :: m (){- ^ Retrieve information about SNMP managers.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20ServicesSnmpManagerManagerId :: Text -> m (){- ^ Retrieve information about the specified SNMP manager.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20ServicesSnmpStatus :: m (){- ^ Retrieve SNMP status settings.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20ServicesSnmpTrap :: m (){- ^ Retrieve information about SNMP traps.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20ServicesSnmpTrapOid :: Text -> m (){- ^ Retrieve information about the specified SNMP trap.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20ServicesSsoconfig :: m (){- ^ Retrieve SSO Configuration. -}
  , gET20ServicesSsoconfigStatus :: m (){- ^ Retrieve the SSO configuration status of NSX Manager. -}
  , gET20ServicesSystemalarms :: Maybe Text -> Maybe Int -> Maybe Bool -> Maybe Int -> m (){- ^ Retrieve all unresolved alarms on NSX Manager.  **Method history:**  Release | Modification --------|------------- 6.3.3 | Method introduced.  -}
  , gET20ServicesSystemalarmsAlarmId :: Text -> m (){- ^ Retrieve information about the specified alarm. Both resolved and unresolved alarms can be retrieved.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , gET20ServicesTaskserviceJob :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> m (){- ^ Query job instances by criterion. -}
  , gET20ServicesTaskserviceJobJobId :: Text -> m (){- ^ Retrieve all job instances for the specified job ID. -}
  , gET20ServicesTranslationVirtualmachineVmIdIpaddresses :: Text -> m (){- ^ Retrieve IP addresses of the provided virtual machine.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20ServicesTruststoreConfig :: m (){- ^ View certificate expiry notification duration in days. This API is available for all roles.  **Method history:**  Release | Modification --------|------------- 6.4.1 | Method introduced.  -}
  , gET20ServicesTruststoreConfigCertificateId :: Text -> m (){- ^ Retrieve the certificate object specified by ID. If the ID specifies a chain, multiple certificate objects are retrieved.  -}
  , gET20ServicesTruststoreConfigScopeScopeId :: Text -> m (){- ^ Retrieve all certificates on the specified scope. -}
  , gET20ServicesTruststoreCrlCrlId :: Text -> m (){- ^ Retrieve certificate object for the specified certificate revocation list (CRL).  -}
  , gET20ServicesTruststoreCrlScopeScopeId :: Text -> m (){- ^ Retrieve all certificates for the specified scope. -}
  , gET20ServicesTruststoreCsrCsrId :: Text -> m (){- ^ Retrieve the specified certificate signing request (CSR).  -}
  , gET20ServicesTruststoreCsrScopeScopeId :: Text -> m (){- ^ Retrieve certificate signing requests (CSR) on the specified scope. -}
  , gET20ServicesUsermgmtRoleUserId :: Text -> m (){- ^ Retrieve a user's role (possible roles are *super_user*, *vshield_admin*, *enterprise_admin*, *security_admin*, and *audit*).  -}
  , gET20ServicesUsermgmtRoles :: m (){- ^ Read all possible roles in NSX Manager -}
  , gET20ServicesUsermgmtScopingobjects :: m (){- ^ Retrieve a list of objects that can be used to define a user's access scope.  -}
  , gET20ServicesUsermgmtUserUserId :: Text -> m (){- ^ Get information about a user. -}
  , gET20ServicesUsermgmtUsersVsm :: m (){- ^ Get information about users who have been assigned a NSX Manager role (local users as well as vCenter users with NSX Manager role).  -}
  , gET20ServicesVcconfig :: m (){- ^ Get vCenter Server configuration details on NSX Manager. -}
  , gET20ServicesVcconfigStatus :: m (){- ^ Get default vCenter Server connection status. -}
  , gET20SiAgentAgentID :: Text -> m (){- ^ Retrieve agent (host components and appliances) details.  -}
  , gET20SiDeployClusterClusterID :: Text -> m (){- ^ Retrieve all services deployed along with their status. -}
  , gET20SiDeployClusterClusterIDServiceServiceID :: Text -> Text -> m (){- ^ Retrieve detailed information about the service. -}
  , gET20SiDeployServiceServiceID :: Text -> m (){- ^ Retrieve all clusters on which the service is installed. -}
  , gET20SiDeployServiceServiceIDDependsOn :: Text -> m (){- ^ Retrieve service on which the specified service depends.  -}
  , gET20SiDeploymentDeploymentunitIDAgents :: Text -> m (){- ^ Retrieve all agents for the specified deployment. -}
  , gET20SiFabricSyncConflicts :: m (){- ^ Retrieve conflicting deployment units and EAM agencies, if any, and the allowed operations on them.  -}
  , gET20SiHostHostIDAgents :: Text -> m (){- ^ Retrieves all agents on the specified host. The response body contains agent IDs for each agent, which you can use to retrieve details about that agent.  -}
  , gET20SystemMonitorCpuusageDetails :: m (){- ^ Retrieve the details of the module which is causing high CPU utilization for the NSX Manager. -}
  , gET20SystemMonitorCpuusageIndicator :: m (){- ^ Retrieve the CPU utilization status and the CPU usage percentage. -}
  , gET20Systemevent :: Maybe Text -> Maybe Text -> Text -> m (){- ^ Get NSX Manager system events   **Method history:**  Release | Modification --------|------------- 6.4.0 |  Method updated. New parameters **eventSourceId**, **eventSourceType**, **eventSourceIP** added under **eventSourceInfo**.  -}
  , gET20TechsupportbundleFilename :: Text -> m (){- ^ You can use the filename to download the support bundle. You can get the file name from the */techsupportbundle/status* API.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20TechsupportbundleStatus :: Text -> m (){- ^ Retrieves the status of the technical support bundle.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20UniversalsyncConfigurationNsxmanagers :: m (){- ^ If run on a primary NSX Manager, it will list secondary NSX Managers configured on the primary NSX Manager.  If run on a secondary NSX Manager, it will list information about the secondary NSX Manager and the primary NSX Manager it is associated with.  -}
  , gET20UniversalsyncConfigurationNsxmanagersNsxManagerID :: Text -> m (){- ^ Retrieve information about the specified secondary NSX Manager.  -}
  , gET20UniversalsyncConfigurationRole :: m (){- ^ Retrieve the universal sync configuration role. -}
  , gET20UniversalsyncEntitystatus :: Maybe Text -> Maybe Text -> m (){- ^ Retrieve the status of a universal sync entity. -}
  , gET20UniversalsyncStatus :: m (){- ^ Retrieve the universal sync status. -}
  , gET20VdnBfdConfigurationGlobal :: Text -> m (){- ^ Retrieve the BFD global configuration.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20VdnCdo :: m (){- ^ Retrieves the status of CDO mode.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20VdnConfigMulticasts :: m (){- ^ Retrieve information about all configured multicast address ranges.  Universal multicast address ranges have the property isUniversal set to *true*.  -}
  , gET20VdnConfigMulticastsMulticastAddresssRangeId :: Text -> m (){- ^ Retrieve information about the specified multicast address range.  -}
  , gET20VdnConfigResourcesAllocated :: Maybe Text -> Maybe Text -> Maybe Text -> m (){- ^ Retrieve information about allocated segment IDs or multicast addresses.  -}
  , gET20VdnConfigSegments :: m (){- ^ Retrieve information about all segment ID pools.  -}
  , gET20VdnConfigSegmentsSegmentPoolId :: Text -> m (){- ^ Retrieve information about the specified segment ID pool.  -}
  , gET20VdnConfigVxlanUdpPort :: m (){- ^ Retrieve the UDP port configured for VXLAN traffic.  -}
  , gET20VdnConfigVxlanUdpPortTaskStatus :: m (){- ^ Retrieve the status of the VXLAN port configuration update.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20VdnController :: m (){- ^ Retrieves details and runtime status for all controllers.  Runtime status can be one of the following:    * **Deploying** - controller is being deployed and the procedure has not   completed yet.   * **Removing** - controller is being removed and the procedure has not   completed yet.   * **Running** - controller has been deployed and can respond to API   invocation.   * **Unknown** - controller has been deployed but fails to respond to API   invocation.  -}
  , gET20VdnControllerCluster :: m (){- ^ Retrieve cluster wide configuration information for controller.  -}
  , gET20VdnControllerClusterNtp :: m (){- ^ Retrieve NTP configuration for the NSX Controller cluster.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20VdnControllerControllerIdSnapshot :: Text -> m (){- ^ Take a snapshot of the control cluster from the specified controller node.  -}
  , gET20VdnControllerControllerIdSyslog :: Text -> m (){- ^ Retrieve details about the syslog exporter on the controller.  -}
  , gET20VdnControllerControllerIdSystemStats :: Text -> m (){- ^ Retrieve NSX Controller system statistics.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20VdnControllerControllerIdTechsupportlogs :: Text -> Maybe Text -> m (){- ^ Retrieve controller logs. Response content type is application/octet-stream and response header is filename. This streams a fairly large bundle back (possibly hundreds of MB).  -}
  , gET20VdnControllerProgressJobId :: Text -> m (){- ^ Retrieves status of controller creation or removal. The progress gives a percentage indication of current deploy / remove procedure.  -}
  , gET20VdnControllerSynchronizeStatus :: m (){- ^ Get the status of the controller synchronization.  If the sync is in progress, the response includes the status *JOB_IN_PROGRESS*, and the jobId. If the sync has finished, the response includes the status *NOT_RUNNING*.  -}
  , gET20VdnControllerUpgradeAvailable :: m (){- ^ Retrieve controller upgrade availability. -}
  , gET20VdnHardwaregatewayBfdConfig :: m (){- ^ Retrieve global hardware gateway BFD configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20VdnHardwaregatewayBfdStatus :: m (){- ^ Retrieve hardware gateway BFD tunnel status for all tunnel endpoints, including hosts and hardware gateways.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20VdnHardwaregatewayBindings :: Maybe Text -> Maybe Int -> m (){- ^ Retrieve information about hardware gateway bindings.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20VdnHardwaregatewayBindingsBindingId :: Text -> m (){- ^ Retrieve information about the specified hardware gateway binding.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20VdnHardwaregatewayBindingsBindingIdStatistic :: Text -> m (){- ^ Retrieve statistics for the specified hardware gateway binding.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20VdnHardwaregateways :: m (){- ^ Retrieve information about all hardware gateways.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20VdnHardwaregatewaysHardwareGatewayId :: Text -> m (){- ^ Retrieve information about the specified hardware gateway.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20VdnHardwaregatewaysHardwareGatewayIdSwitches :: Text -> m (){- ^ Retrieve information about switches on the specified hardware gateway.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20VdnHardwaregatewaysHardwareGatewayIdSwitchesSwitchNameSwitchports :: Text -> Text -> m (){- ^ Retrieve information about the hardware gateway switch ports for the specified switch and hardware gateway.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20VdnHardwaregatewaysReplicationcluster :: m (){- ^ Retrieve information about the hardware gateway replication cluster.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20VdnHostHostIdRemoteHostStatus :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> m (){- ^ Retrieve status of all remote hosts with tunnel connections to the given host.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20VdnHostHostIdStatus :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> m (){- ^ Retrieve information about tunnel health status for a specific host.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20VdnHostHostIdTunnel :: Text -> Text -> m (){- ^ Retrieve tunnel connections for a specific host.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20VdnHostStatus :: Maybe Text -> Maybe Text -> Maybe Text -> Text -> m (){- ^ Retrieve  overall information about tunnel health of a hypervisor.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20VdnInventoryHostHostIdConnectionStatus :: Text -> m (){- ^ Retrieve the status of the specified host.  History:  Release | Modification --------|------------- 6.2.3 | Method updated. Introduced **hostToControllerConnectionErrors** array.<br>Deprecated **fullSyncCount** parameter. Parameter is still present, but always has value of -1.  -}
  , gET20VdnInventoryHostsConnectionStatus :: Maybe Text -> m (){- ^ Retrieve the status of a list of hosts.  Release | Modification --------|------------- 6.2.3 | Method updated. Introduced **hostToControllerConnectionErrors** array.<br>Deprecated **fullSyncCount** parameter. Parameter is still present, but always has value of -1.  -}
  , gET20VdnPnicCheckConfigurationGlobal :: Text -> m (){- ^ Get pNIC status information.   **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET20VdnScopes :: m (){- ^ Retrieve information about all transport zones (also known as network scopes).  **CDO mode state parameters (read-only)**  The CDO mode state shows the most recent CDO operation, and the status of that operation. The status can be: *UNKNOWN*, *PENDING*, *IN_PROGRESS*, *COMPLETED*, or *FAILED*.  Operation Type | Description  ----|---- *ENABLE* | Enable CDO mode on all distributed switches in the transport zone. *DISABLE* | Disable CDO mode on all distributed switches in the transport zone. *EXPAND* | Enable CDO mode on newly joined distributed swithes. *SHRINK* | Disable CDO mode on removed distributed switches. *CLEAN_UP* | Transport zone removed, clean up the CDO mode configuration from all distributed switches in the transport zone. *SYNC_ENABLE* | Repush CDO mode configuration data to all distributed switches in the scope *SYNC_DISABLE* | Remove CDO mode configuration from all distributed switches in the transport zone.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method updated. Output includes information about CDO mode. See *Working With Transport Zone CDO Mode* for more information.  -}
  , gET20VdnScopesScopeId :: Text -> m (){- ^ Retrieve information about the specified transport zone.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method updated. Output includes information about CDO mode. See *Working With Transport Zone CDO Mode* for more information.  -}
  , gET20VdnScopesScopeIdVirtualwires :: Text -> Maybe Text -> Maybe Text -> m (){- ^ Retrieve information about all logical switches in the specified transport zone (network scope).  -}
  , gET20VdnSwitches :: m (){- ^ Retrieve information about all vSphere Distributed Switches.  -}
  , gET20VdnSwitchesDatacenterDatacenterID :: Text -> m (){- ^ Retrieve information about all vSphere Distributed Switches in the specified datacenter.  -}
  , gET20VdnSwitchesVdsId :: Text -> m (){- ^ Retrieve information about the specified vSphere Distributed Switch.  -}
  , gET20VdnTraceflowTraceflowId :: Text -> m (){- ^ Query a specific Traceflow by *tracflowId* which is the value returned after executing the create Traceflow API call.  -}
  , gET20VdnTraceflowTraceflowIdObservations :: Text -> m (){- ^ Retrieve traceflow observations.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. New parameter **replicateType** added.  -}
  , gET20VdnVirtualwires :: Maybe Text -> Maybe Text -> Maybe Text -> m (){- ^ Retrieve information about all logical switches in all transport zones.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. Added *name* query parameter.  -}
  , gET20VdnVirtualwiresVirtualWireID :: Text -> m (){- ^ Retrieve information about the specified logical switch.  If the switch is a universal logical switch the **isUniversal** parameter is set to true in the response body.  -}
  , gET20VdnVirtualwiresVirtualWireIDHardwaregateways :: Text -> m (){- ^ Retrieve hardware gateway bindings for the specified logical switch.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET20XvsNetworksIDFeatures :: Text -> m (){- ^ Retrieve IP discovery and MAC learning information. -}
  , gET21AppExcludelist :: Maybe Bool -> m (){- ^ Retrieve the set of VMs in the exclusion list.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. Added query parameter *excludelist?listSystemResources=true* to list the system resources in the exclude list.  -}
  , gET21AppFlowConfig :: m (){- ^ Retrieve flow monitoring configuration. -}
  , gET21AppFlowFlowstats :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> m (){- ^ Retrieve flow statistics for a datacenter, port group, VM, or vNIC.  Response values for flow statistics: * **blocked** - indicates whether traffic is blocked:   * 0 - flow allowed   * 1 - flow blocked   * 2 - flow blocked by SpoofGuard * **protocol** - protocol in flow:   * 0 - TCP   * 1 - UDP   * 2 - ICMP * **direction** - direction of flow:   * 0 - to virtual machine   * 1 - from virtual machine * **controlDirection** - control direction for dynamic TCP traffic:   * 0 - source -> destination   * 1 - destination -> source  -}
  , gET21AppFlowFlowstatsInfo :: m (){- ^ Retrieve flow statistics meta-data.  This method retrieves the following information for each flow type: * minimum start time * maximum end time * total flow count  -}
  , gET30AiApp :: m (){- ^ Retrieve app details. -}
  , gET30AiAppAppID :: Text -> m (){- ^ Retrieve details for specific app. -}
  , gET30AiDesktoppool :: m (){- ^ Retrieve list of all discovered desktop pools by agent introspection.  -}
  , gET30AiDesktoppoolDesktoppoolID :: Text -> m (){- ^ Retrieve specific desktop pool details. -}
  , gET30AiDirectorygroup :: m (){- ^ Retrieve list of all discovered (and configured) LDAP directory groups.  -}
  , gET30AiDirectorygroupDirectorygroupID :: Text -> m (){- ^ Retrieve details about a specific directory group. -}
  , gET30AiDirectorygroupUserUserID :: Text -> m (){- ^ Retrieve Active Directory groups that user belongs to. -}
  , gET30AiHost :: m (){- ^ Retrieve list of all discovered hosts (both by agent introspection and LDAP Sync) and their detail.  -}
  , gET30AiHostHostID :: Text -> m (){- ^ Get host details. -}
  , gET30AiRecords :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m (){- ^ ### View Outbound Activity  You can view what applications are being run by a security group or desktop pool and then drill down into the report to find out which client applications are making outbound connections by a particular group of users. You can also discover all user groups and users who are accessing a particular application, which can help you determine if you need to adjust identity firewall in your environment.  * query=*resource* * param=&lt;param-name&gt;:&lt;param-type&gt;:&lt;comma-separated-values&gt;:&lt;operator&gt;, where:   * &lt;param-name&gt; is one of:     * *src* (required)     * *dest* (required)     * *app*   * &lt;param-type&gt; is one of:     * for src: *SECURITY_GROUP*, *DIRECTORY_GROUP*, *DESKTOP_POOL*     * for dest: *VIRTUAL_MACHINE*     * for app: *SRC_APP*   * &lt;comma-separated-values&gt; is a comma-separated numbers (optional). If none specified then no filter is applied.   * &lt;operator&gt; is one of *INCLUDE*, *EXCLUDE* (default is *INCLUDE*).  **Example:** View user activities to VM ID 1 originating from application ID 1   `GET /api/3.0/ai/records?query=resource&interval=60m&param=src:DIRECTORY_GROUP`   `&param=dest:VIRTUAL_MACHINE:1&param=app:SRC_APP:1`  ### View Inbound Activity  You can view all inbound activity to a server by desktop pool, security group, or AD group.  * query=*sam* * param=&lt;param-name&gt;:&lt;param-type&gt;:&lt;comma-separated-values&gt;:&lt;operator&gt;, where:   * &lt;param-name&gt; is one of:     * *src* (required)     * *dest* (required)     * *app*   * &lt;param-type&gt; is one of:     * for src: *SECURITY_GROUP*, *DIRECTORY_GROUP*, *DESKTOP_POOL*     * for dest: *VIRTUAL_MACHINE*     * for app: *DEST_APP*   * &lt;comma-separated-values&gt; is a comma-separated numbers (optional). If none specified then no filter is applied.   * &lt;operator&gt; is one of *INCLUDE*, *EXCLUDE*, *NOT* (default is *INCLUDE*).  **Example:** View user activities to VM ID 1 originating from application ID 1   `GET /api/3.0/ai/records?query=containers&interval=60m&param=dest:SECURITY_GROUP:1:EXCLUDE`   `&param=src:SECURITY_GROUP:1`  ### View Interaction between Inventory Containers You can view the traffic passing between defined containers such as AD groups, security groups and/or desktop pools. This can help you identify and configure access to shared services and to resolve misconfigured relationships between Inventory container definitions, desktop pools and AD groups.  * query=*containers* * param=&lt;param-name&gt;:&lt;param-type&gt;:&lt;comma-separated-values&gt;:&lt;operator&gt;, where:   * &lt;param-name&gt; is one of:     * *src* (required)     * *dest* (required)   * &lt;param-type&gt; is one of:     * for src: *SECURITY_GROUP*, *DIRECTORY_GROUP*, *DESKTOP_POOL*     * for dest: *SECURITY_GROUP*, * *DESKTOP_POOL*    * &lt;comma-separated-values&gt; is a comma-separated numbers (optional). If none specified then no filter is applied.   * &lt;operator&gt; is one of *INCLUDE*, *EXCLUDE*, or *NOT* (default * is *INCLUDE*).  **Example:** View interaction between inventory containers   `GET /api/3.0/ai/records?query=containers&interval=60m&param=dest:SECURITY_GROUP:1:EXCLUDE`   `&param=src:SECURITY_GROUP:1`  ### View Outbound AD Group Activity  You can view the traffic between members of defined Active Directory groups and can use this data to fine tune your firewall rules.  * query=*adg* * param=&lt;param-name&gt;:&lt;param-type&gt;:&lt;comma-separated-values&gt;:&lt;operator&gt;, where:   * &lt;param-name&gt; is one of:     * *src* (required)     * *adg*   * &lt;param-type&gt; is one of:     * for src: *SECURITY_GROUP*, *DESKTOP_POOL*     * for adg: *USER*   * &lt;comma-separated-values&gt; is a comma-separated numbers (optional). If none specified then no filter is applied.   * &lt;operator&gt; is one of *INCLUDE*, *EXCLUDE* (default * is *INCLUDE*).  **Example:** View outbound AD group activity     `GET https://NSX-Manager-IP-Address/api/3.0/ai/records?query=adg&interval=24h&param=adg:USER:1:INCLUDE`   `&param=src:SECURITY_GROUP:1:EXCLUDE`  -}
  , gET30AiSecuritygroup :: m (){- ^ Retrieve list of all observed security groups.  Observed entities are the ones that are reported by the agents. For example, if a host activity is reported by an agent and if that host belongs to a security group then that security group would reported as observed in SAM database.  -}
  , gET30AiSecuritygroupSecgroupID :: Text -> m (){- ^ Retrieve details about specific security group. -}
  , gET30AiUserUserID :: Text -> m (){- ^ Retrieve details for a specific user. -}
  , gET30AiUserdetails :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m (){- ^ ### View Outbound Activity You can view what applications are being run by a security group or desktop pool and then drill down into the report to find out which client applications are making outbound connections by a particular group of users. You can also discover all user groups and users who are accessing a particular application, which can help you determine if you need to adjust identity firewall in your environment.  * query=*resource* * param=&lt;param-name&gt;&lt;param-type&gt;&lt;comma-separated-values&gt;&lt;operator&gt;, where:   * &lt;param-name&gt; is one of:     * *src* (required)     * *dest* (required)     * *app*   * &lt;param-type&gt; is one of:     * for src: *SECURITY_GROUP*, *DIRECTORY_GROUP*, *DESKTOP_POOL*     * for dest: *IP* - a valid IP address in dot notation, xx.xx.xx.xx     * for app: *SRC_APP*   * &lt;comma-separated-values&gt; is a comma-separated numbers (optional). If none specified then no filter is applied.   * &lt;operator&gt; is one of *INCLUDE*, *EXCLUDE* (default is *INCLUDE*).  **Example:** View user activities to VM ID 1 originating from application ID 1   `GET /api/3.0/ai/userdetails?query=resource&stime=2012-10-15T00:00:00&etime=2012-10-20T00:00:00`   `&param=src:DIRECTORY_GROUP:2&param=app:SRC_APP:16&param=dest:IP:172.16.4.52`  ### View Inbound Activity  You can view all inbound activity to a server by desktop pool, security group, or AD group.  * query=*sam* * param=&lt;param-name&gt;&lt;param-type&gt;&lt;comma-separated-values&gt;&lt;operator&gt;, where:   * &lt;param-name&gt; is one of:     * *src* (required)     * *dest* (required)     * *app* (required)   * &lt;param-type&gt; is one of:     * for src: *SECURITY_GROUP*, *DIRECTORY_GROUP*, *DESKTOP_POOL*     * for dest: *VIRTUAL_MACHINE*     * for app: *DEST_APP*   * &lt;comma-separated-values&gt; is a comma-separated numbers (optional). If none specified then no filter is applied.   * &lt;operator&gt; is one of *INCLUDE*, *EXCLUDE*, *NOT* (default is *INCLUDE*).  **Example:** View user activities to VM ID 1 originating from application ID 1   `GET /api/3.0/userdetails?query=sam&interval=60m&param=app:DEST_APP:1:EXCLUDE`   `&param=dest:IP:1:EXCLUDE&param=src:SECURITY_GROUP:1:EXCLUDE`  ### View Interaction between Inventory Containers You can view the traffic passing between defined containers such as AD groups, security groups and/or desktop pools. This can help you identify and configure access to shared services and to resolve misconfigured relationships between Inventory container definitions, desktop pools and AD groups.  * query=*containers* * param=&lt;param-name&gt;&lt;param-type&gt;&lt;comma-separated-values&gt;&lt;operator&gt;, where:   * &lt;param-name&gt; is one of:     * *src* (required)     * *dest* (required)   * &lt;param-type&gt; is one of:     * for src: *SECURITY_GROUP*, *DIRECTORY_GROUP*, *DESKTOP_POOL*     * for dest: *SECURITY_GROUP*, * *DESKTOP_POOL*    * &lt;comma-separated-values&gt; is a comma-separated numbers (optional). If none specified then no filter is applied.   * &lt;operator&gt; is one of *INCLUDE*, *EXCLUDE*, or *NOT* (default * is *INCLUDE*).  **Example:** View interaction between inventory containers   `GET /api/3.0/ai/userdetails?query=containers&interval=60m&param=dest:SECURITY_GROUP:1:EXCLUDE`   `&param=src:SECURITY_GROUP:1`  ### View Outbound AD Group Activity  You can view the traffic between members of defined Active Directory groups and can use this data to fine tune your firewall rules.  * query=*adg* * param=&lt;param-name&gt;&lt;param-type&gt;&lt;comma-separated-values&gt;&lt;operator&gt;, where:   * &lt;param-name&gt; is one of:     * *src* (required)     * *adg*   * &lt;param-type&gt; is one of:     * for src: *SECURITY_GROUP*, *DESKTOP_POOL*     * for adg: *USER*   * &lt;comma-separated-values&gt; is a comma-separated numbers (optional). If none specified then no filter is applied.   * &lt;operator&gt; is one of *INCLUDE*, *EXCLUDE* (default is *INCLUDE*).  **Example:** View outbound AD group activity     `GET /api/3.0/ai/userdetails?query=adg&interval=24h&param=adg:USER:1:INCLUDE`   `&param=src:SECURITY_GROUP:1:EXCLUDE`  ### View Virtual Machine Activity Report  * query=*vma* * param=&lt;param-name&gt;&lt;param-type&gt;&lt;comma-separated-values&gt;&lt;operator&gt;, where:   * &lt;param-name&gt; is one of:     * *src*     * *dst*     * *app*     * If no parameters are passed, then this would show all SAM     activities   * &lt;param-type&gt; is one of:     * for src: *SECURITY_GROUP*, *DESKTOP_POOL*     * for dst: *VIRTUAL_MACHINE*, *VM_UUID*     * for app - *SRC_APP* or *DEST_APP*   * &lt;comma-separated-values&gt; is a comma-separated numbers (optional). If none specified then no filter is applied.   * &lt;operator&gt; is one of *INCLUDE*, *EXCLUDE* (default is *INCLUDE*).  **Example:** View outbound AD group activity     `GET /api/3.0/ai/userdetails?query=vma&interval=60m&param=dest:VIRTUAL_MACHINE:1 &param=app:DEST_APP:16`  -}
  , gET30AiVm :: m (){- ^ Retrieve list of all discovered VMs. -}
  , gET30AiVmVmID :: Text -> m (){- ^ Retrieve details about a specific virtual machine. -}
  , gET40EdgePublishTuningConfiguration :: m (){- ^ Retrieve the NSX Edge tuning configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , gET40Edges :: Maybe Text -> Maybe Text -> Maybe Text -> m (){- ^ Retrieve a list of all NSX Edges in your inventory. You can use the query parameters to filter results.  -}
  , gET40EdgesEdgeId :: Text -> Maybe Bool -> m (){- ^ Retrieve information about the specified NSX Edge.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **haAdminState**, **configuredResourcePool**, **configuredDataStore**, **configuredHost**, **configuredVmFolder** parameters added.  6.4.0 | Method updated. New parameter **ipsecSessionType** added under the *site* section. This is a read-only parameter.  -}
  , gET40EdgesEdgeIdAppliances :: Text -> m (){- ^ Retrieve appliance configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **haAdminState**, **configuredResourcePool**, **configuredDataStore**, **configuredHost**, **configuredVmFolder** parameters added.   -}
  , gET40EdgesEdgeIdAppliancesHaIndex :: Text -> Text -> m (){- ^ Retrieve the configuration of the specified appliance.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **haAdminState**, **configuredResourcePool**, **configuredDataStore**, **configuredHost**, **configuredVmFolder** parameters added.   -}
  , gET40EdgesEdgeIdAutoconfiguration :: Text -> m (){- ^ Retrieve the auto configuration settings for the NSX Edge.  -}
  , gET40EdgesEdgeIdBridgingConfig :: Text -> m (){- ^ Retrieve bridge configuration. The value of the *enabled* field is always *true* for a Distributed Logical Router. -}
  , gET40EdgesEdgeIdDhcpConfig :: Text -> m (){- ^ Retrieve DHCP configuration.  **Method History**  Release | Modification --------|------------- 6.2.3 | Method updated. DHCP options added.  -}
  , gET40EdgesEdgeIdDhcpConfigBindings :: Text -> m (){- ^ Retrieve the multiple DHCP bindings with IP and MAC address.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.     -}
  , gET40EdgesEdgeIdDhcpConfigBindingsBindingID :: Text -> Text -> m (){- ^ Retrieve the specified static binding.  **Method history:**  Release | Modification --------|------------- 6.3.3 | Method introduced.     -}
  , gET40EdgesEdgeIdDhcpConfigRelay :: Text -> m (){- ^ Retrieve DHCP relay information. -}
  , gET40EdgesEdgeIdDhcpLeaseInfo :: Text -> m (){- ^ Get DHCP lease information. -}
  , gET40EdgesEdgeIdDnsConfig :: Text -> m (){- ^ Retrieve DNS configuration. -}
  , gET40EdgesEdgeIdDnsStatistics :: Text -> m (){- ^ Get DNS server statistics ----  **DNS Server Statistics Parameters**  Parameter Name | Parameter Information ------|----- **requests > total** | Indicates all of the incoming requests to the DNS server, including DNS query and other types of requests such as transfers, and updates.  **requests > queries** | Indicates all of the DNS queries the server received. **requests > total** | Indicates all of the responses the server returned to requests. This might be different from the requests.total because some requests might be rejected. total = success + nxrrset + servFail + formErr + nxdomain + others. **responses > success** | Indicates all of the successful DNS responses. **responses > nxrrset** | Indicates the count of no existent resource record.  **responses > servFail** | Indicates the count of the SERVFAIL responses. **responses > formErr** | Indicates the count of the format error responses. **responses > nxdomain** | Indicates the count of no-such-domain answer **responses > others** | Indicates the count of other types of responses.  -}
  , gET40EdgesEdgeIdFirewallConfig :: Text -> m (){- ^ Retrieve the NSX Edge firewall configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **enableSynFloodProtection** parameter added.  6.3.0 | Method updated. **logIcmpErrors** and **dropIcmpReplays** parameters added.   -}
  , gET40EdgesEdgeIdFirewallConfigDefaultpolicy :: Text -> m (){- ^ Retrieve default firewall policy -}
  , gET40EdgesEdgeIdFirewallConfigGlobal :: Text -> m (){- ^ Retrieve the firewall default policy for an Edge.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **enableSynFloodProtection** parameter added.  6.3.0 | Method updated. **logIcmpErrors** and **dropIcmpReplays** parameters added.   -}
  , gET40EdgesEdgeIdFirewallConfigRulesRuleId :: Text -> Text -> m (){- ^ Retrieve specific rule. -}
  , gET40EdgesEdgeIdFirewallStatisticsRuleId :: Text -> Text -> m (){- ^ Retrieve stats for firewall rule. -}
  , gET40EdgesEdgeIdHealthsummary :: Text -> m (){- ^ Retrieve detailed health information about an NSX Edge.   This includes features, VM health status, upgrade availability, alarms and pending jobs.   **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET40EdgesEdgeIdHighavailabilityConfig :: Text -> m (){- ^ Get high availability configuration. -}
  , gET40EdgesEdgeIdInterfaces :: Text -> m (){- ^ Retrieve all interfaces on the logical router. -}
  , gET40EdgesEdgeIdInterfacesIndex :: Text -> Text -> m (){- ^ Retrieve information about the specified logical router interface.  -}
  , gET40EdgesEdgeIdIpsecConfig :: Text -> Maybe Bool -> m (){- ^ Retrieve IPsec configuration.   **Method history:**    Release | Modification  --------|-------------  6.3.5 | Method updated. *showSensitiveData* query parameter added.   6.4.0 | Method updated. New parameters **ikeOptions** and **digestAlgorithm** added. New parameter **ipsecSessionType** added under the *site* section. This is a read-only parameter.  -}
  , gET40EdgesEdgeIdIpsecStatistics :: Text -> m (){- ^ Retrieve IPsec statistics.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. New parameter **channelIkeVersion** added under **IkeStatus** section. New parameters **failureMessage**, **packetsOut**, **packetSentErrors**, **encryptionFailures**, **sequenceNumberOverFlowErrors**, **packetsIn**, **packetReceivedErrors**, **decryptionFailures**, **replayErrorsintegrityErrors** added under **tunnelStatus** section. New parameter **siteId** added.  -}
  , gET40EdgesEdgeIdL2vpnConfig :: Text -> Maybe Bool -> m (){- ^ Retrieve the current L2VPN configuration for NSX Edge.  **Method history:**  Release | Modification --------|------------- 6.3.5 | Method updated. *showSensitiveData* query parameter added.   -}
  , gET40EdgesEdgeIdL2vpnConfigStatistics :: Text -> m (){- ^ Retrieve L2 VPN statistics, which has information such as tunnel status, sent bytes, received bytes for the specified Edge.  -}
  , gET40EdgesEdgeIdLoadbalancerConfig :: Text -> m (){- ^ Get load balancer configuration. -}
  , gET40EdgesEdgeIdLoadbalancerConfigApplicationprofiles :: Text -> m (){- ^ Retrieve all application profiles on the specified Edge. -}
  , gET40EdgesEdgeIdLoadbalancerConfigApplicationprofilesAppProfileID :: Text -> Text -> m (){- ^ Retrieve an application profile. -}
  , gET40EdgesEdgeIdLoadbalancerConfigApplicationrules :: Text -> m (){- ^ Retrieve all application rules. -}
  , gET40EdgesEdgeIdLoadbalancerConfigApplicationrulesAppruleID :: Text -> Text -> m (){- ^ Retrieve an application rule. -}
  , gET40EdgesEdgeIdLoadbalancerConfigMonitors :: Text -> m (){- ^ Retrieve all load balancer monitors. -}
  , gET40EdgesEdgeIdLoadbalancerConfigMonitorsMonitorID :: Text -> Text -> m (){- ^ Retrieve a load balancer monitor. -}
  , gET40EdgesEdgeIdLoadbalancerConfigPools :: Text -> m (){- ^ Get all server pools on the specified NSX Edge. -}
  , gET40EdgesEdgeIdLoadbalancerConfigPoolsPoolID :: Text -> Text -> m (){- ^ Retrieve information about the specified server pool. -}
  , gET40EdgesEdgeIdLoadbalancerConfigVirtualservers :: Text -> m (){- ^ Retrieve all virtual servers. -}
  , gET40EdgesEdgeIdLoadbalancerConfigVirtualserversVirtualserverID :: Text -> Text -> m (){- ^ Retrieve details for the specified virtual server. -}
  , gET40EdgesEdgeIdLoadbalancerStatistics :: Text -> m (){- ^ Retrieve load balancer statistics. -}
  , gET40EdgesEdgeIdMgmtinterface :: Text -> m (){- ^ Retrieve the management interface configuration for the logical router.  -}
  , gET40EdgesEdgeIdNatConfig :: Text -> m (){- ^ Retrieve NAT rules for the specified NSX Edge.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method updated. **dnatMatchSourceAddress**, **snatMatchDestinationAddress**, **dnatMatchSourcePort**, **snatMatchDestinationPort** parameters added. <br>**protocol**, **originalPort**, and **translatedPort** now supported in SNAT rules.  -}
  , gET40EdgesEdgeIdRoutingConfig :: Text -> Maybe Double -> Maybe Double -> m (){- ^ Retrieve routes.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **isis** configuration section removed.  6.3.0 | Method updated. Parameter **defaultOriginate** removed for logical router NSX Edges.  <br>Parameter **translateType7ToType5** added to OSPF section. <br>Parameters **localASNumber** and **remoteASNumber** added to BGP section. 6.4.0 | Method updated. Parameters **LE** and **GE** added. Parameter **removePrivateAS** added.  -}
  , gET40EdgesEdgeIdRoutingConfigBgp :: Text -> m (){- ^ Retrieve BGP configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **isis** configuration section removed.  6.3.0 | Method updated. Parameter **defaultOriginate** removed for logical router NSX Edges.  <br>Parameters **localASNumber** and **remoteASNumber** added to BGP section. 6.4.0 | Method updated. Parameter **removePrivateAS** added.  -}
  , gET40EdgesEdgeIdRoutingConfigGlobal :: Text -> Maybe Double -> Maybe Double -> m (){- ^ Retrieve routing info from NSX Manager database (default route settings, static route configurations).  -}
  , gET40EdgesEdgeIdRoutingConfigOspf :: Text -> m (){- ^ Retrieve OSPF configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **isis** configuration section removed.  6.3.0 | Method updated. Parameter **defaultOriginate** removed for logical router NSX Edges.  <br>Parameter **translateType7ToType5** added to OSPF section.   -}
  , gET40EdgesEdgeIdRoutingConfigStatic :: Text -> m (){- ^ Read static and default routes. -}
  , gET40EdgesEdgeIdSslvpnActivesessions :: Text -> m (){- ^ Retrieve a list of active clients for the SSL VPN session. -}
  , gET40EdgesEdgeIdSslvpnConfig :: Text -> m (){- ^ Retrieve SSL VPN details. -}
  , gET40EdgesEdgeIdSslvpnConfigAdvancedconfig :: Text -> m (){- ^ Retrieve SSL VPN advanced configuration. -}
  , gET40EdgesEdgeIdSslvpnConfigAuthLocalserverUsersUserID :: Text -> Text -> m (){- ^ Get information about the specified user. -}
  , gET40EdgesEdgeIdSslvpnConfigAuthSettings :: Text -> m (){- ^ Retrieve information about authentication settings. -}
  , gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionClientconfig :: Text -> m (){- ^ Retrieve client configuration. -}
  , gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages :: Text -> m (){- ^ Retrieve information about all installation packages. -}
  , gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackagesPackageID :: Text -> Text -> m (){- ^ Get information about the specified installation package.  -}
  , gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools :: Text -> m (){- ^ Retrieve all IP pools configured on SSL VPN. -}
  , gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppoolsIppoolID :: Text -> Text -> m (){- ^ Retrieve details of specified IP pool. -}
  , gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks :: Text -> m (){- ^ Retrieve all private network profiles in the SSL VPN instance.  -}
  , gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworksNetworkID :: Text -> Text -> m (){- ^ Retrieve the specified private network in the SSL VPN service.  -}
  , gET40EdgesEdgeIdSslvpnConfigLayout :: Text -> m (){- ^ Retrieve layout configuration. -}
  , gET40EdgesEdgeIdSslvpnConfigScript :: Text -> m (){- ^ Retrieve all script configurations. -}
  , gET40EdgesEdgeIdSslvpnConfigScriptFileID :: Text -> Text -> m (){- ^ Retrieve parameters associated with the specified script file.  -}
  , gET40EdgesEdgeIdSslvpnConfigServer :: Text -> m (){- ^ Retrieve server settings. -}
  , gET40EdgesEdgeIdStatisticsDashboardFirewall :: Text -> Maybe Text -> m (){- ^ Retrieve number of ongoing connections for the firewall configuration.  This API is not supported for Distributed Logical Routers.  -}
  , gET40EdgesEdgeIdStatisticsDashboardInterface :: Text -> Maybe Text -> m (){- ^ Retrieves dashboard statistics between the specified start and end times. When start and end time are not specified, all statistics since the Edge deployed are displayed. When no end time is specified, the current Edge Manager time is set as endTime. Each record has the stats of 5 minutes granularity. This API is not supported for Distributed Logical Routers.  -}
  , gET40EdgesEdgeIdStatisticsDashboardIpsec :: Text -> Maybe Text -> m (){- ^ Retrieve tunnel traffic statistics for specified time interval. This API is not supported for Distributed Logical Routers.  -}
  , gET40EdgesEdgeIdStatisticsDashboardSslvpn :: Text -> Maybe Text -> m (){- ^ Retrieve SSL VPN statistics on the specified NSX Edge. This API is not supported for Distributed Logical Routers.   -}
  , gET40EdgesEdgeIdStatisticsInterfaces :: Text -> m (){- ^ Retrieve interface statistics. -}
  , gET40EdgesEdgeIdStatisticsInterfacesInternal :: Text -> m (){- ^ Retrieve internal interface statistics. -}
  , gET40EdgesEdgeIdStatisticsInterfacesUplink :: Text -> m (){- ^ Retrieve uplink interface statistics. -}
  , gET40EdgesEdgeIdStatus :: Text -> Maybe Bool -> Maybe Bool -> Maybe Bool -> m (){- ^ Retrieve the status of the specified Edge.  The **edgeStatus** has the following possible states: * *GREEN*: Health checks are successful, status is good. * *YELLOW*: Intermittent health check failure. If health check fails   for five consecutive times for all appliances, status will turn   *RED*. * *GREY*: unknown status. * *RED*: None of the appliances for this NSX Edge are in a serving state.   **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. The **detailed** query parameter now specifies whether detailed info is displayed for **featureStatuses** only. Detailed info is now always displayed for **edgeVMStatus**. <br>The **systemStatus** parameter is deprecated, and might be removed in a future release.  -}
  , gET40EdgesEdgeIdSummary :: Text -> m (){- ^ Retrieve details about the specified NSX Edge.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method updated. **enableFips** parameter added to **appliancesSummary**.  -}
  , gET40EdgesEdgeIdSyslogConfig :: Text -> m (){- ^ Retrieve syslog servers information.  -}
  , gET40EdgesEdgeIdSystemcontrolConfig :: Text -> m (){- ^ Retrieve all NSX Edge system control configuration.  If no system control parameters are configured, the response is empty.  -}
  , gET40EdgesEdgeIdTechsupportlogs :: Text -> m (){- ^ Retrieve the tech support logs for the specified NSX Edge.  The response status for the tech support logs API request is `303 See Other`, and the **Location** header contains the file location of the tech support logs on the NSX Manager web server.  If your REST client is configured to not follow redirects, see the Location header to find the location of the tech support logs on the NSX Manager web server. You can retrieve the logs from `https://<nsxmgr-address>/<location>`.  Example in curl: ``` $ curl -k -i -s -H 'Authorization: Basic YWRtaW46Vk13YXJlMSE=' -H \"Content-Type: application/xml\" -H \"Accept: application/xml\" -X GET https://192.168.110.42/api/4.0/edges/edge-4/techsupportlogs HTTP/1.1 303 See Other Cache-Control: private Expires: Thu, 01 Jan 1970 00:00:00 GMT+00:00 Server: Cache-Control: no-cache Location: /tech_support_logs/vse/NSX_Edge_Support_edge-4_050217_155853GMT+00:00.log.gz Date: Tue, 02 May 2017 15:59:02 GMT Strict-Transport-Security: max-age=31536000; includeSubDomains X-Frame-Options: SAMEORIGIN Content-Length: 0 ```  In this example, the log location is `https://192.168.110.42/tech_support_logs/vse/NSX_Edge_Support_edge-4_050217_155853GMT+00:00.log.gz`  If your REST client is configured to follow redirects, the request retrieves the tech support log file from the file location in the **Location** field. Consult your REST client documentation for information on saving binary file responses.  Example in curl: ``` curl -k -L -s -H 'Authorization: Basic YWRtaW46ZGXXXXXXXX==' -H \"Content-Type: application/xml\" -H \"Accept: application/xml\" -X GET https://192.168.110.42/api/4.0/edges/edge-4/techsupportlogs > NSX_Edge_Support_edge-4.log.gz ```  -}
  , gET40EdgesEdgeIdTunnels :: Text -> m (){- ^ Retrieve information about all tunnels on this Edge Services Gateway.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET40EdgesEdgeIdTunnelsTunnelId :: Text -> Text -> m (){- ^ Retrieve information about the specified tunnel.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET40EdgesEdgeIdVnics :: Text -> m (){- ^ Retrieve all interfaces for the specified Edge Services Gateway.  -}
  , gET40EdgesEdgeIdVnicsIndex :: Text -> Text -> m (){- ^ Retrieve the specified interface. -}
  , gET40EdgesEdgeIdVnicsParentVnicIndexSubinterfacesSubInterfaceIndex :: Text -> Text -> Text -> Text -> m (){- ^ Retrieve the specified sub-interface.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , gET40EdgesJobs :: Maybe Text -> m (){- ^ Retrieve NSX Edge job status.  -}
  , gET40EdgesJobsJobId :: Text -> m (){- ^ Retrieve job status for the specified job.  -}
  , gET40FirewallConfigGlobalconfiguration :: m (){- ^ Retrieve performance configuration for distributed firewall.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict** in the global configuration is ignored. Instead, configure **tcpStrict** at the section level.  -}
  , gET40FirewallGlobalroot0Config :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> m (){- ^ Retrieve distributed firewall rule configuration.  If no query parameters are used, all rule configuration is retrieved. Use the query parameters to filter the rule configuration information.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict**, **stateless**, and **useSid** added as **section** attributes.  -}
  , gET40FirewallGlobalroot0ConfigIpfix :: m (){- ^ Retrieve IPFIX configuration. -}
  , gET40FirewallGlobalroot0ConfigLayer2sections :: Maybe Text -> m (){- ^ Retrieve rules from the layer 2 section specified by section **name**.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict**, **stateless**, and **useSid** added as **section** attributes.  -}
  , gET40FirewallGlobalroot0ConfigLayer2sectionsSectionId :: Text -> m (){- ^ Retrieve information about the specified layer 2 section.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict**, **stateless**, and **useSid** added as **section** attributes.  -}
  , gET40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRulesRuleId :: Text -> Text -> m (){- ^ Retrieve the configuration of the specified rule.  -}
  , gET40FirewallGlobalroot0ConfigLayer3redirectProfiles :: m (){- ^ Retrieve the Service Insertion profiles that can be applied to layer3 redirect rules.  -}
  , gET40FirewallGlobalroot0ConfigLayer3redirectsectionsSection :: Text -> m (){- ^ Get L3 redirect section configuration  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict**, **stateless**, and **useSid** added as **section** attributes.  -}
  , gET40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRulesRuleID :: Text -> Text -> m (){- ^ Get L3 redirect rule -}
  , gET40FirewallGlobalroot0ConfigLayer3sections :: Maybe Text -> m (){- ^ Retrieve rules from the layer 3 section specified by section **name**.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict**, **stateless**, and **useSid** added as **section** attributes.  -}
  , gET40FirewallGlobalroot0ConfigLayer3sectionsSectionId :: Text -> m (){- ^ Retrieve information about the specified layer 3 section.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict**, **stateless**, and **useSid** added as **section** attributes.  -}
  , gET40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRulesRuleId :: Text -> Text -> m (){- ^ Retrieve information about the specified distributed firewall rule.  -}
  , gET40FirewallGlobalroot0Defaultconfig :: m (){- ^ Retrieve the default firewall configuration.  The output of this method can be used to restore the firewall config back to default. For example, to replace the layer 2 or layer 3 default section, use the relevant default section from the `GET /api/4.0/firewall/globalroot-0/defaultconfig` response body to create the request body of `PUT /api/4.0/firewall/globalroot-0/config/layer2sections|layer3sections/{sectionId}`.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , gET40FirewallGlobalroot0Drafts :: m (){- ^ Displays the draft IDs of all saved configurations. -}
  , gET40FirewallGlobalroot0DraftsDraftID :: Text -> m (){- ^ Get a saved firewall configuration. -}
  , gET40FirewallGlobalroot0DraftsDraftIDActionExport :: Text -> Maybe Bool -> m (){- ^ Export a configuration. -}
  , gET40FirewallGlobalroot0State :: m (){- ^ Retrieve current state of firewall functioning after NSX upgrade.  -}
  , gET40FirewallGlobalroot0Status :: m (){- ^ Get firewall configuration status  **Method history:**  Release | Modification --------|------------- 6.2.4 | Method updated. Parameter **generationNumberObjects** added. Clusters not configured for firewall are excluded from the status output.  -}
  , gET40FirewallGlobalroot0StatusLayer2sectionsSectionID :: Text -> m (){- ^ Retrieve status of the last publish action for the specified layer 2 section.  **Method history:**  Release | Modification --------|------------- 6.2.4 | Method updated. Parameter **generationNumberObjects** added. Clusters not configured for firewall are excluded from the status output.  -}
  , gET40FirewallGlobalroot0StatusLayer3sectionsSectionID :: Text -> m (){- ^ Retrieve status of the last publish action for the specified layer 3 section.  **Method history:**  Release | Modification --------|------------- 6.2.4 | Method updated. Parameter **generationNumberObjects** added. Clusters not configured for firewall are excluded from the status output.  -}
  , gET40FirewallGlobalroot0Timeouts :: m (){- ^ Retrieve Distributed Firewall session timer configuration.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , gET40FirewallGlobalroot0TimeoutsConfigId :: Text -> m (){- ^ Retrieve the specified Distributed Firewall session timer configuration.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , gET40FirewallStatsEventthresholds :: m (){- ^ Retrieve threshold configuration for distributed firewall.   **Note**: Starting in NSX 6.4, using this GET API will not display new threshold types such as  process memory, different types of heap memory, and concurrent connections. Instead, use the new API introduced in NSX 6.4 which is *GET /api/4.0/firewall/stats/thresholds/host/<hostId>?type=<>&thresholdValue;=<>*.  -}
  , gET40FirewallStatsThresholdsHostHostId :: Text -> Maybe Text -> Maybe Text -> m (){- ^ Retrieve threshold configuration for distributed firewall like  CPU utilization, heap memory, calls per second, concurrent connections, process memory.   Use *GET /api/4.0/firewall/stats/thresholds/host/<hostId>?type=<>&thresholdValue;=<>*.  -}
  , gET40FirewallStatsThresholdsTypes :: Text -> m (){- ^ Get the different types of thresholds for distributed firewall.  -}
  , gET40ServicesSpoofguardPolicies :: m (){- ^ Retrieve information about all SpoofGuard policies.  **Note:** you must include the trailing slash for this URI: `/api/4.0/services/spoofguard/policies/`.  -}
  , gET40ServicesSpoofguardPoliciesPolicyID :: Text -> m (){- ^ Retrieve information about the specified SpoofGuard policy.  -}
  , gET40ServicesSpoofguardPolicyID :: Text -> Maybe Text -> m (){- ^ Retrieve IP addresses for the specified state.  -}
  , pOST10ApplianceManagementBackuprestoreBackup :: Maybe Text -> m (){- ^ Start an on-demand NSX backup.  You must set the **Content-Type** header to *application/xml* for the backup to run successfully.  -}
  , pOST10ApplianceManagementBackuprestoreRestore :: Maybe Text -> Maybe Bool -> m (){- ^ Restore data from a backup file.  Retrieve a list of restore files using `GET /api/1.0/appliance-management/backuprestore/backups`.  Restore the backup to a newly deployed, unconfigured NSX Manager appliance. Restoring to an NSX Manager which is in use might result in inconsistent behavior.  **Method history:**  Release | Modification --------|------------- 6.4.1 | Method updated. Query parameter *forceRestore* added.  -}
  , pOST10ApplianceManagementCertificatemanagerCsrNsx :: Text -> m (){- ^ Create a certificate signing request (CSR) for NSX Manager.  The response header contains the created file location.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced. Replaces `PUT /api/1.0/appliance-management/certificatemanager/csr/nsx`.  -}
  , pOST10ApplianceManagementCertificatemanagerPkcs12keystoreNsx :: Maybe Text -> m (){- ^ Upload keystore file.  Input is PKCS#12 formatted NSX file along with password.  -}
  , pOST10ApplianceManagementCertificatemanagerUploadchainNsx :: m (){- ^ Upload certificate chain.  Input is certificate chain file which is a PEM encoded chain of certificates received from the CA after signing a CSR.  -}
  , pOST10ApplianceManagementComponentsComponentAPPMGMTRestart :: m (){- ^ Restart the appliance management web application. -}
  , pOST10ApplianceManagementComponentsComponentComponentIDToggleStatusCommand :: Text -> Text -> m (){- ^ Start or stop a component. -}
  , pOST10ApplianceManagementNotificationsIDAcknowledge :: Text -> m (){- ^ Acknowledge a notification. The notification is then deleted from the system.  -}
  , pOST10ApplianceManagementSystemRestart :: m (){- ^ Reboot the NSX Manager appliance. -}
  , pOST10ApplianceManagementSystemSecuritysettings :: Text -> m (){- ^ Update the NSX Manager security settings, including FIPS and TLS.  Do not enable FIPS until you have upgraded all NSX components to NSX 6.3.0 or later. Enable FIPS on NSX Edges before enabling it on the NSX Manager.  Changing the FIPS mode will reboot the NSX Manager appliance.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , pOST10ApplianceManagementSystemTlssettings :: Text -> m (){- ^ Update TLS settings.  Include a comma separated list of the TLS versions you want to enable, for both server and client.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pOST10ApplianceManagementTechsupportlogsComponentID :: Text -> m (){- ^ Generate tech support logs. The location response header contains the location of the created tech support file.   -}
  , pOST10ApplianceManagementUpgradeStartComponentID :: Text -> Text -> m (){- ^ Start upgrade process.  If you want to enable SSH or join the VMware CEIP program, you must specify *Yes* (not *YES*) for the **answer** parameter.  -}
  , pOST10ApplianceManagementUpgradeUploadbundleComponentID :: Text -> m (){- ^ Upload upgrade bundle. -}
  , pOST10ApplianceManagementUpgradeUploadbundlefromurl :: m (){- ^ Upload upgrade bundle from URL.  **Method history:**  Release | Modification --------|------------- 6.3.3 | Method introduced. 6.4.0 | Method updated. FTP protocol support added.  -}
  , pOST10DirectoryLdapSyncSettings :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> m (){- ^ LDAP full sync settings  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pOST10DirectoryUpdateDomain :: DomainCreate -> m (){- ^ Register or update a domain with NSX Manager -}
  , pOST10DirectoryUpdateEventLogServer :: ELogServerCreate -> m (){- ^ Create EventLog server. -}
  , pOST10DirectoryUpdateLdapServer :: LdapServerCreate -> m (){- ^ Create LDAP server. -}
  , pOST10DirectoryVerifyRootDn :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> m (){- ^ Verify that the rootDNs in the rootDNList are independent to each other. Verify that the rootDNs in the rootDNList exist in Active Director server.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pOST10EventcontrolEventcontrolRootRequest :: DataCollectionKillSwitchToggle -> m (){- ^ Turn data collection on or off at the global level.  In case of an emergency such as a network overload, you can turn off data collection at a global level (kill switch). This overrides all other data collection settings.  Set **value** to *enabled* or *disabled*.  -}
  , pOST10EventcontrolVmVmIDRequest :: Text -> DataCollectionVMCreate -> m (){- ^ Enable or disable data collection on a virtual machine  Set **value** to *enabled* or *disabled*.  -}
  , pOST10IdentityStaticUserMappingUserIDIP :: Text -> Text -> m (){- ^ Create static user IP mapping. -}
  , pOST10NsxCli :: Maybe Text -> NsxCliExecute -> Maybe Text -> m (){- ^ The central command-line interface (central CLI) commands are run from the NSX Manager command line, and retrieve information from the NSX Manager and other devices. These commands can also be executed in the API.  You can insert any valid central CLI command as the **command** parameter. For a complete list of the central CLI commands executable through the API, please see the central CLI chapter of the *NSX Command Line Interface Reference*.  You must set the **Accept** header to *text/plain*.  -}
  , pOST10SamSyslogDisable :: m (){- ^ Disable syslog support. -}
  , pOST10SamSyslogEnable :: m (){- ^ Enable syslog support. -}
  , pOST20EndpointsecurityActivationVendorIDAltitude :: Text -> Text -> VShieldSolutionActivate -> m (){- ^ Activate an endpoint protection solution that has been registered and located. Specify the following parameter in the request body.  | Name            | Comments | |-----------------|------------| |**svmMoid**     | Managed object ID of the virtual machine of the activated endpoint protection solution. |  -}
  , pOST20EndpointsecurityRegistration :: VShieldVendorCreate -> m (){- ^ Register the vendor of an endpoint protection solution. Specify the following parameters in the request.  | Name            | Comments | |-----------------|------------| |**vendorId**     | VMware-assigned ID for the vendor. | |**vendorTitle**  | Vendor-specified title. | |**vendorDescription** | Vendor-specified description. |  -}
  , pOST20EndpointsecurityRegistrationVendorID :: Text -> VShieldSolutionCreate -> m (){- ^ Register an endpoint protection solution. Specify the following parameters in the request.  | Name            | Comments | |-----------------|------------| |**solutionAltitude**     | VMware-assigned altitude for the solution. *Altitude* is a number that VMware assigns to uniquely identify the solution. The altitude describes the type of solution and the order in which the solution receives events relative to other solutions on the same host. | |**solutionTitle**  | Vendor-specified title for the solution. | |**solutionDescription** | Vendor-specified description of the solution. |  -}
  , pOST20EndpointsecurityRegistrationVendorIDAltitudeLocation :: Text -> Text -> SolutionIPPortSet -> m (){- ^ Set the IP address and port on the vNIC host for an endpoint protection solution.  -}
  , pOST20Hostevents :: Text -> m (){- ^ Add configuration of host event notifications on the NSX Manager and host.  **Method history:**    Release | Modification   --------|------------- 6.4.0 |  Method added.  Request body parameters:    * **enabled** - Required. Enable or disable host event notifications. Options are True or False.   * **notificationInterval** - Required. Time interval in seconds at which the NSX Manager receives host event notifications from each host. Valid range is 300 to 3600.  -}
  , pOST20NwfabricConfigure :: Maybe Text -> NwFabricConfig -> m (){- ^ Install network fabric or VXLAN.  This method can be used to perform the following tasks:  * Install Network Virtualization Components * Configure VXLAN * Configure VXLAN with LACPv2 * Reset Communication Between NSX Manager and a Host or Cluster  **Parameter Information**  | Name | Comments | |------|----------| |**resourceId** | vCenter MOB ID of cluster. For example, *domain-7*. A host can be specified when resetting communication. For example, *host-24*. | |**featureId** | Feature to act upon. Omit for network virtualization components operations. Use *com.vmware.vshield.vsm.vxlan* for VXLAN operations, *com.vmware.vshield.vsm.messagingInfra* for message bus operations.| |**ipPoolId** | Used for VXLAN installation. If not specified, DHCP is used for VTEP address assignment.| |**teaming** | Used for VXLAN installation. Options are *FAILOVER_ORDER*, *ETHER_CHANNEL*, *LACP_ACTIVE*, *LACP_PASSIVE*, *LOADBALANCE_LOADBASED*, *LOADBALANCE_SRCID*, *LOADBALANCE_SRCMAC*, *LACP_V2*| |**uplinkPortName** | The *uplinkPortName* as specified in vCenter.|  ### Install Network Virtualization Components  `POST /api/2.0/nwfabric/configure`  ``` <nwFabricFeatureConfig>   <resourceConfig>     <resourceId>CLUSTER MOID</resourceId>   </resourceConfig> </nwFabricFeatureConfig> ```  ### Configure VXLAN  `POST /api/2.0/nwfabric/configure`  ``` <nwFabricFeatureConfig>   <featureId>com.vmware.vshield.vsm.vxlan</featureId>   <resourceConfig>     <resourceId>CLUSTER MOID</resourceId>     <configSpec class=\"clusterMappingSpec\">       <switch>         <objectId>DVS MOID</objectId></switch>         <vlanId>0</vlanId>         <vmknicCount>1</vmknicCount>         <ipPoolId>IPADDRESSPOOL ID</ipPoolId>     </configSpec>   </resourceConfig>   <resourceConfig>     <resourceId>DVS MOID</resourceId>     <configSpec class=\"vdsContext\">       <switch>           <objectId>DVS MOID</objectId>       </switch>       <mtu>1600</mtu>       <teaming>ETHER_CHANNEL</teaming>     </configSpec>   </resourceConfig> </nwFabricFeatureConfig> ```  ### Configure VXLAN with LACPv2  `POST /api/2.0/nwfabric/configure`  ``` <nwFabricFeatureConfig>   <featureId>com.vmware.vshield.nsxmgr.vxlan</featureId>   <resourceConfig>     <resourceId>CLUSTER MOID</resourceId>     <configSpec class=\"clusterMappingSpec\">       <switch>         <objectId>DVS MOID</objectId>       </switch>       <vlanId>0</vlanId>       <vmknicCount>1</vmknicCount>     </configSpec>   </resourceConfig>   <resourceConfig>     <resourceId>DVS MOID</resourceId>     <configSpec class=\"vdsContext\">       <switch>         <objectId>DVS MOID</objectId>       </switch>       <mtu>1600</mtu>       <teaming>LACP_V2</teaming>       <uplinkPortName>LAG NAME</uplinkPortName>     </configSpec>   </resourceConfig> </nwFabricFeatureConfig> ```  ### Reset Communication Between NSX Manager and a Host or Cluster  `POST /api/2.0/nwfabric/configure?action=synchronize`  ```  <nwFabricFeatureConfig>   <featureId>com.vmware.vshield.vsm.messagingInfra</featureId>   <resourceConfig>     <resourceId>resourceId</resourceId>   </resourceConfig> </nwFabricFeatureConfig>  ```  -}
  , pOST20ServicesAlarmsSourceId :: Text -> Maybe Text -> Text -> m (){- ^ Resolve all alarms for the specified source.  Alarms will resolve automatically when the cause of the alarm is resolved.  For example, if an NSX Edge appliance is powered off, this will trigger an alarm. If you power the NSX Edge appliance back on, the alarm will resolve. If however, you delete the NSX Edge appliance, the alarm will persist, because the alarm cause was never resolved. In this case, you may want to manually resolve the alarm. Resolving the alarms will clear them from the NSX Dashboard.  Use `GET /api/2.0/services/alarms/{sourceId}` to retrieve the list of alarms for the source. Use this response as the request body for the `POST` call.  -}
  , pOST20ServicesApplicationScopeId :: Text -> ServicesScopeCreate -> m (){- ^ Create a new service on the specified scope.  -}
  , pOST20ServicesApplicationgroupScopeId :: Text -> ServiceGroupsCreate -> m (){- ^ Create a new service group on the specified scope. -}
  , pOST20ServicesAuthToken :: Maybe Text -> m (){- ^ Create a new authentication token.  You can use this token in your REST client to access the API. Send the token in the Authorization header with the AUTHTOKEN keyword. See the documentation for your REST client for more information.  **Example Authorization header:** ``` Authorization: AUTHTOKEN eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJhZG1pbiIsImV4cCI6MTUyMDU1NTU0NH0.bXPVyHp6uR4HmCmyIMcgJQIS-E1xeb6MLz_3BDk7Lzw ```  By default, this token is created with the default expiry value. You  can also set a custom expiration using the *expiresInMinutes* query  parameter.  If a user authenticates with a token, and the user is deleted or their NSX access is disabled, their token will remain valid until the token expires.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.   -}
  , pOST20ServicesAuthTokeninvalidation :: Maybe Text -> m (){- ^ Invalidate tokens created by the specified user.  **Method history:**  Release | Modification --------|------------- 6.4.1 | Method introduced.   -}
  , pOST20ServicesDashboardUiViewsDashboardWidgetconfigurations :: Text -> m (){- ^ Creates a new Widget Configuration and adds it to the default Dashboard on UI. Supported resource_type are *LabelValueConfiguration* and *GridConfiguration*.  **Notes for Expressions in Widget Configuration**  Expressions should be given in a single line. If an expression spans multiple lines, then form the expression in a single line.  Order of evaluation of expressions is as follows:<br> * First, render configurations are evaluated in their order of appearance in the widget configuration.  The *field* is evaluated at the end. * Next, when render configuration is provided then the order of evaluation is as follows:         * If expressions provided in condition and display value are well-formed and free  of runtime errors such as null pointers      and evaluates to true; then the remaining render configurations are not evaluated, and the current render configurations      *display value* is taken as the final value.          * If expression provided in condition of render configuration is false, then next render configuration is evaluated.           * Finally, field is evaluated only when every render configuration evaluates to false and no error occurs during steps mentioned above.          If an error occurs during evaluation of render configuration, then an error message: \"__ERROR__: See the Error_Messages field of      this report for details\" is shown. The display value corresponding to that label is not shown and evaluation of the      remaining render configurations continues to collect and show all the error messages (marked with the Label for identification)     as Error_Messages: {}. If during evaluation of expressions for any label-value pair an error occurs,      then it is marked with error. The errors are shown in the report, along with the label value pairs that are error-free.      **Important Note for text in condition, field and render configuration's display value**:  For elements that take expressions, strings should be provided by escaping them with a back-slash. These elements are - condition, field and render_configuration's display_value.  **Notes for Drilldowns**: Only *GridConfiguration* is supported as drilldown widget. To make a widget as a drilldown, its category_id should be set as *drilldown*. Drilldowns are supported for *aggregate_count* (subtype of *LabelValueConfiguration*) widgets only.  In other words, only 'aggregate_count' widgets can have drilldowns.  **Notes for Sharing the widget to other users**: Use a valid vsphere user, who has an NSX role assigned that has sufficient permissions, to create the widget and it will get displayed on the UI when that vsphere user logs in. For other users to view the widget on the UI, the owner (user who owners that widget) needs to share the widget (set **shared** parameter to *true*).  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pOST20ServicesHousekeepingManagementIndexMaintenance :: Maybe Text -> m (){- ^ Trigger the reindexing task on demand. Tables with index bloat size greater than 75% are reindexed.  **Method history:**    Release | Modification   --------|------------- 6.3.3 | Method introduced.   -}
  , pOST20ServicesIpamPoolsPoolIdIpaddresses :: Text -> IpAddressRequest -> m (){- ^ Allocate an IP address from the pool.   To allocate the next available IP, set **allocationMode** to *ALLOCATE*    ``` <ipAddressRequest>   <allocationMode>ALLOCATE</allocationMode> </ipAddressRequest> ```  To allocate a specific IP, set **allocationMode** to *RESERVE* and pass the IP to reserve in the **ipAddress** parameter.  ``` <ipAddressRequest>   <allocationMode>RESERVE</allocationMode>   <ipAddress>192.168.1.2</ipAddress> </ipAddressRequest> ```  -}
  , pOST20ServicesIpamPoolsScopeScopeId :: Text -> IpPool -> m (){- ^ Create a pool of IP addresses. For **scopeId** use *globalroot-0* or the *datacenterId* in upgrade use cases.  -}
  , pOST20ServicesIpsetScopeMoref :: Text -> IpsetCreate -> m (){- ^ Create a new IP set. -}
  , pOST20ServicesMacsetScopeScopeId :: Text -> MacSetCreateUpdate -> m (){- ^ Create a MAC address set on the specified scope.  The value parameter can include a single MAC identifier or a comma separated set of MAC identifiers. Universal MAC address sets are read-only from secondary managers.  -}
  , pOST20ServicesPolicySecuritypolicy :: SecurityPolicyCreate -> m (){- ^ Create a security policy.  When creating a security policy, a parent security policy can be specified if required. The security policy inherits services from the parent security policy. Security group bindings and actions can also be specified while creating the policy. Note that execution order of actions in a category is implied by their order in the list. The response of the call has Location header populated with the URI using which the created object can be fetched.  Ensure that: * the required VMware built in services (such as Distributed Firewall   and Endpoint) are installed. See *NSX Installation Guide*. * the required partner services have been registered with NSX Manager. * the required security groups have been created.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tag** parameter added. You can specify  *tag* for the firewall rule.  -}
  , pOST20ServicesPolicySecuritypolicyHierarchy :: Maybe Text -> HierarchyCreate -> m (){- ^ Import a security policy configuration  You can create multiple security policies and parent-child hierarchies using the data fetched through export. All objects including security policies, security groups and security actions are created on a global scope.  The policy that is being imported needs to be included in the request body.  If a suffix is specified, it is added after the names of the security policy, security action, and security group objects in the exported XML. The suffix can thus be used to differentiate locally created objects from imported ones.  The location of the newly created security policy objects (multiple locations are separated by commas) is populated in the Location header of the response.  -}
  , pOST20ServicesSecuritygroupBulkScopeId :: Text -> SecGroupBulkCreate -> m (){- ^ Create a new security group on a global scope or universal scope with membership information.  Universal security groups are read-only when querying a secondary NSX manager.  When you create a universal security group (on scope *universalroot-0*) by default **localMembersOnly** is set to *false* which indicates that the universal security group will contain members across the cross-vCenter NSX environment.  This is the case in an active active environment. You can add the following objects to a universal security group with *localMembersOnly=false* (active active): * IP Address Set * MAC Address Set * Universal Security Groups with *localMembersOnly=false*  When you create a universal security group (on scope *universalroot-0*) you can set the extendedAttribute **localMembersOnly** to *true* to indicate that the universal security group will contain members local to that NSX Manager only.  This is the case in an active standby environment, because only one NSX environment is active at a time, and the same VMs are present in each NSX environment. You can add the following objects to a universal security group with *localMembersOnly=true* (active standby): * Universal Security Tag * IP Address Set * MAC Address Set * Universal Security Groups with *localMembersOnly=true* * Dynamic criteria using VM name  You can set the **localMembersOnly** attribute only when the universal security group is created, it cannot be modified afterwards.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Extended attribute **localMembersOnly** introduced.  -}
  , pOST20ServicesSecuritygroupScopeId :: Text -> Text -> m (){- ^ Create a new security group, with no membership information specified. You can add members later with `PUT /2.0/services/securitygroup/bulk/{objectId}`  When you create a universal security group (on scope *universalroot-0*) by default **localMembersOnly** is set to *false* which indicates that the universal security group will contain members across the cross-vCenter NSX environment.  This is the case in an active active environment. You can add the following objects to a universal security group with *localMembersOnly=false* (active active): * IP Address Set * MAC Address Set * Universal Security Groups with *localMembersOnly=false*  When you create a universal security group (on scope *universalroot-0*) you can set the extendedAttribute **localMembersOnly** to *true* to indicate that the universal security group will contain members local to that NSX Manager only.  This is the case in an active standby environment, because only one NSX environment is active at a time, and the same VMs are present in each NSX environment. You can add the following objects to a universal security group with *localMembersOnly=true* (active standby): * Universal Security Tag * IP Address Set * MAC Address Set * Universal Security Groups with *localMembersOnly=true* * Dynamic criteria using VM name  You can set the **localMembersOnly** attribute only when the universal security group is created, it cannot be modified afterwards.   **Method history:**  Release | Modification --------|------------- 6.3.0 | Extended attribute **localMembersOnly** introduced.  -}
  , pOST20ServicesSecuritytagsTag :: SecurityTagCreate -> m (){- ^ Create a new security tag.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method updated. **isUniversal** parameter can be set to create a universal security tag.  -}
  , pOST20ServicesSecuritytagsTagTagIdVm :: Text -> Maybe Text -> Text -> m (){- ^ Attach or detach a security tag to a virtual machine.  This operation does not check that the virtual machine exists in the local inventory. This allows you to attach a universal security tag to a virtual machine that is connected to a secondary NSX Manager (and therefore is not connected to the primary NSX Manager where the call is sent).  Possible keys for the tagParameter are: * instance_uuid * bios_uuid * vmname  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , pOST20ServicesSecuritytagsVmVmId :: Text -> Maybe Text -> Text -> m (){- ^ Update security tags associated with the specified virtual machine.  You can assign multiple tags at a time to the specified VM, or clear all assigned tags from the specified VM.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , pOST20ServicesSnmpManager :: Text -> m (){- ^ Add an SNMP manager.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pOST20ServicesSsoconfig :: SsoConfig -> m (){- ^ Register NSX Manager to SSO Services. -}
  , pOST20ServicesSystemalarmsAlarmId :: Text -> Maybe Text -> m (){- ^ Resolve the specified alarm.  System alarms resolve automatically when the cause of the alarm is resolved. For example, if an NSX Edge appliance is powered off, this triggers a alarm. If you power the NSX Edge appliance back on, the alarm resolves. If however, you delete the NSX Edge appliance, the alarm persists, because the alarm cause was never resolved. In this case, you might want to manually resolve the alarm. Resolving the alarm will clear it from the NSX Dashboard.           **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , pOST20ServicesTruststoreCertificate :: Maybe Text -> CertificateCreate -> m (){- ^ Import a certificate or a certificate chain against a certificate signing request.  -}
  , pOST20ServicesTruststoreConfigScopeId :: Text -> CertificateSelfSignedCreate -> m (){- ^ Create a single certificate  You can create a certificate for a specific NSX Edge, or if you specify a scope of *globalroot-0* you can create a global certificate in NSX Manager which is available to all NSX Edges.  -}
  , pOST20ServicesTruststoreCrlScopeId :: Text -> CrlCreate -> m (){- ^ Create a certificate revocation list (CRL) on the specified scope.  -}
  , pOST20ServicesTruststoreCsrScopeId :: Text -> CsrCreate -> m (){- ^ Create a certificate signing request (CSR). -}
  , pOST20ServicesUsermgmtRoleUserId :: Text -> Maybe Bool -> UserRoleMgmtCreate -> m (){- ^ Add role and resources for a user. -}
  , pOST20ServicesVcconfigConnectionstatus :: Maybe Text -> m (){- ^ Update the vCenter Server connection status. -}
  , pOST20SiDeploy :: Maybe Text -> SecurityFabricCreate -> m (){- ^ Deploy security fabric.  -}
  , pOST20Techsupportbundle :: Maybe Text -> Maybe Text -> Text -> m (){- ^ Generates the technical support log bundle or aborts the bundle generation process. Use */techsupportbundle?action=generate* to generate the bundle. Use */techsupportbundle?action=cancel* to abort the bundle generation that is in in-progress.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pOST20UniversalsyncConfigurationNsxmanagers :: UniversalSyncConfigurationNsxManagersCreate -> m (){- ^ Add a secondary NSX manager.  Run this method on the primary NSX Manager, providing details of the secondary NSX Manager.  Retrieve the certificate thumbprint of the secondary NSX Manager using the `GET /api/1.0/appliance-management/certificatemanager/certificates/nsx` method. The **sha1Hash** parameter contains the thumbprint.  -}
  , pOST20UniversalsyncConfigurationRole :: Maybe Text -> m (){- ^ Set the universal sync configuration role. -}
  , pOST20UniversalsyncSync :: Maybe Text -> m (){- ^ Sync all objects on the NSX Manager. -}
  , pOST20VdnCdo :: Maybe Text -> Text -> m (){- ^ Modify the status of CDO mode. This method can be used to perform the following tasks:  * Update the CDO mode: POST /api/2.0/vdn/cdo?action=update * Resync the CDO mode: POST /api/2.0/vdn/cdo?action=resync * Enable the CDO mode: POST /api/2.0/vdn/cdo?action=enable * Disable the CDO mode: POST /api/2.0/vdn/cdo?action=disable  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pOST20VdnConfigHostHostIdVxlanVteps :: Text -> Maybe Text -> m (){- ^ Resolve missing VXLAN VMKernel adapters.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pOST20VdnConfigMulticasts :: Maybe Bool -> VdnMulticast -> m (){- ^ Add a multicast address range for logical switches.  The address range includes the beginning and ending addresses.  -}
  , pOST20VdnConfigSegments :: Maybe Bool -> VdnSegment -> m (){- ^ Add a segment ID pool.  * **name** - Required property. * **desc** - Optional property. * **begin** - Required property. Minimum value is *5000* * **end** - Required property. Maximum value is *16777216*  -}
  , pOST20VdnConfigVxlanUdpPortResume :: m (){- ^ If you update the VXLAN port using the **Change** button on the **Installation > Logical Network Preparation** page in the vSphere Web Client, or using `PUT /api/2.0/vdn/config/vxlan/udp/port/{portNumber}` without the **force** parameter, and the port update does not complete, you can try resuming the port config change.  You can check the progress of the VXLAN port update with  `GET /api/2.0/vdn/config/vxlan/udp/port/taskStatus`.  Only try resuming the port update if it has failed to complete. You should not need to resume the port update under normal circumstances.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pOST20VdnController :: Controller -> m (){- ^ Add a new NSX Controller on the specified cluster. The *hostId* parameter is optional. The *resourcePoolId* can be either the *clusterId* or *resourcePoolId*.  The IP address of the controller node will be allocated from the specified IP pool.   **Note:** Controller nodes are deployed with 4 GB of memory regardless of  which **deployType** value is provided.  **Method history:**  Release | Modification --------|------------- 6.3.3 | Method updated. **deployType** is no longer required.  -}
  , pOST20VdnControllerControllerId :: Text -> Maybe Text -> m (){- ^ If you power off or delete a controller from vCenter, NSX Manager detects the change in controller status. You can remediate the controller, which will power on a powered off controller, or remove the controller from the NSX Manager database if the controller is deleted.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pOST20VdnControllerControllerIdSyslog :: Text -> ControllerSyslog -> m (){- ^ Add controller syslog exporter on the controller. -}
  , pOST20VdnHardwaregatewayBindings :: Text -> m (){- ^ Create a hardware gateway binding.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pOST20VdnHardwaregatewayBindingsManage :: Text -> m (){- ^ Manage hardware gateway binding objects.  Use this API to attach, detach, and update multiple bindings in a single API call.  This API accepts three lists for add, update, and delete. Each list accepts a hardwareGatewayManageBindingsItem with a full description of the new binding with its objectID. This API handles a maximum of 100 HardwareGatewayManageBindingsItem objects for each of the Add/Update/Delete lists.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pOST20VdnHardwaregateways :: Text -> m (){- ^ Install a hardware gateway.  **bfdEnabled** is true by default.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pOST20VdnScopes :: Maybe Bool -> VdnScopeCreate -> m (){- ^ Create a transport zone.  Request body parameters:    * **name** - Required. The name of the transport zone.   * **description** - Optional. Description of the transport zone.   * **objectId** - Required. The cluster object ID from vSphere. One or more are     required.   * **controlPlaneMode** - Optional. The control plane mode. It can be     one of the following:       * *UNICAST_MODE*       * *HYBRID_MODE*       * *MULTICAST_MODE*  -}
  , pOST20VdnScopesScopeId :: Text -> Maybe Text -> VdnScopeEdit -> m (){- ^ Update the specified transport zone.  You can add a cluster to or delete a cluster from a transport zone.  You can also repair missing port groups. For every logical switch created, NSX creates a corresponding port group in vCenter. If the port group is lost for any reason, the logical switch will stop functioning. The repair action recreates any missing port groups.  -}
  , pOST20VdnScopesScopeIdCdo :: Text -> Maybe Text -> m (){- ^ **Note**: From 6.4.0, CDO feature is supported at NSX Manager level and not at Transport Zone level. For more details, refer to *Working with Controller Disconnected Operation (CDO) Mode* section.  Enable or disable CDO mode for the specified transport zone.  Controller Disconnected Operation (CDO) mode ensures that the data plane connectivity is unaffected when host lose connectivity with the controller.   If you want to enable CDO mode on the universal transport zone in a cross-vCenter NSX environment, you must do this from the primary NSX Manager. The universal synchronization service will propagate the CDO configuration to the secondary NSX Managers.   **Method history:**  Release | Modification --------|------------- 6.3.2 | Method introduced. (Tech preview in 6.3.0).  -}
  , pOST20VdnScopesScopeIdConnCheckMulticast :: Text -> Text -> m (){- ^ Test multicast group connectivity.  Test multicast group connectivity between two hosts connected to the specified transport zone.  Parameter **packetSizeMode** has one of the following values: * *0* - VXLAN standard packet size * *1* - minimum packet size * *2* - customized packet size. If you set **packetSizeMode** to *2*, you must specify the size using the **packetSize** parameter.  -}
  , pOST20VdnScopesScopeIdVirtualwires :: Text -> LogicalSwitchCreate -> m (){- ^ Create a logical switch.  To create a universal logical switch use *universalvdnscope* as the scopeId in the URI and send the request to the primary NSX Manager. Request body parameters:   * **name** - Optional. The name of the logical switch.   * **description** - Optional. Description of the logical switch.   * **tenantId** - Required.   * **controlPlaneMode** - Optional. The control plane mode. If not     specified, the **controlPlaneMode** of the transport zone is used. It     can be one of the following:       * *UNICAST_MODE*       * *HYBRID_MODE*       * *MULTICAST_MODE*   * **guestVlanAllowed** - Optional. Default is *false*.  -}
  , pOST20VdnSwitches :: VdsContext -> m (){- ^ Prepare a vSphere Distributed Switch.  The MTU is the maximum amount of data that can be transmitted in one packet before it is divided into smaller packets. VXLAN frames are slightly larger in size because of the traffic encapsulation, so the MTU required is higher than the standard MTU. You must set the MTU for each switch to 1602 or higher.  -}
  , pOST20VdnTraceflow :: TraceflowCreate -> m (){- ^ Create a traceflow. -}
  , pOST20VdnVirtualwiresVirtualWireIDBacking :: Text -> Maybe Text -> m (){- ^ For every logical switch created, NSX creates a corresponding port group in vCenter. If the port group is missing, the logical switch will stop functioning.  If the port group backing a logical switch is deleted, you can recreate a new backing port group for the logical switch.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pOST20VdnVirtualwiresVirtualWireIDConnCheckMulticast :: Text -> LogicalSwitchConnCheck -> m (){- ^ Test multicast group connectivity.  Test multicast group connectivity between two hosts connected to the specified logical switch.  Parameter **packetSizeMode** has one of the following values: * *0* - VXLAN standard packet size * *1* - minimum packet size * *2* - customized packet size. If you set **packetSizeMode** to *2*, you must specify the size using the **packetSize** parameter.  -}
  , pOST20VdnVirtualwiresVirtualWireIDConnCheckP2p :: Text -> LogicalSwitchPing -> m (){- ^ Test point-to-point connectivity.  Test point-to-point connectivity between two hosts connected to the specified logical switch.  Parameter **packetSizeMode** has one of the following values: * *0* - VXLAN standard packet size * *1* - minimum packet size * *2* - customized packet size. If you set **packetSizeMode** to *2*, you must specify the size using the **packetSize** parameter.  -}
  , pOST20VdnVirtualwiresVirtualWireIDHardwaregatewaysHardwareGatewayBindingId :: Text -> Text -> Maybe Text -> Text -> m (){- ^ Manage the connection between a hardware gateway and a logical switch.  ### Attach a hardware gateway to a logical switch and create a new binding with the information provided  `POST /api/2.0/vdn/virtualwires/{virtualwireid}/hardwaregateways`  ``` <hardwareGatewayBinding>   <hardwareGatewayId>hardwarewgateway1</hardwareGatewayId>   <vlan>v1</vlan>   <switchName>s1</switchName>   <portName>s1</portName> </hardwareGatewayBinding>  ```  ### Attach a hardware gateway to a logical switch, specifying an existing binding by ID  `POST /api/2.0/vdn/virtualwires/<virtualwireId>/hardwaregateways/{bindingId}?action=attach`  ``` <virtualWire>   ...   <hardwareGatewayBindings>     <hardwareGatewayBinding>       <id>binding id</id>     </hardwareGatewayBinding>   </hardwareGatewayBindings> </virtualWire> ```  ### Detach a hardware gateway from a logical switch  `POST /api/2.0/vdn/virtualwires/<virtualwireId>/hardwaregateways/{bindingId}?action=detach`  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pOST20VdnVirtualwiresVmVnic :: LogicalSwitchVmAttach -> m (){- ^ Attach a VM vNIC to, or detach a VM vNIC from a logical switch.  Specify the logical switch ID in the **portgroupId** parameter. To detach a VM vNIC from a logical switch, leave the **portgroupId** parameter empty.  To find the ID of a VM vNIC, do the following: 1. In the vSphere MOB, navigate to the VM you want to connect or disconnect. 2. Click **config** and take note of the **instanceUuid**. 3. Click **hardware** and take note of the last three digits of the appropriate network interface device.  Use these two values to form the VM vNIC ID.  For example, if the **instanceUuid** is *502e71fa-1a00-759b-e40f-ce778e915f16* and the appropriate **device** value is *device[4000]*, the **objectId** and **vnicUuid** are both *502e71fa-1a00-759b-e40f-ce778e915f16.000*.  -}
  , pOST40Edges :: Maybe Bool -> NsxEdgesCreate -> m (){- ^ You can install NSX Edge as a services gateway or as a logical router.  The **type** parameter determines which type of NSX Edge is deployed: *distributedRouter* or *gatewayServices*. If no type is specified, the type is *gatewayServices*.  Other parameters for this method will differ depending on which type of NSX Edge you are deploying. See the examples and parameter tables for more information.  ### NSX Edge: Service Gateway  The NSX Edge installation API copies the NSX Edge OVF from the Edge Manager to the specified datastore and deploys an NSX Edge on the given datacenter. After the NSX Edge is installed, the virtual machine powers on and initializes according to the given network configuration. If an appliance is added, it is deployed with the specified configuration.  Installing an NSX Edge instance adds a virtual machine to the vCenter Server inventory, you must specify an IP address for the management interface, and you may name the NSX Edge instance.  The configuration you specify when you install an NSX Edge is stored in the database. If an appliance is added, the configuration is applied to it and it is deployed.  NOTE: Do not use hidden/system resource pool IDs as they are not supported on the UI.  #### Request Body to Create Edge Services Gateway      <edge>       <datacenterMoid>datacenter-2</datacenterMoid>       <name>org1-edge</name>       <description>Description for the edge gateway</description>       <tenant>org1</tenant>       <fqdn>org1edge1</fqdn>       <vseLogLevel>info</vseLogLevel>       <enableAesni>false</enableAesni>       <enableFips>true</enableFips>       <appliances>         <applianceSize>compact</applianceSize>         <enableCoreDump>true</enableCoreDump>         <appliance>           <resourcePoolId>resgroup-53</resourcePoolId>           <datastoreId>datastore-29</datastoreId>           <hostId>host-28</hostId>            <vmFolderId>group-v38</vmFolderId>            <customField>              <key>system.service.vmware.vsla.main01</key>             <value>string</value>           </customField>           <cpuReservation>              <limit>2399</limit>             <reservation>500</reservation>             <shares>500</shares>           </cpuReservation>           <memoryReservation>              <limit>5000</limit>             <reservation>500</reservation>             <shares>20480</shares>           </memoryReservation>         </appliance>       </appliances>       <vnics>         <vnic>           <index>0</index>           <name>internal0</name>           <type>internal</type>           <portgroupId>dvportgroup-114</portgroupId>           <addressGroups>             <addressGroup>               <primaryAddress>192.168.3.1</primaryAddress>               <secondaryAddresses>                 <ipAddress>192.168.3.2</ipAddress>                 <ipAddress>192.168.3.3</ipAddress>               </secondaryAddresses>               <subnetMask>255.255.255.0</subnetMask>             </addressGroup>             <addressGroup>               <primaryAddress>192.168.4.1</primaryAddress>               <secondaryAddresses>                 <ipAddress>192.168.4.2</ipAddress>                 <ipAddress>192.168.4.3</ipAddress>               </secondaryAddresses>               <subnetPrefixLength>24</subnetPrefixLength>             </addressGroup>             <addressGroup>               <primaryAddress>ffff::1</primaryAddress>               <secondaryAddresses>                 <ipAddress>ffff::2</ipAddress>               </secondaryAddresses>               <subnetPrefixLength>64</subnetPrefixLength>             </addressGroup>           </addressGroups>           <macAddress>             <edgeVmHaIndex>0</edgeVmHaIndex>             <value>00:50:56:01:03:23</value>           </macAddress>           <fenceParameter>             <key>ethernet0.filter1.param1</key>             <value>1</value>           </fenceParameter>           <mtu>1500</mtu>           <enableProxyArp>false</enableProxyArp>           <enableSendRedirects>true</enableSendRedirects>           <isConnected>true</isConnected>           <inShapingPolicy>             <averageBandwidth>200000000</averageBandwidth>             <peakBandwidth>200000000</peakBandwidth>             <burstSize>0</burstSize>             <enabled>true</enabled>             <inherited>false</inherited>           </inShapingPolicy>           <outShapingPolicy>             <averageBandwidth>400000000</averageBandwidth>             <peakBandwidth>400000000</peakBandwidth>             <burstSize>0</burstSize>             <enabled>true</enabled>             <inherited>false</inherited>           </outShapingPolicy>         </vnic>       </vnics>       <cliSettings>         <userName>test</userName>         <password>test123!</password>         <remoteAccess>false</remoteAccess>       </cliSettings>       <autoConfiguration>         <enabled>true</enabled>         <rulePriority>high</rulePriority>       </autoConfiguration>       <dnsClient>         <primaryDns>10.117.0.1</primaryDns>         <secondaryDns>10.117.0.2</secondaryDns>         <domainName>vmware.com</domainName>         <domainName>foo.com</domainName>       </dnsClient>       <queryDaemon>         <enabled>true</enabled>         <port>5666</port>       </queryDaemon>     </edge>  ### NSX Edge: Logical (Distributed) Router  Before installing a logical router, you must prepare the hosts on the appropriate clusters.   The user specified configuration is stored in the database and Edge identifier is returned to the user. This identifier must be used for future configurations on the given Edge.  If any appliance(s) are specified and at least one connected interface/vnic is specified, then the appliance(s) are deployed and configuration is applied to them.  It is not possible to set the <ecmp>true</ecmp> property upon creation of a distributed logical router Edge and a subsequent API call is required to enable ECMP.  DHCP relay settings are not able to be used when creating a distributed logical router Edge and a subsequent API call is required to configure DHCP relay properties.  #### Request Body to Create Logical (Distributed) Router      <edge>       <datacenterMoid>datacenter-2</datacenterMoid>       <type>distributedRouter</type>       <appliances>         <appliance>         <resourcePoolId>resgroup-20</resourcePoolId>         <datastoreId>datastore-23</datastoreId>         </appliance>       </appliances>       <mgmtInterface>         <connectedToId>dvportgroup-38</connectedToId>           <addressGroups>              <addressGroup>               <primaryAddress>10.112.196.165</primaryAddress>               <subnetMask>255.255.252.0</subnetMask>             </addressGroup>            </addressGroups>       </mgmtInterface>       <interfaces>         <interface>           <type>uplink</type>           <mtu>1500</mtu>           <isConnected>true</isConnected>           <addressGroups>              <addressGroup>               <primaryAddress>192.168.10.1</primaryAddress>               <subnetMask>255.255.255.0</subnetMask>             </addressGroup>           </addressGroups>           <connectedToId>dvportgroup-39</connectedToId>         </interface>         <interface>           <type>internal</type>           <mtu>1500</mtu>           <isConnected>true</isConnected>           <addressGroups>              <addressGroup>               <primaryAddress>192.168.20.1</primaryAddress>               <subnetMask>255.255.255.0</subnetMask>             </addressGroup>           </addressGroups>           <connectedToId>dvportgroup-40</connectedToId>         </interface>       </interfaces>     </edge> ### Request and Response Body Parameters for NSX Edge  #### General Request Body Parameters: Edge Services Gateway and Logical (Distributed) Router  Parameter |  Description | Comments  ---|---|--- **datacenterMoid** |Specify vCenter Managed Object Identifier of data center on which edge has to be deployed|Required.  **type** | Specify which kind of NSX Edge to deploy. Choice of *distributedRouter* or *gatewayServices*. | Optional. Default is *gatewayServices*. **name** |Specify a name for the new NSX Edge.|Optional. Default is *NSX-&lt;edgeId&gt;*. Used as a VM name on vCenter appended by *-&lt;haIndex&gt;*.  **description** |NSX Edge description.|Optional.  **tenant** |Specify the tenant. Used for syslog messages.|Optional.  **fqdn** |Fully Qualified Domain Name for the edge.|Optional. Default is *NSX-&lt;edgeId&gt;* Used to set hostname on the VM. Appended by *-&lt;haIndex&gt;* **vseLogLevel** |Defines the log level for log messages captured in the log files.|Optional. Choice of: *emergency*, *alert*, *critical*, *error*, *warning*, *notice*, *debug*. Default is *info*. **enableAesni** |Enable support for Advanced Encryption Standard New Instructions on the Edge.|Optional. True/False. Default is *true*. **enableCoreDump** |Deploys a new NSX Edge for debug/core-dump purpose.|Optional. Default is false. Enabling core-dump will deploy an extra disk for core-dump files.  #### Appliances Configuration: Edge Services Gateway and Logical (Distributed) Router  Parameter |  Description | Comments  ---|---|--- **applianceSize** |Edge form factor, it determines the NSX Edge size and capability. |Required. Choice of: *compact*, *large*, *quadlarge*, *xlarge*. Default is *compact*. **deployAppliances** | Determine whether to deploy appliances. | Default is *true*. **appliance** |Appliance configuration details.|Required. Can configure a maximum of two appliances. Until one appliance is configured and NSX Edge VM is deployed successfully, none of the configured features will serve the network. **resourcePoolId** |Details of resource pool on which to deploy NSX Edge. |Required. Can be resource pool ID, e.g. *resgroup-15* or cluster ID, e.g. *domain-c41*. **datastoreId** |Details of datastore on which to deploy NSX Edge.|Required.  **hostId** |ID of the host on which to deploy the NSX Edge.|Optional.  **vmFolderId** |The folder in which to save the NSX Edge.|Optional.  **customField** |Custom key-value attributes. |Optional. Use custom attributes to associate user-specific meta-information with VMs and managed hosts, stored on vCenter Server. **customField > key** |Meta information Key.|Required if customField is specified.  **customField > value** |Meta information Value.|Required if customField is specified.  **cpuReservation > limit** |Maximum CPU capacity the NSX Edge can use, specified in MHz.|Optional. -1 (unlimited), any positive integer **cpuReservation > reservation** |CPU capacity reserved for NSX Edge in MHz.|Optional.  **cpuReservation > shares** |Higher value implies NSX Edge has priority when accessing resources.|Optional.  **memoryReservation > limit** |Maximum memory the NSX Edge can use, specified in MB.|Optional. -1 (unlimited), any positive integer **memoryReservation > reservation** |Memory capacity reserved for NSX Edge in MB.|Optional.  **memoryReservation > shares** |Higher value implies NSX Edge has priority when accessing resources.|Optional.  **cliSettings > userName** |User name.|Required. length 1-33. **cliSettings > password** |Password.|Required. The password must be at least 12 characters long. Must contain at-least 1 uppercase, 1 lowercase, 1 special character and 1 digit. In addition, a character cannot be repeated 3 or more times consectively. **cliSettings > remoteAccess** |Enables or disables remote access through SSH. |Required. Relevant firewall rules to allow traffic on port 22 must be opened by user/client **autoConfiguration > enabled** |Enable/Disable status of autoConfiguration|Optional. True/False. Default is *true*. If autoConfiguration is enabled, firewall rules are automatically created to allow control traffic. Rules to allow data traffic are not created.  For example, if you are using IPsec VPN, and **autoConfiguration** is *true*, firewall rules will automatically be configured to allow IKE traffic. However, you will need to add additional rules to allow the data traffic for the IPsec tunnel. If HA is enabled, firewall rules are always created, even if **autoConfiguration** is *false*, otherwise both HA appliances will become active. **autoConfiguration > rulePriority** |Defines the priority of system-defined rules over user-defined rules.|Optional. High, Low.  Default is *high*. **queryDaemon > enabled** |Configure the communication between server load balancer and NSX Edge VM.|Default is *false*. **queryDaemon > port** |Defines the port through which the communication happens.|Integer 1-65535. Default is *5666*.  #### DNS Client: Edge Services Gateway and Logical (Distributed) Router   Parameter |  Description | Comments  ---|---|--- **dnsClient** |Configures the DNS settings of the Edge Services Gateway.|Optional. If the primary/secondary are specified and the DNS service is not specified, the primary/secondary will be used as the default of the DNS service. **primaryDns** |Primary DNS IP | **secondaryDns** |Secondary DNS IP | **domainName** |Domain Name of Edge | **domainName** |Secondary Domain Name of Edge |  #### vNIC Parameters: Edge Services Gateway Only  Parameter |  Description | Comments ---|---|--- **vnic** |Configure interface (vNic).|Required. Until one connected vNic is configured, none of the configured features will serve the network. **index** |Index of vNic to be configured. Value varies from 0-9. 4094 sub-interfaces can be configured in trunk mode.|Required.  **name** |Name of the vNic.|Optional. System provides default names: vnic0...vnic9. **label** |Label for the vNic.|Optional. System provides default labels: vNic_0...vNic_9. **type** |Type of interface connected to vNic.|Optional. Choice of: *Uplink*, *Internal*, *TRUNK*. Default is *Internal*. *TRUNK* should be specified when sub-interfaces are configured. **portgroupId** |Connect NSX Edge to the network through this port group.|Required. Choice of: *portgroupId* or *virtualWireId*. *portgroupId* needs to be defined if *isConnected=true* **addressGroup** |Address Group assigned to vNic.|Required. More than one addressGroup/subnets can be assigned to the vNic. **primaryAddress** |Primary Address of Edge Interface.|Required. IPv4 and IPv6 addresses are supported. **secondaryAddresses > ipAddress** |IP assigned to interface.|Optional. One or more **ipAddress** parameters are allowed, to enable assigning multiple IP addresses to a vNic, for example, for load balancing, NAT, VPN. At least one is required if **secondaryAddresses** is specified.  **subnetMask** or **subnetPrefixLength** |Subnet mask or prefix value.  |Required. Either **subnetMask** or **subnetPrefixLength** should be provided. When both are provided then **subnetprefixLength** is ignored. **macAddress** |Option to manually specify the MAC address. |Optional.  Managed by vCenter if not provided. **macAddress > edgeVmHaIndex** |HA index of the Edge VM. |Required. 0 or 1. **macAddress > value** |Value of the MAC address.|Optional. Ensure that MAC addresses provided are unique within the given layer 2 domain. **vnic > mtu** |The maximum transmission value for the data packets.|Optional.  Default is *1500*. **enableProxyArp** |Enables proxy ARP. Do not use this flag unless you want NSX Edge to proxy ARP for all configured subnets.  |Optional.  True/False. Default is *false*. **enableSendRedirects** |Enables ICMP redirect. |Optional. True/False.  Default is *true*. **isConnected** |Sets if the interface is connected to the port group network. |Optional. True/False. Default is *false*. **portgroupId** needs to be defined if *isConnected=true*. **inShapingPolicy** |Configure Incoming Traffic.|Optional.  **outShapingPolicy** |Configure Outgoing Traffic.|Optional.  **averageBandwidth**<br>(inShapingPolicy or outShapingPolicy) |Sets average bandwidth for traffic.|Optional.  **peakBandwidth**<br>(inShapingPolicy or outShapingPolicy) |Sets peak bandwidth for traffic.|Required.  **burstSize**<br>(inShapingPolicy or outShapingPolicy) |Sets the burst size of the interface.|Required.  **enabled**<br>(inShapingPolicy or outShapingPolicy) |Enable/disable status of this traffic policy.|Required.  **inherited**<br>(inShapingPolicy or outShapingPolicy) |Determine whether properties should be inherited to the vNic from the port group.|Required.   #### HA (Management) Interfaces and Interfaces Configuration: Logical (Distributed) Router Only  Parameter |  Description | Comments  ---|---|--- **mgmtInterface** | High availability interface configuration. Interface index 0 is assigned. | Required. **interface** | Interface configuration. 1-9 are reserved for uplinks, 10-999 are used for internal interfaces. | Optional. Can be added after logical router creation. **connectedToId**<br>(mgmtInterface or interface) | Managed Object ID of logical switch or port group. | For example, *virtualwire-1* or *dvportgroup-50*. Logical router interfaces do not support legacy port groups.  **name**<br>(mgmtInterface or interface) | Name assigned to interface. | Optional. **addressGroup**<br>(mgmtInterface or interface) |Address Group assigned to interface. |Required. Only one **addressGroup** can be configured on each logical router **mgmtInterface** or **interface**. **primaryAddress**<br>(mgmtInterface or interface) |Primary Address of interface. |Required. Secondary Addresses are not supported on logical routers. Address must be IPv4. **subnetMask** or **subnetPrefixLength**<br>(mgmtInterface or interface) |Subnet mask or prefix value.  |Required. Either **subnetMask** or **subnetPrefixLength** should be provided. When both are provided then **subnetprefixLength** is ignored. **mtu**<br>(mgmtInterface or interface) |The maximum transmission value for the data packets. |Optional. Default is 1500. **type** | Type of interface. | Required. Choice of *uplink* or *internal*.   -}
  , pOST40EdgesEdgeId :: Text -> Maybe Text -> m (){- ^ Manage NSX Edge. -}
  , pOST40EdgesEdgeIdAesni :: Text -> Maybe Bool -> m (){- ^ Modify AESNI setting. -}
  , pOST40EdgesEdgeIdAppliances :: Text -> Maybe Text -> m (){- ^ Change the size of both appliances.  -}
  , pOST40EdgesEdgeIdAppliancesHaIndex :: Text -> Text -> Maybe Text -> Text -> Maybe Text -> m (){- ^ Used to send CLI Commands to the Edge Gateway. To use CLI commands you also need to add an additional Accept Header with type text/plain, as well as the query parameter action=execute.  VMware recommends using the Central CLI instead of this method. See *Working With the Central CLI*   -}
  , pOST40EdgesEdgeIdCliremoteaccess :: Text -> Maybe Bool -> m (){- ^ Change CLI remote access -}
  , pOST40EdgesEdgeIdCoredump :: Text -> Maybe Bool -> m (){- ^ Modify core dump setting. -}
  , pOST40EdgesEdgeIdDhcpConfigBindings :: Text -> DhcpStaticBindingCreate -> m (){- ^ Append a static-binding to DHCP config. A static-binding ID is returned within a Location HTTP header.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. DHCP options added.  -}
  , pOST40EdgesEdgeIdDhcpConfigIppools :: Text -> DhcpPoolCreate -> m (){- ^ Add an IP pool to the DHCP configuration. Returns a pool ID within a Location HTTP header.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. DHCP options added.  -}
  , pOST40EdgesEdgeIdFips :: Text -> Maybe Bool -> m (){- ^ Modify FIPS setting. -}
  , pOST40EdgesEdgeIdFirewallConfigRules :: Text -> Maybe Text -> FirewallRulesCreate -> m (){- ^ Add one or more rules. You can add a rule above a specific rule using the query parameter, indicating the desired ruleID.  -}
  , pOST40EdgesEdgeIdInterfaces :: Text -> Maybe Text -> InterfacesCreate -> m (){- ^ Add interfaces for a logical router.   -}
  , pOST40EdgesEdgeIdL2vpnConfig :: Text -> Maybe Bool -> m (){- ^ Enable or disable L2 VPN service.  -}
  , pOST40EdgesEdgeIdLoadbalancerAcceleration :: Text -> Maybe Bool -> m (){- ^ Configure load balancer acceleration mode. -}
  , pOST40EdgesEdgeIdLoadbalancerConfigApplicationprofiles :: Text -> ApplicationProfilesCreate -> m (){- ^ Add an application profile. -}
  , pOST40EdgesEdgeIdLoadbalancerConfigApplicationrules :: Text -> AppRulesCreate -> m (){- ^ Add an application rule. -}
  , pOST40EdgesEdgeIdLoadbalancerConfigMembersMemberID :: Text -> Text -> Maybe Bool -> m (){- ^ Update enabled status of the specified member. -}
  , pOST40EdgesEdgeIdLoadbalancerConfigMonitors :: Text -> LbMonitorsCreate -> m (){- ^ Add a load balancer monitor. -}
  , pOST40EdgesEdgeIdLoadbalancerConfigPools :: Text -> PoolsCreate -> m (){- ^ Add a load balancer server pool to the Edge.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method updated. Member **condition** can be set to *drain*.  -}
  , pOST40EdgesEdgeIdLoadbalancerConfigVirtualservers :: Text -> VirtualServersCreate -> m (){- ^ Add a virtual server.  You can add an NSX Edge internal or uplink interface as a virtual server.  See *Working With NSX Edge Load Balancer* for **virtualServer** parameter information.  -}
  , pOST40EdgesEdgeIdLogging :: Text -> Maybe Text -> m (){- ^ Modify log setting. -}
  , pOST40EdgesEdgeIdNatConfigRules :: Text -> Maybe Text -> EdgeNatRulesCreate -> m (){- ^ Add a NAT rule above a specific rule in the NAT rules table (using **aboveRuleId** query parameter) or append NAT rules to the bottom.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **vnic** parameter is now optional. The **originalAddress** for DNAT rules, and the **translatedAddress** for SNAT rules is no longer required to be a IP configured on one of the NSX Edge vNics. 6.3.0 | Method updated. **dnatMatchSourceAddress**, **snatMatchDestinationAddress**, **dnatMatchSourcePort**, **snatMatchDestinationPort** parameters added. <br>**protocol**, **originalPort**, and **translatedPort** now supported in SNAT rules.  -}
  , pOST40EdgesEdgeIdParentVnicIndexSubinterfaces :: Text -> Text -> Text -> m (){- ^ Add an sub-interface of backing type VLAN or Network.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pOST40EdgesEdgeIdSslvpnConfig :: Text -> Maybe Bool -> m (){- ^ Enable or disable SSL VPN on the NSX Edge appliance.  -}
  , pOST40EdgesEdgeIdSslvpnConfigAuthLocalserverUsers :: Text -> UsersCreate -> m (){- ^ Add a new portal user. -}
  , pOST40EdgesEdgeIdSslvpnConfigAuthSettingsRsaconfigfile :: Text -> m (){- ^ Upload RSA config file (See \"Generate the Authentication Manager Configuration File\" section of the RSA Authentication Manager Administrator's guide for instructions on how to configure and download the RSA config file from RSA Authentication Manager).  -}
  , pOST40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages :: Text -> InstallPackagesCreate -> m (){- ^ Creates setup executables (installers) for full access network clients. These setup binaries are later downloaded by remote clients and installed on their systems. The primary parameters needed to configure this setup are hostname of the gateway, and its port and a profile name which is shown to the user to identify this connection. The Administrator can also set other parameters such as whether to automatically start the application on windows login, or hide the system tray icon.  -}
  , pOST40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools :: Text -> NetExtipPoolsCreate -> m (){- ^ Create an IP pool. -}
  , pOST40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks :: Text -> PrivateNetworksCreate -> m (){- ^ Configure a private network. -}
  , pOST40EdgesEdgeIdSslvpnConfigLayoutImagesImageType :: Text -> Text -> m (){- ^ Upload images for use with SSL VPN portal and client.  You can upload a logo to use in the SSL VPN portal, and a banner and icons to use in the SSL VPN client.  You must upload the image files using the form-data content-type. Consult the documentation for your REST client for instructions.   Do not set other Content-type headers in your request, for example, *Content-type: application/xml*.  When you upload a file as form-data, you must provide a **key** and a **value** for the file. See the table below for the form-data **key** to use for each image type. The **value** is the path to the image file.  Image Type | form-data key | Image format requirements ----|------|---- portallogo | layoutFile | n/a phatbanner | banner | bmp connecticon | icon | ico disconnecticon | icon | ico erroricon | icon | ico desktopicon | icon | ico  **Example using curl**  ``` /usr/bin/curl -v -k -i -F layoutFile=@/tmp/portalLogo.jpg -H 'Authorization: Basic YWRtaW46ZGXXXXXXXX=='  https://192.168.110.42/api/4.0/edges/edge-3/sslvpn/config/layout/images/portallogo ```  -}
  , pOST40EdgesEdgeIdSslvpnConfigScript :: Text -> ScriptCreate -> m (){- ^ Configure parameters associated with the uploaded script file.  -}
  , pOST40EdgesEdgeIdSslvpnConfigScriptFile :: Text -> m (){- ^ You can add multiple login or logoff scripts. For example, you can bind a login script for starting Internet Explorer with gmail.com. When the remote user logs in to the SSL client, Internet Explorer opens up gmail.com. This method returns a *scriptFileId* which can be used to update parameters associated with the script file.  You must upload the script files using the form-data content-type. Consult the documentation for your REST client for instructions.  Do not set other Content-type headers in your request, for example, *Content-type: application/xml*.  When you upload a file as form-data, you must provide a **key** and a **value** for the file. The **key** is *file*, and the **value** is the location of the script file.  **Example using curl** ``` /usr/bin/curl -v -k -i -F file=@/tmp/script.sh -H 'Authorization: Basic YWRtaW46ZGXXXXXXXX==' https://192.168.110.42/api/4.0/edges/edge-3/sslvpn/config/script/file/ ```  -}
  , pOST40EdgesEdgeIdTunnels :: Text -> Text -> m (){- ^ Create a tunnel on this Edge Services Gateway.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pOST40EdgesEdgeIdVnics :: Text -> Maybe Text -> Text -> m (){- ^ Add an interface or sub interface. -}
  , pOST40FirewallForceSyncID :: Text -> m (){- ^ Force sync host or cluster. -}
  , pOST40FirewallGlobalroot0ConfigLayer2sections :: Maybe Text -> Maybe Text -> DfwSection -> m (){- ^ Create a layer 2 distributed firewall section.  By default, the section is created at the top of the firewall table. You can specify a location for the section with the **operation** and **anchorId** query parameters.  See \"Working with Distributed Firewall Configuration\" for information about configuring **tcpStrict**, **stateless**, and **useSid** for a section.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict**, **stateless**, and **useSid** added as **section** attributes.  -}
  , pOST40FirewallGlobalroot0ConfigLayer2sectionsSectionId :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Maybe Text -> m (){- ^ Move the specified layer 2 section.  Use the **action**, **operation**, and optionally **anchorId** query parameters to specify the destination for the section.  `POST /api/4.0/firewall/globalroot-0/config/layer2sections/1009 ?action=revise&operation=insert_before&anchorId=1008`  `If-Match: 1478307787160`  ``` <section id=\"1009\" name=\"Test Section\" generationNumber=\"1478307787160\" timestamp=\"1478307787160\" type=\"LAYER2\">   ... </section> ```  -}
  , pOST40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRules :: Text -> DfwRule -> Maybe Text -> m (){- ^ Add rules to the specified layer 2 section in distributed firewall.  You add firewall rules at the global scope. You can then narrow down the scope (datacenter, cluster, distributed virtual port group, network, virtual machine, vNIC, or logical switch) at which you want to apply the rule. Firewall allows you to add multiple objects at the source and destination levels for each rule, which helps reduce the total number of firewall rules to be added.  To add a identity based firewall rule, first create a security group based on Directory Group objects. Then create a firewall rule with the security group as the source or destination.  Rules that direct traffic to a third part service are referred to as layer3 redirect rules, and are displayed in the layer3 redirect tab.  When Distributed Firewall is used with Service Composer, firewall rules created by Service Composer contain an additional attribute in the XML called managedBy.  Follow this procedure to add a rule:  * Retrieve the configuration for the specified section. * Retrieve the Etag value from the response headers.   **Note**: Each section contains its own Etag, generationNumber, and   timestamp. When adding a new rule, you must use the Etag value of the   firewall section to which you wish to add the rule. * Extract and modify the configuration from the response body as needed. * Set the If-Match header to the section Etag value, and submit the request.  Not all fields are required while sending the request. All the optional fields are safe to be ignored while sending the configuration to server. For example, if an IP set is referenced in the rule only IPSet and Type is needed in the Source/Destination objects and not Name and isValid tags.  When updating the firewall configuration:  * IDs for new rules should be removed or set to zero. * If new rules have been sent in the request, the response   will contain the system-generated IDs, which are assigned to these new   entities. * **appliedTo** can be any valid firewall rule element. * **action** can be *ALLOW*, *BLOCK*, or *REJECT*. REJECT sends reject message for   unaccepted packets; RST packets are sent for TCP connections and ICMP   unreachable code packets are sent for UDP, ICMP, and other IP connections * source and destination can have an exclude flag. For example, if you add an   exclude tag for 1.1.1.1 in the source parameter, the rule looks for traffic   originating from all IPs other than 1.1.1.1.  -}
  , pOST40FirewallGlobalroot0ConfigLayer3redirectsections :: Layer3RedirectSectionsCreate -> m (){- ^ Add L3 redirect section  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict**, **stateless**, and **useSid** added as **section** attributes.  -}
  , pOST40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRules :: Text -> RulesCreate -> m (){- ^ Add L3 redirect rule -}
  , pOST40FirewallGlobalroot0ConfigLayer3sections :: Maybe Text -> Maybe Text -> DfwSection -> m (){- ^ Create a layer 3 distributed firewall section.  By default, the section is created at the top of the firewall table. You can specify a location for the section with the **operation** and **anchorId** query parameters.  See \"Working with Distributed Firewall Configuration\" for information about configuring **tcpStrict**, **stateless**, and **useSid** for a section.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict**, **stateless**, and **useSid** added as **section** attributes.  -}
  , pOST40FirewallGlobalroot0ConfigLayer3sectionsSectionId :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Text -> Maybe Text -> m (){- ^ Move the specified layer 3 section.  Use the **action**, **operation**, and optionally **achorId** query parameters to specify the destination for the section.  `POST /api/4.0/firewall/globalroot-0/config/layer3sections/1007 ?action=revise&operation=insert_before&anchorId=1006`  `If-Match: 1477989118875`   ``` <section id=\"1007\" name=\"Web Section\" generationNumber=\"1477989118875\" timestamp=\"1477989118875\" type=\"LAYER3\">   ... </section> ```  -}
  , pOST40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRules :: Text -> DfwRule -> Maybe Text -> m (){- ^ Add rules to the specified layer 2 section in distributed firewall.  You add firewall rules at the global scope. You can then narrow down the scope (datacenter, cluster, distributed virtual port group, network, virtual machine, vNIC, or logical switch) at which you want to apply the rule. Firewall allows you to add multiple objects at the source and destination levels for each rule, which helps reduce the total number of firewall rules to be added.  To add a identity based firewall rule, first create a security group based on Directory Group objects. Then create a firewall rule with the security group as the source or destination.  Rules that direct traffic to a third part service are referred to as layer3 redirect rules, and are displayed in the layer3 redirect tab.  When Distributed Firewall is used with Service Composer, firewall rules created by Service Composer contain an additional attribute in the XML called managedBy.  Follow this procedure to add a rule:  * Retrieve the configuration for the specified section. * Retrieve the Etag value from the response headers.   **Note**: Each section contains its own Etag, generationNumber, and   timestamp. When adding a new rule, you must use the Etag value of the   firewall section to which you wish to add the rule. * Extract and modify the configuration from the response body as needed. * Set the If-Match header to the section Etag value, and submit the request.  Not all fields are required while sending the request. All the optional fields are safe to be ignored while sending the configuration to server. For example, if an IP set is referenced in the rule only IPSet and Type is needed in the Source/Destination objects and not Name and isValid tags.  When updating the firewall configuration:  * IDs for new rules should be removed or set to zero. * If new rules have been sent in the request, the response   will contain the system-generated IDs, which are assigned to these new   entities. * **appliedTo** can be any valid firewall rule element. * **action** can be *ALLOW*, *BLOCK*, or *REJECT*. REJECT sends reject message for   unaccepted packets; RST packets are sent for TCP connections and ICMP   unreachable code packets are sent for UDP, ICMP, and other IP connections * source and destination can have an exclude flag. For example, if you add an   exclude tag for 1.1.1.1 in the source parameter, the rule looks for traffic   originating from all IPs other than 1.1.1.1.  -}
  , pOST40FirewallGlobalroot0Drafts :: DfwDraftsCreate -> m (){- ^ Save a firewall configuration. -}
  , pOST40FirewallGlobalroot0DraftsActionImport :: DfwConfigImport -> m (){- ^ Import a configuration. -}
  , pOST40FirewallGlobalroot0Timeouts :: Text -> m (){- ^ Create a Distributed Firewall session timer configuration.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , pOST40FirewallObjectsStatusVmVmIDContainers :: Text -> Text -> m (){- ^ Get VM Status for the grouping object (container). The parameters in the *container* field are mandatory, and parameters in the *appliedTo* field are optional in the POST request body.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pOST40ServicesSpoofguardPolicies :: SpoofGuardPoliciesCreate -> m (){- ^ Create a SpoofGuard policy to specify the operation mode for networks.  **Note:** you must include the trailing slash for this URI: `/api/4.0/services/spoofguard/policies/`.  -}
  , pOST40ServicesSpoofguardPolicyID :: Text -> Maybe Text -> Maybe Text -> SpoofGuardPolicyApprove -> m (){- ^ Approve or publish IP addresses. -}
  , pUT10ApplianceManagementBackuprestoreBackupsettings :: ApplianceMgrBackupSettingsUpdate -> m (){- ^ Configure backups on the appliance manager.  You must set a **passPhrase** for the backups. The passphrase is used to encrypt and decrypt backup files. If you do not set a passphrase, backups will fail. If you forget the passphrase set on a backup file, you cannot restore that backup file.  **Method history:**  Release | Modification --------|------------- 6.3.3 | Method updated. Parameters **passiveMode** and **useEPSV** previously defaulted to *false*, now default to *true*.  -}
  , pUT10ApplianceManagementBackuprestoreBackupsettingsExcludedata :: Text -> m (){- ^ Specify tables that need not be backed up. -}
  , pUT10ApplianceManagementBackuprestoreBackupsettingsFtpsettings :: Text -> m (){- ^ Configure FTP settings.  **Method history:**  Release | Modification --------|------------- 6.3.3 | Method updated. Parameters **passiveMode** and **useEPSV** previously defaulted to *false*, now default to *true*.  -}
  , pUT10ApplianceManagementBackuprestoreBackupsettingsSchedule :: Text -> m (){- ^ Set backup schedule. -}
  , pUT10ApplianceManagementSystemLocale :: SystemLocaleUpdate -> m (){- ^ Configure locale. -}
  , pUT10ApplianceManagementSystemNetwork :: Text -> m (){- ^ Update network information for the NSX Manager appliance.  -}
  , pUT10ApplianceManagementSystemNetworkDns :: ApplianceDnsClientUpdate -> m (){- ^ Configure DNS. -}
  , pUT10ApplianceManagementSystemSyslogserver :: SystemSyslogServerUpdate -> m (){- ^ Configures one syslog server. If there are syslog server(s) already configured, this API replaces the first one in the list. -}
  , pUT10ApplianceManagementSystemSyslogservers :: Text -> m (){- ^ Configure one or more syslog servers. Unconfigures all servers that were previously configured, and configures the one provided in the request body for this API. Duplicates are ignored.   **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pUT10ApplianceManagementSystemTimesettings :: SystemTimeUpdate -> m (){- ^ Configure time or specify the NTP server to use for time synchronization.  -}
  , pUT10DirectoryDeltaSyncDomainID :: Text -> m (){- ^ Start LDAP delta sync. -}
  , pUT10DirectoryFullSyncDomainID :: Text -> m (){- ^ Start LDAP full sync. -}
  , pUT10TelemetryConfig :: Text -> m (){- ^ Update the CEIP configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  6.3.3 | Method updated. *minutes* parameter is configurable.  -}
  , pUT10TelemetryProxy :: Text -> m (){- ^ Retrieve the NSX Manager proxy settings for CEIP.  **Method history:**  Release | Modification --------|------------- 6.3.3 | Method introduced.   -}
  , pUT20CapacityParametersThresholds :: Text -> m (){- ^ You can configure the scale threshold of the system. If you change the global threshold from 80 to 70, it means the System Scale dashboard displays warning when the system threshold reaches at 70%.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pUT20EndpointsecurityUsvmstatsUsvmhealththresholds :: Text -> m (){- ^ Update Guest Introspection service VM CPU and memory usage thresholds.  Valid values are *0-100*. The default value is *75*.  **Method history:**  Release | Modification --------|------------- 6.3.5 | Method introduced.  -}
  , pUT20NwfabricClustersClusterID :: Text -> NwfabricClustersUpdate -> m (){- ^ Update the locale ID for the specified cluster. -}
  , pUT20NwfabricConfigure :: NwFabricConfig -> m (){- ^ Upgrade Network virtualization components.  This API call can be used to upgrade network virtualization components. After NSX Manager is upgraded, previously prepared clusters must have the 6.x network virtualization components installed.  -}
  , pUT20NwfabricHostsHostID :: Text -> NwfabricHostsUpdate -> m (){- ^ Update the locale ID for the specified host. -}
  , pUT20ServicesApplicationApplicationId :: Text -> ServiceUpdate -> m (){- ^ Modify the name, description, applicationProtocol, or port value of a service.  -}
  , pUT20ServicesApplicationgroupApplicationgroupId :: Text -> ServiceGroupUpdate -> m (){- ^ Modify the name, description, applicationProtocol, or port value of the specified service group.  -}
  , pUT20ServicesApplicationgroupApplicationgroupIdMembersMoref :: Text -> Text -> m (){- ^ Add a member to the service group. -}
  , pUT20ServicesAuthTokenexpiration :: Text -> m (){- ^ Update the default token expiry time.  The default expiry time is 90 minutes. The maximum expiry time is 24 hours.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.   -}
  , pUT20ServicesConfiguration :: Text -> m (){- ^ Update the configuration for the High CPU Usage Reporting Tool. -}
  , pUT20ServicesDashboardUiViewsDashboardWidgetconfigurationsWidgetconfigurationId :: Text -> m (){- ^ Updates the configuration about a specific widget on the dashboard. For example, *LabelValueConfiguration*,  PUT https://<nsx-mgr>/api/2.0/services/dashboard/ui-views/dashboard/widgetconfigurations/ LabelValueConfiguration_497802b7-e0d9-48b3-abfd-479058540956.          **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.       -}
  , pUT20ServicesHousekeepingManagementIndexMaintenance :: Text -> m (){- ^ Update the index maintenance default settings. You can enable or disable the settings and change the CRON configuration. To make the changes effective, you must restart the NSX Manager. To change the CRON expression,  make sure the new CRON expression is correct using any CRON evaluators. Note that incorrect CRON expression will not run the reindexing task at the expected frequency.  **CRON expression guidelines**:       CRON expression pattern is a list of six single space-separated fields,representing second, minute, hour, day, month, weekday. Month and weekday can be given as first three letters of the English names.   You can refer to the following Web sites for details:        *  https://docs.spring.io/spring/docs/current/javadoc-api/org/springframework/scheduling/support/CronSequenceGenerator.html   *  http://www.manpagez.com/man/5/crontab/  **Method history:**    Release | Modification   --------|------------- 6.3.3 | Method introduced.   -}
  , pUT20ServicesIpamPoolsPoolId :: Text -> IpPoolUpdate -> m (){- ^ To modify an IP pool, query the IP pool first. Then modify the output and send it back as the request body.  -}
  , pUT20ServicesIpsetIpsetId :: Text -> IpsetUpdate -> m (){- ^ Modify an existing IP set. -}
  , pUT20ServicesMacsetMacsetId :: Text -> MacSetCreateUpdate -> m (){- ^ Modify an existing MAC address set. -}
  , pUT20ServicesPolicySecuritypolicyID :: Text -> SecurityPolicyIDUpdate -> m (){- ^ Edit a security policy.  To update a security policy, you must first fetch it. Then edit the received XML and pass it back as the input. The specified configuration replaces the current configuration.  Security group mappings provided in the PUT call replaces the security group mappings for the security policy. To remove all mappings, delete the securityGroupBindings parameter.  You can add or update actions for the security policy by editing the actionsByCategory parameter. To remove all actions (belonging to all categories), delete the actionsByCategory parameter. To remove actions belonging to a specific category, delete the block for that category.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tag** parameter added. You can specify  *tag* for the firewall rule.  -}
  , pUT20ServicesPolicySecuritypolicyIDSgbindingSecurityGroupId :: Text -> Text -> m (){- ^ Apply the specified security policy to the specified security group.  -}
  , pUT20ServicesPolicySecuritypolicyServiceproviderFirewall :: Text -> m (){- ^ Update the Service Composer firewall applied to setting.  -}
  , pUT20ServicesSecuritygroupBulkObjectId :: Text -> SecGroupBulkUpdate -> m (){- ^ Update configuration for the specified security group, including membership information.  -}
  , pUT20ServicesSecuritygroupObjectId :: Text -> SecGroupObjectUpdate -> m (){- ^ Update configuration for the specified security group. Members are not updated. You must use `PUT /2.0/services/securitygroup/bulk/{objectId}` to update a security group membership.  -}
  , pUT20ServicesSecuritygroupObjectIdMembersMemberId :: Text -> Text -> Maybe Bool -> m (){- ^ Add a new member to the specified security group.  -}
  , pUT20ServicesSecuritytagsSelectionCriteria :: Text -> m (){- ^ Configure the unique ID section criteria configuration.  If you set the selection criteria and assign security tags to VMs, you must remove all security tags from VMs before you can change the selection criteria.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , pUT20ServicesSecuritytagsTagTagIdVmVmId :: Text -> Text -> m (){- ^ Apply a security tag to the specified virtual machine.  **Note:** this method can attach a universal security tag to a virtual machine. However, this method checks that the VM exists on the NSX Manager to which the API call is sent. In a cross-vCenter active active environment, the VM might exist on a secondary NSX Manager, and so the call would fail.   You can instead use the `POST /api/2.0/services/securitytags/tag/{tagId}/vm?action=attach` method to attach universal security tags to a VM that is not local to the primary NSX Manager. This method does not check that the VM is local to the NSX Manager.  -}
  , pUT20ServicesSnmpManagerManagerId :: Text -> Text -> m (){- ^ Update an SNMP manager configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pUT20ServicesSnmpStatus :: Text -> m (){- ^ Update SNMP status settings.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pUT20ServicesSnmpTrapOid :: Text -> Text -> m (){- ^ Update the specified SNMP trap.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pUT20ServicesTruststoreConfig :: m (){- ^ Update certificate expiry notification duration in days. This duration is used to generate notification before the certificate expires, which helps you to monitor and renew certificates.  Default value for the expiry notification is 7 days. This API is available to Enterprise Administrator, NSX Administrator, and Security Administrator roles.  **Method history:**  Release | Modification --------|------------- 6.4.1 | Method introduced.  -}
  , pUT20ServicesTruststoreCsrCsrId :: Text -> Maybe Text -> m (){- ^ Create a self-signed certificate for CSR.  -}
  , pUT20ServicesUsermgmtRoleUserId :: Text -> UserRoleMgmtUpdate -> m (){- ^ Change a user's role. -}
  , pUT20ServicesUsermgmtUserUserIdEnablestateValue :: Text -> Int -> m (){- ^ You can disable or enable a user account, either local user or vCenter user. When a user account is created, the account is enabled by default.  -}
  , pUT20ServicesVcconfig :: VcConfig -> m (){- ^ Synchronize NSX Manager with vCenter server. -}
  , pUT20SiDeploy :: Maybe Text -> ServiceUpgrade -> m (){- ^ Upgrade service to recent version.  The datastore, dvPortGroup, and ipPool variables should either not be specified or have same value as provided at time of deployment.  -}
  , pUT20SiFabricSyncConflicts :: Text -> m (){- ^ Create deployment units for conflicting EAM Agencies, delete conflicting EAM agencies, or delete deployment units for conflicting EAM agencies.  ### Create deployment units for conflicting EAM agencies  ``` <conflictResolverInfo>   <agencyAction>RESTORE</agencyAction> </conflictResolverInfo> ```  ### Delete conflicting EAM agencies  ``` <conflictResolverInfo>   <agencyAction>DELETE</agencyAction> </conflictResolverInfo> ```  ### Delete deployment units for conflicting EAM agencies  ``` <conflictResolverInfo>   <deploymentUnitAction>DELETE</deploymentUnitAction> </conflictResolverInfo> ```  -}
  , pUT20UniversalsyncConfigurationNsxmanagersNsxManagerID :: Text -> Text -> m (){- ^ Update the the specified secondary NSX manager IP or thumbprint in the universal sync configuration.  -}
  , pUT20VdnBfdConfigurationGlobal :: m (){- ^ Update the BFD global configuration.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pUT20VdnConfigMulticastsMulticastAddresssRangeId :: Text -> VdnMulticastUpdate -> m (){- ^ Update the specified multicast address range.  If the multicast address range is universal you must send the API request to the primary NSX Manager.  -}
  , pUT20VdnConfigSegmentsSegmentPoolId :: Text -> VdnSegmentUpdate -> m (){- ^ Update the specified segment ID pool.  If the segment ID pool is universal you must send the API request to the primary NSX Manager.  -}
  , pUT20VdnConfigVxlanUdpPortPortNumber :: Text -> Maybe Bool -> m (){- ^ Update the VXLAN port configuration to use port *portNumber*.  This method changes the VXLAN port in a three phrase process, avoiding disruption of VXLAN traffic. In a cross-vCenter NSX environment, change the VXLAN port on the primary NSX Manager to propagate this change on all NSX Managers and hosts in the cross-vCenter NSX environment.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. Port change is now non-disruptive, and propagates to secondary NSX Managers if performed on the primary NSX Manager. Force parameter added.  -}
  , pUT20VdnControllerCluster :: ClusterUpdate -> m (){- ^ Modify cluster wide configuration information for controller.  -}
  , pUT20VdnControllerClusterNtp :: Text -> m (){- ^ Update NTP configuration for the NSX Controller cluster.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pUT20VdnControllerControllerId :: Text -> Text -> m (){- ^ Update the name of the controller. The name must not contain spaces  or underscores.  When you update the controller name, the following changes are made:  * the name displayed in the Networking & Security UI is changed to *newName* * the VM's hostname is changed to *newName-NSX-&lt;controller_id&gt;*  * the VM name is vSphere is changed to *newName-NSX-&lt;controller_id&gt;*  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pUT20VdnControllerCredential :: NsxControllerPasswordUpdate -> m (){- ^ Change the NSX controller password. -}
  , pUT20VdnControllerSynchronize :: m (){- ^ Synchronize the controller cluster with the NSX Manager database.  -}
  , pUT20VdnHardwaregatewayBfdConfig :: Text -> m (){- ^ Update global hardware gateway BFD configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pUT20VdnHardwaregatewayBindingsBindingId :: Text -> Text -> m (){- ^ Update the specified hardware gateway binding.  You can update the binding parameters. This API will fail if: * the specified *hardwareGatewayId* does not exist. * the specified logical switch (*virtualWire*) is not present or there is a software   gateway on the binding. * the new binding value is a duplicate of an existing binding.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pUT20VdnHardwaregatewaysHardwareGatewayId :: Text -> Text -> m (){- ^ Update the specified hardware gateway.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pUT20VdnHardwaregatewaysReplicationcluster :: Text -> m (){- ^ Update the hardware gateway replication cluster.  Add or remove hosts on a replication cluster.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pUT20VdnPnicCheckConfigurationGlobal :: m (){- ^ Update the global configuration for pNIC status check.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pUT20VdnScopesScopeIdAttributes :: Text -> VdnScopeUpdate -> m (){- ^ Update the attributes of a transport zone.  For example, you can update the name, description, or control plane mode. You must include the cluster object IDs for the transport zone in the request body.  -}
  , pUT20VdnVirtualwiresVirtualWireID :: Text -> LogicalSwitchUpdate -> m (){- ^ Update the specified logical switch.  For example, you can update the name, description, or control plane mode.  -}
  , pUT20XvsNetworksIDFeatures :: Text -> ArpMACUpdate -> m (){- ^ Enable or disable IP discovery and MAC learning.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. IP discovery can be disabled on secondary NSX Managers.  -}
  , pUT21AppExcludelistMemberID :: Text -> m (){- ^ Add a vm to the exclusion list. -}
  , pUT21AppFlowConfig :: FlowsExcludeCreate -> m (){- ^ Update flow monitoring configuration. -}
  , pUT40EdgePublishTuningConfiguration :: Text -> m (){- ^ Update the NSX Edge tuning configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method introduced.  -}
  , pUT40EdgesEdgeId :: Text -> NsxEdgeUpdate -> m (){- ^ Update the NSX Edge configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **haAdminState** parameter added. 6.3.0 | Method updated. **dnatMatchSourceAddress**, **snatMatchDestinationAddress**, **dnatMatchSourcePort**, **snatMatchDestinationPort** parameters added. <br>**protocol**, **originalPort**, and **translatedPort** now supported in SNAT rules. 6.4.0 | Method updated. New parameter **ipsecSessionType** added under the *site* section. This is a read-only parameter, and optional if used in a PUT call. If used, it must be set to *policybasedSession*.  -}
  , pUT40EdgesEdgeIdAppliances :: Text -> m (){- ^ You can retrieve the configuration of both appliances by using the GET call and replace the size, resource pool, datastore, and custom parameters of the appliances by using a PUT call. If there were two appliances earlier and you PUT only one appliance, the other appliance is deleted.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **haAdminState** parameter added.  -}
  , pUT40EdgesEdgeIdAppliancesHaIndex :: Text -> Text -> ApplianceUpdate -> m (){- ^ Update the configuration of the specified appliance.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **haAdminState** parameter added.  -}
  , pUT40EdgesEdgeIdAutoconfiguration :: Text -> AutoConfigUpdate -> m (){- ^ Update the auto configuration settings for the NSX Edge.  -}
  , pUT40EdgesEdgeIdBridgingConfig :: Text -> BridingUpdate -> m (){- ^ Configure a bridge. Note that the bridging is always enabled for Distributed Logical Router and is unsupported for Edge Services Gateway.  You cannot disable the bridging by setting the *enable* field to *false*. The value for the *enable* field is not honored. -}
  , pUT40EdgesEdgeIdClisettings :: Text -> CliSettingsUpdate -> m (){- ^ Modify CLI credentials and enable/disable SSH for Edge.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. Modified existing API to enable SSH on edge without changing the password. Now you can enable SSH without mentioning the password. If password is provided, the provided password is saved in the database. If password is not provided, NSX Manager will retain password from the database.  -}
  , pUT40EdgesEdgeIdDhcpConfig :: Text -> DhcpUpdate -> m (){- ^ Configure DHCP service.  **Method History**  Release | Modification --------|------------- 6.2.3 | Method updated. DHCP options added.  -}
  , pUT40EdgesEdgeIdDhcpConfigRelay :: Text -> DhcpRelayUpdate -> m (){- ^ Configure DHCP relay. -}
  , pUT40EdgesEdgeIdDnsConfig :: Text -> EdgeDnsUpdate -> m (){- ^ Configure DNS servers. -}
  , pUT40EdgesEdgeIdDnsclient :: Text -> EdgeDnsClientUpdate -> m (){- ^ Update Edge DNS settings. -}
  , pUT40EdgesEdgeIdFirewallConfig :: Text -> NsxEdgeFirewallConfigUpdate -> m (){- ^ Configure NSX Edge firewall.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **enableSynFloodProtection** parameter added. Default value of **tcpTimeoutEstablished** increased from 3600 to 21600 seconds (6 hours). 6.3.0 | Method updated. **logIcmpErrors** and **dropIcmpReplays** parameters added.  6.4.0 | Method updated. *nat64* is now a possible value for **vnicGroupId** for source and destination.  -}
  , pUT40EdgesEdgeIdFirewallConfigDefaultpolicy :: Text -> DefaultFirewallPolicyUpdate -> m (){- ^ Configure default firewall policy -}
  , pUT40EdgesEdgeIdFirewallConfigGlobal :: Text -> GlobalFirewallConfigUpdate -> m (){- ^ Configure firewall global config for an Edge.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **enableSynFloodProtection** parameter added. Default value of **tcpTimeoutEstablished** increased from 3600 to 21600 seconds (6 hours). 6.3.0 | Method updated. **logIcmpErrors** and **dropIcmpReplays** parameters added.   -}
  , pUT40EdgesEdgeIdFirewallConfigRulesRuleId :: Text -> Text -> FirewallRuleUpdate -> m (){- ^ Modify a specific firewall rule. -}
  , pUT40EdgesEdgeIdHighavailabilityConfig :: Text -> HighAvailabilityCreate -> m (){- ^ Configure high availability.  * **ipAddress** - Optional. A pair of ipAddresses with /30 subnet   mandatory, one for each appliance. If provided, they must NOT   overlap with any subnet defined on the Edge vNics. If not specified,   a pair of IPs will be picked up from the reserved subnet,   169.254.0.0/16. * **declareDeadTime** Optional. The default is 6 seconds.  * **enabled** - Optional. The default is set to true. The enabled flag   will cause the HA appliance to be deployed or destroyed.  -}
  , pUT40EdgesEdgeIdInterfacesIndex :: Text -> Text -> m (){- ^ Update interface configuration for the specified logical router interface.  -}
  , pUT40EdgesEdgeIdIpsecConfig :: Text -> Text -> m (){- ^ Update IPsec VPN configuration.   **Method history:**    Release | Modification  --------|-------------  6.4.0 | Method updated. New parameters **ikeOptions** and **digestAlgorithm** added. New parameter **ipsecSessionType** added under the *site* section. This is a read-only parameter, and optional if used in a PUT call. If used, it must be set to *policybasedSession*.  -}
  , pUT40EdgesEdgeIdL2vpnConfig :: Text -> Text -> m (){- ^ Configure L2VPN for server or client.  You first enable the L2 VPN service on the NSX Edge instance and then configure a server and a client.  **L2 VPN Parameters**  Parameter |  Description | Comments ---|---|--- **enabled**      |Whether L2 VPN is enabled.|Optional. Boolean. Options are *True* or *False*. Default is *True*.  **logging**      |L2 VPN logging setting.|Optional. Disable by default. **logging > enable**      |Whether logging is enabled.|Optional. Boolean. Options are *True* or *False*. Default is *False*. **logging > logLevel**      |Logging level.|Optional. Options are: EMERGENCY, ALERT, CRITICAL, ERROR, WARNING, NOTICE, INFO, and DEBUG. Default is *INFO*. **listenerIp** | IP of external interface on which L2VPN service listens to. |Required. **listenerPort** | Port on which L2VPN service listens to. |Optional. Default is 443. **encryptionAlgorithm** | Encryption algorithm for communication between the server and the client. |Mandatory. Supported ciphers are *RC4-MD5*, *AES128-SHA*, *AES256-SHA*, *DES-CBC3-SHA*, *AES128-GCM-SHA256*, and *NULL-MD5*. **serverCertificate** | Select the certificate to be bound to L2 VPN server. |Optional. If not specified server will use its default (self-signed) certificate.  **Peer Site Parameters**  Parameter |  Description | Comments ---|---|--- **peerSites **| To connect multiple sites to the L2 VPN server. |Required. Minimum one peer site must be configured to enable L2 VPN server service.  **name **| Unique name for the site getting configured. |Required. **description **| Description about the site. |Optional. **l2VpnUser **| Every peer site must have a user configuration. |Required. **l2VpnUser > userId **| L2 VPN user ID. |Required. **l2VpnUser > password **| Password for L2 VPN user. |Required. **vnics**| List of vNICs to be stretched over the tunnel. |Required. **vnics > index** | Select the virtual machine NIC to bind to the IP address. |Required. **egressOptimization > gatewayIpAddress** | The gateway IP addresses for which the traffic should be locally routed or for which traffic is to be blocked over the tunnel. |Optional. **enabled**| Whether the peer site is enabled.|Optional. Boolean. Options are *True* or *False*. Default is *True*.      **Example to configure L2 VPN for Client**      <l2Vpn>       <enabled>true</enabled>       <logging>         <enable>false</enable>         <logLevel>info</logLevel>       </logging>       <l2VpnSites>         <l2VpnSite>           <client>             <configuration>               <serverAddress>192.168.15.23</serverAddress>               <serverPort>443</serverPort>               <vnic>10</vnic>               <encryptionAlgorithm>AES128-SHA</encryptionAlgorithm>               <caCertificate>certificate-4</caCertificate>               <egressOptimization>                 <gatewayIpAddress>192.168.15.1</gatewayIpAddress>               </egressOptimization>             </configuration>             <proxySetting>               <type>https</type>               <address>10.112.243.202</address>               <port>443</port>               <userName>root</userName>               <password>java123</password>             </proxySetting>             <l2VpnUser>               <userId>apple</userId>               <password>apple</password>             </l2VpnUser>           </client>         </l2VpnSite>       </l2VpnSites>     </l2Vpn>            **Example to configure L2 VPN for Server**      <l2Vpn>       <enabled>true</enabled>       <logging>         <enable>false</enable>         <logLevel>info</logLevel>       </logging>       <l2VpnSites>         <l2VpnSite>           <server>             <configuration>               <listenerIp>192.168.15.65</listenerIp>               <listenerPort>443</listenerPort>               <encryptionAlgorithm>RC4-MD5</encryptionAlgorithm>               <peerSites>                 <peerSite>                   <name>PeerSite1</name>                   <description>description</description>                   <l2VpnUser>                     <userId>apple</userId>                     <password>apple</password>                   </l2VpnUser>                   <vnics>                     <index>10</index>                   </vnics>                   <egressOptimization>                     <gatewayIpAddress>192.168.15.1</gatewayIpAddress>                   </egressOptimization>                   <enabled>true</enabled>                 </peerSite>               </peerSites>             </configuration>           </server>         </l2VpnSite>       </l2VpnSites>     </l2Vpn>  -}
  , pUT40EdgesEdgeIdLoadbalancerConfig :: Text -> LoadBalancerConfig -> m (){- ^ Configure load balancer.  The input contains five parts: application profile, virtual server, pool, monitor and application rule.  For the data path to work, you need to add firewall rules to allow required traffic as per the load balancer configuration.  **General Load Balancer Parameters**  Parameter |  Description | Comments ---|---|---   **logging**      |Load balancer logging setting.|Optional.   **enable**     |Whether logging is enabled.|Optional. Options are *True* or *False*. Default is *False*.   **logLevel**     |Logging level.|Optional. Options are: *EMERGENCY*, *ALERT*, *CRITICAL*, *ERROR*, *WARNING*, *NOTICE*, *INFO*, and *DEBUG*. Default is *INFO*.   **accelerationEnabled**      |Whether **accelerationEnabled** is enabled.|Optional. Options are *True* or *False*. Default is *False*.   **enabled**      |Whether load balancer is enabled.|Optional. Options are *True* or *False*. Default is *True*.     **Parameter Table for Monitors**    Parameter |  Description | Comments  ---|---|---   **monitor**      |Monitor list.|Optional.   **monitorId**     |Generated monitor identifier.|Optional. Required if it is used in a pool.   **name**     |Name of the monitor.|Required.   **type**     |Monitor type.|Required. Options are : *HTTP*, *HTTPS*, *TCP*, *ICMP*, *UDP*.   **interval**     |Interval in seconds in which a server is to be tested.|Optional. Default is *5*.   **timeout**     |Timeout value is the maximum time in seconds within which a response from the server must be received.|Optional. Default is *15*.   **maxRetries**     |Maximum number of times the server is tested  before it is declared DOWN.|Optional. Default is *3*.   **method**     |Method to send the health check request to the server.|Optional. Options are: *OPTIONS*, *GET*, *HEAD*, *POST*, *PUT*, *DELETE*, *TRACE*, *CONNECT*. Default is *GET* for HTTP monitor.   **url**     |URL to *GET* or *POST*.|Optional. Default is *\"/\"* for HTTP monitor.   **expected**     |Expected string.|Optional. Default is \"HTTP/1\" for HTTP/HTTPS protocol.   **send**     |String to be sent to the backend server after a connection is established.|Optional. URL encoded HTTP POST data for HTTP/HTTPS protocol.   **receive**     |String to be received from the backend server for HTTP/HTTPS protocol.|Optional.   **extension**     |Advanced monitor configuration.|Optional.    **Parameter Table for Virtual Servers**   Parameter |  Description | Comments  ---|---|---   **virtualServer**      |Virtual server list.|Optional. 0-64 **virtualServer** items can be added   **name**     |Name of the virtual server.|Required. Unique virtualServer name per NSX Edge.   **description**     |Description of the virtual server.|Optional.   **enabled**     |Whether the virtual server is enabled.|Optional. Boolean. Options are *True* or *False*. Default is *True*.   **ipAddress**     |IP address that the load balancer  is listening on. |Required. A valid Edge vNic IP address (IPv4 or IPv6).   **protocol**     |Virtual server protocol.|Required. Options are: *HTTP*, *HTTPS*, *TCP*, *UDP*.   **port**    |Port number or port range.|Required. Port number such as *80*, port range such as *80,443* or *1234-1238*, or a combination such as *443,6000-7000*. Valid range: 1-65535.   **connectionLimit**     |Maximum concurrent connections.|Optional. Long. Default is *0*.   **connectionRateLimit**     |Maximum incoming new connection requests per second.|Optional. Long. Default is *null*.   **defaultPoolId**     |Default pool ID.|Optional.   **applicationProfileId**     |Application profile ID.|Optional.    **accelerationEnabled**     |Use the faster L4 load balancer  engine rather than L7 load  balancer engine.|Optional. Boolean. Options are *True* or *False*. Default is *False*. If a virtual server configuration such as application rules, HTTP type, or cookie persistence, is using the L7 load balancer engine, then the L7 load balancer engine is used, even if **accelerationEnabled** is not set to true.   **applicationRuleId**     |Application rule ID list.|Optional.  **Parameter Table for Pools**   Parameter |  Description | Comments  ---|---|---   **pool**      |Pool list.|Optional.   **poolId**     |Generated pool identifier.|Optional. Required if you specify pool object.   **name**     |Name of the pool.|Required.   **description**     |Description of the pool.|Optional.   **algorithm**     |Pool member balancing algorithm.|Optional. Options are: *round-robin*, *ip-hash*, *uri*, *leastconn*, *url*, *httpheader*. Default is *round-robin*.   **algorithmParameters**     |Algorithm parameters for *httpheader*, *url*. |Optional. Required for *url*,*httpheader* algorithm.   **transparent**     |Whether client IP addresses are  visible to the backend servers.|Optional. Options are *True* or *False*. Default is *False*.   **monitorId**     |Monitor identifier list.|Optional. Only one monitor is supported.   **member**     |Pool member list.|Optional.   **memberId**    |Generated member identifier.|Optional. Required if you specify member object.   **name**    |Member name.|Optional. Required when it is used in ACL rule.   **ipAddress**    |Member IP address (IPv4/IPv6).|Optional. Required if **groupingObjectId** is null.   **groupingObjectId**    |Member grouping object identifier.|Optional. Required if **ipAddress** is null.   **groupingObjectName**    |Member grouping object name.|Optional.   **weight**    |Member weight.|Optional. Default is *1*.   **monitorPort**    |Monitor port.|Optional. Long. Either  **monitorPort** or **port** must be configured.   **port**    |Member port.|Optional. Long. Either  **monitorPort** or **port** must be configured.   **maxConn**    |Maximum number of concurrent connections a member can handle.|Optional. Default is *0* which means unlimited.   **minConn**    |Minimum number of concurrent connections a member can handle.|Optional. Default is *0* which means unlimited.   **condition**    |Whether the member is enabled or disabled.|Optional. Options are: *enabled* or *disabled*. Default is *enabled*.  **Parameter Table for Application Profiles**  Parameter |  Description | Comments ---|---|---   **applicationProfile**      |Application profile list.|Optional.   **applicationProfileId**     |Generated application profile identifier.|Optional. Required if it is used in virtual server.   **name**     |Name of application profile.|Required.   **persistence**     |Persistence setting.|Optional.   **method**    |Persistent method.|Required. Options are: *cookie*, *ssl_sessionid*, *sourceip*, *msrdp*.   **cookieName**    |Cookie name.|Optional.   **cookieMode**    |Cookie mode.|Optional. Options are: *insert*, *prefix*, *app*.   **expire**    |Expire time.|Optional.   **insertXForwardedFor**     |Whether **insertXForwardedFor** is enabled.|Optional. Boolean. Options are *True* or *False*. Default is *False*.   **sslPassthrough**     |Whether **sslPassthrough** is enabled.|Optional. Boolean. Options are *True* or *False*. Default is *False*.   **httpRedirect**     |HTTP redirect setting.|Optional.   **to**    |HTTP redirect to.|Required. Required if **httpRedirect** is specified.   **serverSslEnabled**     |Whether **serverSsl** offloading is enabled.|Optional. Boolean. Options are *True* or *False*.   **serverSsl**     |Server SSL setting.|Optional.   **ciphers**    |Cipher suites.|Optional. Options are: *DEFAULT* *ECDHE-RSA-AES128-GCM-SHA256*, *ECDHE-RSA-AES256-GCM-SHA384*, *ECDHE-RSA-AES256-SHA*, *ECDHE-ECDSA-AES256-SHA*, *ECDH-ECDSA-AES256-SHA*, *ECDH-RSA-AES256-SHA*, *AES256-SHA AES128-SHA*, *DES-CBC3-SHA*. Default is *DEFAULT*.   **serviceCertificate**    |Service certificate identifier list.|Optional. Only one certificate is supported.   **caCertificate**    |CA identifier list.|Optional. Required if **serverAuth** is required.   **crlCertificate**    |CRL identifier list.|Optional.   **serverAuth**    |Whether peer certificate should be verified.|Optional. Options are *Required* or *Ignore*. Default is *Ignore*.   **clientSsl**     |Client SSL setting.|Optional.   **ciphers**    |Cipher suites.|Optional. Options are: *DEFAULT* *ECDHE-RSA-AES128-GCM-SHA256*, *ECDHE-RSA-AES256-GCM-SHA384*, *ECDHE-RSA-AES256-SHA*, *ECDHE-ECDSA-AES256-SHA*, *ECDH-ECDSA-AES256-SHA*, *ECDH-RSA-AES256-SHA*, *AES256-SHA AES128-SHA*, *DES-CBC3-SHA*. Default is *DEFAULT*.   **serviceCertificate**    |Service certificate identifier list.|Required. Only one certificate is supported.   **caCertificate**    |CA identifier list.|Optional.   **crlCertificate**    |CRL identifier list.|Optional.   **clientAuth**    |Whether peer certificate should be verified.|Optional. Options are *Required* or *Ignore*. Default is *Ignore*.    **Parameter Table for Application Rules**  Parameter |  Description | Comments ---|---|---   **applicationRule**      |Application rule list.|Optional.    **applicationRuleId**     |Generated application rule identifier.|Optional.    **name**     |Name of application rule.|Required.   **script**     |Application rule script.|Required.    For the data path to work, you need to add firewall rules to allow required traffic as per the load balancer configuration.   -}
  , pUT40EdgesEdgeIdLoadbalancerConfigApplicationprofilesAppProfileID :: Text -> Text -> ApplicationProfileUpdate -> m (){- ^ Modify an application profile. -}
  , pUT40EdgesEdgeIdLoadbalancerConfigApplicationrulesAppruleID :: Text -> Text -> AppRuleUpdate -> m (){- ^ Modify an application rule. -}
  , pUT40EdgesEdgeIdLoadbalancerConfigMonitorsMonitorID :: Text -> Text -> LbMonitorUpdate -> m (){- ^ Modify a load balancer monitor. -}
  , pUT40EdgesEdgeIdLoadbalancerConfigPoolsPoolID :: Text -> Text -> PoolUpdate -> m (){- ^ Update the specified server pool.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method updated. Member **condition** can be set to *drain*.  -}
  , pUT40EdgesEdgeIdMgmtinterface :: Text -> MgmtInterfaceUpdate -> m (){- ^ Configure high availability (management) interface for logical (distributed) router.  See *Working With NSX Edge* for descriptions of parameters used to configure the logical router HA interface.  -}
  , pUT40EdgesEdgeIdNatConfig :: Text -> EdgeNatConfig -> m (){- ^ Configure NAT rules for an Edge.  If you use this method to add new NAT rules, you must include all existing rules in the request body. Any rules that are omitted will be deleted.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **vnic** parameter is now optional. The **originalAddress** for DNAT rules, and the **translatedAddress** for SNAT rules is no longer required to be a IP configured on one of the NSX Edge vNics. 6.3.0 | Method updated. **dnatMatchSourceAddress**, **snatMatchDestinationAddress**, **dnatMatchSourcePort**, **snatMatchDestinationPort** parameters added. <br>**protocol**, **originalPort**, and **translatedPort** now supported in SNAT rules. 6.4.0 | Method updated. NAT64 support added.  -}
  , pUT40EdgesEdgeIdNatConfigRulesRuleID :: Text -> Text -> EdgeNatRuleUpdate -> m (){- ^ Update the specified NAT rule.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **vnic** parameter is now optional. The **originalAddress** for DNAT rules, and the **translatedAddress** for SNAT rules is no longer required to be a IP configured on one of the NSX Edge vNics. 6.3.0 | Method updated. **dnatMatchSourceAddress**, **snatMatchDestinationAddress**, **dnatMatchSourcePort**, **snatMatchDestinationPort** parameters added. <br>**protocol**, **originalPort**, and **translatedPort** now supported in SNAT rules.  -}
  , pUT40EdgesEdgeIdRoutingConfig :: Text -> Maybe Double -> Maybe Double -> RoutingConfigUpdate -> m (){- ^ Configure NSX Edge global routing configuration, static routing, and dynamic routing (OSPF and BGP).  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **isis** configuration section removed.  6.3.0 | Method updated. Parameter **defaultOriginate** removed for logical router NSX Edges.  <br>Parameter **translateType7ToType5** added to OSPF section. <br>Parameters **localASNumber** and **remoteASNumber** added to BGP section. 6.4.0 | Method updated. Parameters **LE** and **GE** added. Parameter **removePrivateAS** added.  -}
  , pUT40EdgesEdgeIdRoutingConfigBgp :: Text -> RoutingBGPUpdate -> m (){- ^ Configure BGP.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **isis** configuration section removed.  6.3.0 | Method updated. Parameter **defaultOriginate** removed for logical router NSX Edges. <br>Parameters **localASNumber** and **remoteASNumber** added to BGP section. 6.4.0 | Method updated. Parameter **removePrivateAS** added.  -}
  , pUT40EdgesEdgeIdRoutingConfigGlobal :: Text -> Maybe Double -> Maybe Double -> RoutingGlobalConfigUpdate -> m (){- ^ Configure global route. -}
  , pUT40EdgesEdgeIdRoutingConfigOspf :: Text -> RoutingOSPFUpdate -> m (){- ^ Configure OSPF.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **isis** configuration section removed.  6.3.0 | Method updated. Parameter **defaultOriginate** removed for logical router NSX Edges.  <br>Parameter **translateType7ToType5** added to OSPF section.   -}
  , pUT40EdgesEdgeIdRoutingConfigStatic :: Text -> RoutingConfigStaticUpdate -> m (){- ^ Configure static and default routes. -}
  , pUT40EdgesEdgeIdSslvpnAuthLocalusersUsers :: Text -> AllUsersUpdate -> m (){- ^ Update all users with the given list of users. If the user is present, it is updated. Otherwise, and new user is created. Existing users not included in the body are deleted.  -}
  , pUT40EdgesEdgeIdSslvpnConfig :: Text -> m (){- ^ Update the entire SSL VPN configuration to the specified NSX Edge in a single call.  -}
  , pUT40EdgesEdgeIdSslvpnConfigAdvancedconfig :: Text -> AdvancedConfigUpdate -> m (){- ^ Update SSL VPN advanced configuration. -}
  , pUT40EdgesEdgeIdSslvpnConfigAuthLocalserverUsers :: Text -> UsersUpdate -> m (){- ^ Modify the portal user specified in the request body. -}
  , pUT40EdgesEdgeIdSslvpnConfigAuthSettings :: Text -> AuthSettingsUpdate -> m (){- ^ Update authentication settings for remote users. Specify username/password authentication, active directory, ldap, radius, client certificate based authentication.  -}
  , pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionClientconfig :: Text -> ClientConfigUpdate -> m (){- ^ Set advanced parameters for full access client configurations, such as whether client should auto-reconnect in case of network failures or network unavailability, or whether the client should be uninstalled after logout.  -}
  , pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages :: Text -> m (){- ^ Update all installation packages with the given list. If the package is present, it is updated; otherwise a new installation package is created. Existing packages not included in the body are deleted.  -}
  , pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackagesPackageID :: Text -> Text -> InstallPackageUpdate -> m (){- ^ Modify the specified installation package. -}
  , pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools :: Text -> NetExtipPoolsUpdate -> m (){- ^ Update all IP pools with the given list of pools. If the pool is present, it is updated; otherwise, a new pool is created. Existing pools not in the body are deleted.  -}
  , pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppoolsIppoolID :: Text -> Text -> Text -> m (){- ^ Update specified IP pool. -}
  , pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks :: Text -> m (){- ^ Update all private network configs of NSX Edge with the given list of private network configs. If the config is present, it is updated; otherwise, a new private network config is created. Existing configs not included in the call body are deleted.  -}
  , pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworksNetworkID :: Text -> Text -> PrivateNetworkUpdate -> m (){- ^ Update the specified private network in the SSL VPN service.  -}
  , pUT40EdgesEdgeIdSslvpnConfigLayout :: Text -> LayoutUpdate -> m (){- ^ Update the portal layout. -}
  , pUT40EdgesEdgeIdSslvpnConfigScript :: Text -> ScriptUpdate -> m (){- ^ Update all script configurations with the given list of configurations. If the config is present, its is updated; otherwise, a new config is created. Existing configs not included in the body are deleted.  -}
  , pUT40EdgesEdgeIdSslvpnConfigScriptFileID :: Text -> Text -> ScriptFileIDUpdate -> m (){- ^ Update parameters associated with the specified script file.  -}
  , pUT40EdgesEdgeIdSslvpnConfigServer :: Text -> ServerSettingsUpdate -> m (){- ^ Update server settings. -}
  , pUT40EdgesEdgeIdSyslogConfig :: Text -> SyslogUpdate -> m (){- ^ Configure syslog servers.  -}
  , pUT40EdgesEdgeIdSystemcontrolConfig :: Text -> Text -> m (){- ^ Update the NSX Edge system control (sysctl) configuration.  -}
  , pUT40EdgesEdgeIdTunnels :: Text -> Text -> m (){- ^ Update all tunnels on this Edge Services Gateway.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pUT40EdgesEdgeIdTunnelsTunnelId :: Text -> Text -> Text -> m (){- ^ Update the specified tunnel.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pUT40EdgesEdgeIdVnicsIndex :: Text -> Text -> Text -> m (){- ^ Update the specified interface. -}
  , pUT40EdgesEdgeIdVnicsParentVnicIndexSubinterfacesSubInterfaceIndex :: Text -> Text -> Text -> Text -> m (){- ^ Update the specified sub-interface.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method introduced.  -}
  , pUT40FirewallConfigGlobalconfiguration :: DfwPerformanceUpdate -> m (){- ^ Update the distributed firewall performance configuration.  **Method history:**  Release | Modification --------|------------- 6.2.3 | Method updated. **autoDraftDisabled** parameter added. 6.4.0 | Method updated. **tcpStrict** in the global configuration is ignored. Instead, configure **tcpStrict** at the section level.   -}
  , pUT40FirewallDomainIDEnableTruefalse :: Text -> Text -> m (){- ^ Enable or disable firewall components on a cluster -}
  , pUT40FirewallGlobalroot0Config :: Text -> Maybe Text -> m (){- ^ Update the complete firewall configuration in all sections.  * Retrieve the configuration with `GET /api/4.0/firewall/globalroot-0/config`. * Retrieve the Etag value from the response headers. * Extract and modify the configuration from the response body as needed. * Set the If-Match header to the Etag value, and submit the request.  Not all fields are required while sending the request. All the optional fields are safe to be ignored while sending the configuration to server. For example, if an IP set is referenced in the rule only IPSet and Type is needed in the Source/Destination objects and not Name and isValid tags.  When updating the firewall configuration: * IDs for new objects (rule/section) should be removed or set to zero. * If new entities (sections/rules) have been sent in the request, the response   will contain the system-generated IDs, which are assigned to these new   entities. * **appliedTo** can be any valid firewall rule element. * **action** can be *ALLOW*, *BLOCK*, or *REJECT*. REJECT sends reject message for   unaccepted packets; RST packets are sent for TCP connections and ICMP   unreachable code packets are sent for UDP, ICMP, and other IP connections * source and destination can have an exclude flag. For example, if you add an   exclude tag for 1.1.1.1 in the source parameter, the rule looks for traffic   originating from all IPs other than 1.1.1.1.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict**, **stateless**, and **useSid** added as **section** attributes.  -}
  , pUT40FirewallGlobalroot0ConfigIpfix :: DfwIPFixUpdate -> m (){- ^ Update IPFIX configuration.  **Method history:**  Release | Modification --------|------------- 6.3.5 | Default value for collector port changed from *0* to *4739*.  -}
  , pUT40FirewallGlobalroot0ConfigLayer2sectionsSectionId :: Text -> Text -> Maybe Text -> m (){- ^ Update the specified layer 2 section in distributed firewall.  * Retrieve the configuration for the specified section. * Retrieve the Etag value from the response headers. * Extract and modify the configuration from the response body as needed. * Set the If-Match header to the Etag value, and submit the request.  Not all fields are required while sending the request. All the optional fields are safe to be ignored while sending the configuration to server. For example, if an IP set is referenced in the rule only IPSet and Type is needed in the Source/Destination objects and not Name and isValid tags.  When updating the firewall configuration: * IDs for new objects (rule/section) should be removed or set to zero. * If new entities (sections/rules) have been sent in the request, the response   will contain the system-generated IDs, which are assigned to these new   entities. * **appliedTo** can be any valid firewall rule element. * **action** can be *ALLOW*, *BLOCK*, or *REJECT*. REJECT sends reject message for   unaccepted packets; RST packets are sent for TCP connections and ICMP   unreachable code packets are sent for UDP, ICMP, and other IP connections * source and destination can have an exclude flag. For example, if you add an   exclude tag for 1.1.1.1 in the source parameter, the rule looks for traffic   originating from all IPs other than 1.1.1.1.  When Distributed Firewall is used with Service Composer, firewall sections created by Service Composer contain an additional attribute in the XML called managedBy. You should not modify Service Composer firewall sections using Distributed Firewall REST APIs. If you do, you must synchronize firewall rules from Service Composer using the `GET /api/2.0/services/policy/serviceprovider/firewall` API.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict**, **stateless**, and **useSid** added as **section** attributes.   -}
  , pUT40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRulesRuleId :: Text -> Text -> Text -> Maybe Text -> m (){- ^ Update a distributed firewall rule in a layer 2 section.  * Retrieve the configuration for the section that contains the rule you want   to modify. * Retrieve the Etag value from the response headers.   **Note**: This is the Etag value of the firewall section to which you want   to add the rule. If you are keeping this rule in the same section, you must   keep the same Etag number. * Extract and modify the rule configuration from the response body as needed. * Set the If-Match header to the section Etag value, and submit the request.  Not all fields are required while sending the request. All the optional fields are safe to be ignored while sending the configuration to server. For example, if an IP set is referenced in the rule only IPSet and Type is needed in the Source/Destination objects and not Name and isValid tags.  -}
  , pUT40FirewallGlobalroot0ConfigLayer3redirectsectionsSection :: Text -> Layer3RedirectSectionUpdate -> Maybe Text -> m (){- ^ Modify layer 3 redirect section. You will need to get the Etag value out of the GET first. Then pass the modified version of the whole redirect section configuration in the GET body.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict**, **stateless**, and **useSid** added as **section** attributes.  -}
  , pUT40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRulesRuleID :: Text -> Text -> RuleUpdate -> Maybe Text -> m (){- ^ Modify L3 redirect rule. You will need Etag value from the response header of GET call. Then, pass Etag value as the if-match header in PUT call  -}
  , pUT40FirewallGlobalroot0ConfigLayer3sectionsSectionId :: Text -> Text -> Maybe Text -> m (){- ^ Update the specified layer 3 section in distributed firewall.  * Retrieve the configuration for the specified section. * Retrieve the Etag value from the response headers. * Extract and modify the configuration from the response body as needed. * Set the If-Match header to the Etag value, and submit the request.  Not all fields are required while sending the request. All the optional fields are safe to be ignored while sending the configuration to server. For example, if an IP set is referenced in the rule only IPSet and Type is needed in the Source/Destination objects and not Name and isValid tags.  When updating the firewall configuration: * IDs for new objects (rule/section) should be removed or set to zero. * If new entities (sections/rules) have been sent in the request, the response   will contain the system-generated IDs, which are assigned to these new   entities. * **appliedTo** can be any valid firewall rule element. * **action** can be *ALLOW*, *BLOCK*, or *REJECT*. REJECT sends reject message for   unaccepted packets; RST packets are sent for TCP connections and ICMP   unreachable code packets are sent for UDP, ICMP, and other IP connections * source and destination can have an exclude flag. For example, if you add an   exclude tag for 1.1.1.1 in the source parameter, the rule looks for traffic   originating from all IPs other than 1.1.1.1.  When Distributed Firewall is used with Service Composer, firewall sections created by Service Composer contain an additional attribute in the XML called managedBy. You should not modify Service Composer firewall sections using Distributed Firewall REST APIs. If you do, you must synchronize firewall rules from Service Composer using the `GET /api/2.0/services/policy/serviceprovider/firewall` API.  **Method history:**  Release | Modification --------|------------- 6.4.0 | Method updated. **tcpStrict**, **stateless**, and **useSid** added as **section** attributes.  -}
  , pUT40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRulesRuleId :: Text -> Text -> Text -> Maybe Text -> m (){- ^ Update a distributed firewall rule in a layer 3 section.  * Retrieve the configuration for the section that contains the rule you want   to modify. * Retrieve the Etag value from the response headers.   **Note**: This is the Etag value of the firewall section to which you want   to add the rule. If you are keeping this rule in the same section, you must   keep the same Etag number. * Extract and modify the rule configuration from the response body as needed. * Set the If-Match header to the section Etag value, and submit the request.  Not all fields are required while sending the request. All the optional fields are safe to be ignored while sending the configuration to server. For example, if an IP set is referenced in the rule only IPSet and Type is needed in the Source/Destination objects and not Name and isValid tags.  -}
  , pUT40FirewallGlobalroot0DraftsDraftID :: Text -> DfwDraftUpdate -> m (){- ^ Update a saved firewall configuration. -}
  , pUT40FirewallGlobalroot0State :: m (){- ^ Enable distributed firewall. -}
  , pUT40FirewallGlobalroot0TimeoutsConfigId :: Text -> Text -> m (){- ^ Update the specified Distributed Firewall session timer configuration.  **Method history:**  Release | Modification --------|------------- 6.3.0 | Method introduced.  -}
  , pUT40FirewallStatsEventthresholds :: DfwThresholdsUpdate -> m (){- ^ Update threshold configuration for distributed firewall.  **Note**: Starting in NSX 6.4, using this PUT API will disable the new threshold types such as process memory and concurrent connections. Instead, use the new API introduced in NSX 6.4 which is *PUT /api/4.0/firewall/stats/thresholds*.  -}
  , pUT40FirewallStatsThresholds :: Text -> m (){- ^ Configure threshold values for distributed firewall such as CPU utilization, heap memory, calls per second, concurrent connections, and process memory.  **Parameters**   Types of threshold | Default Value | Range | Unit -----|-----|-----|----- CPU | 90 | 0 - 100  | Percent Heap Memory | 90 | 0 - 100  | Percent Process Memory | 70 | 0 - 100  | Percent Connections per second | 40000 |   | Count Maximum connections | 500000 |   | Count  -}
  , pUT40ServicesSpoofguardPoliciesPolicyID :: Text -> SpoofGuardPolicyUpdate -> m (){- ^ Modify the specified SpoofGuard policy. -}
  }

newtype VMwareNSXForVSphereClient a = VMwareNSXForVSphereClient
  { runClient :: Manager -> BaseUrl -> ExceptT ServantError IO a
  } deriving Functor

instance Applicative VMwareNSXForVSphereClient where
  pure x = VMwareNSXForVSphereClient (\_ _ -> pure x)
  (VMwareNSXForVSphereClient f) <*> (VMwareNSXForVSphereClient x) =
    VMwareNSXForVSphereClient (\manager url -> f manager url <*> x manager url)

instance Monad VMwareNSXForVSphereClient where
  (VMwareNSXForVSphereClient a) >>= f =
    VMwareNSXForVSphereClient (\manager url -> do
      value <- a manager url
      runClient (f value) manager url)

instance MonadIO VMwareNSXForVSphereClient where
  liftIO io = VMwareNSXForVSphereClient (\_ _ -> liftIO io)

createVMwareNSXForVSphereClient :: VMwareNSXForVSphereBackend VMwareNSXForVSphereClient
createVMwareNSXForVSphereClient = VMwareNSXForVSphereBackend{..}
  where
    ((coerce -> dELETE10ApplianceManagementBackuprestoreBackupsettings) :<|>
     (coerce -> dELETE10ApplianceManagementBackuprestoreBackupsettingsSchedule) :<|>
     (coerce -> dELETE10ApplianceManagementNotifications) :<|>
     (coerce -> dELETE10ApplianceManagementSystemNetworkDns) :<|>
     (coerce -> dELETE10ApplianceManagementSystemSyslogserver) :<|>
     (coerce -> dELETE10ApplianceManagementSystemSyslogservers) :<|>
     (coerce -> dELETE10ApplianceManagementSystemTimesettingsNtp) :<|>
     (coerce -> dELETE10DirectoryDeleteDomainID) :<|>
     (coerce -> dELETE10DirectoryDeleteDomainRootDNDomainID) :<|>
     (coerce -> dELETE10DirectoryDeleteEventLogServerServerID) :<|>
     (coerce -> dELETE10DirectoryDeleteLdapServerServerID) :<|>
     (coerce -> dELETE10IdentityStaticUserMappingsbyIPIP) :<|>
     (coerce -> dELETE10IdentityStaticUserMappingsbyUserUserID) :<|>
     (coerce -> dELETE20EndpointsecurityActivationVendorIDAltitudeMoid) :<|>
     (coerce -> dELETE20EndpointsecurityRegistrationVendorID) :<|>
     (coerce -> dELETE20EndpointsecurityRegistrationVendorIDAltitude) :<|>
     (coerce -> dELETE20EndpointsecurityRegistrationVendorIDAltitudeLocation) :<|>
     (coerce -> dELETE20NwfabricClustersClusterID) :<|>
     (coerce -> dELETE20NwfabricConfigure) :<|>
     (coerce -> dELETE20NwfabricHostsHostID) :<|>
     (coerce -> dELETE20ServicesApplicationApplicationId) :<|>
     (coerce -> dELETE20ServicesApplicationgroupApplicationgroupId) :<|>
     (coerce -> dELETE20ServicesApplicationgroupApplicationgroupIdMembersMoref) :<|>
     (coerce -> dELETE20ServicesDashboardUiViewsDashboardWidgetconfigurationsWidgetconfigurationId) :<|>
     (coerce -> dELETE20ServicesIpamPoolsPoolId) :<|>
     (coerce -> dELETE20ServicesIpamPoolsPoolIdIpaddressesIpAddress) :<|>
     (coerce -> dELETE20ServicesIpsetIpsetId) :<|>
     (coerce -> dELETE20ServicesMacsetMacsetId) :<|>
     (coerce -> dELETE20ServicesPolicySecuritypolicyID) :<|>
     (coerce -> dELETE20ServicesSecuritygroupObjectId) :<|>
     (coerce -> dELETE20ServicesSecuritygroupObjectIdMembersMemberId) :<|>
     (coerce -> dELETE20ServicesSecuritytagsTagTagId) :<|>
     (coerce -> dELETE20ServicesSecuritytagsTagTagIdVmVmId) :<|>
     (coerce -> dELETE20ServicesSnmpManagerManagerId) :<|>
     (coerce -> dELETE20ServicesSsoconfig) :<|>
     (coerce -> dELETE20ServicesTruststoreConfigCertificateId) :<|>
     (coerce -> dELETE20ServicesTruststoreCrlCrlId) :<|>
     (coerce -> dELETE20ServicesUsermgmtRoleUserId) :<|>
     (coerce -> dELETE20ServicesUsermgmtUserUserId) :<|>
     (coerce -> dELETE20SiDeployClusterClusterID) :<|>
     (coerce -> dELETE20SiDeployServiceServiceID) :<|>
     (coerce -> dELETE20Techsupportbundle) :<|>
     (coerce -> dELETE20UniversalsyncConfigurationNsxmanagers) :<|>
     (coerce -> dELETE20UniversalsyncConfigurationNsxmanagersNsxManagerID) :<|>
     (coerce -> dELETE20VdnConfigMulticastsMulticastAddresssRangeId) :<|>
     (coerce -> dELETE20VdnConfigSegmentsSegmentPoolId) :<|>
     (coerce -> dELETE20VdnControllerControllerId) :<|>
     (coerce -> dELETE20VdnControllerControllerIdSyslog) :<|>
     (coerce -> dELETE20VdnHardwaregatewayBindingsBindingId) :<|>
     (coerce -> dELETE20VdnHardwaregatewaysHardwareGatewayId) :<|>
     (coerce -> dELETE20VdnScopesScopeId) :<|>
     (coerce -> dELETE20VdnSwitchesVdsId) :<|>
     (coerce -> dELETE20VdnVirtualwiresVirtualWireID) :<|>
     (coerce -> dELETE21AppExcludelistMemberID) :<|>
     (coerce -> dELETE21AppFlowContextId) :<|>
     (coerce -> dELETE40EdgesEdgeId) :<|>
     (coerce -> dELETE40EdgesEdgeIdAppliancesHaIndex) :<|>
     (coerce -> dELETE40EdgesEdgeIdBridgingConfig) :<|>
     (coerce -> dELETE40EdgesEdgeIdDhcpConfig) :<|>
     (coerce -> dELETE40EdgesEdgeIdDhcpConfigBindingsBindingID) :<|>
     (coerce -> dELETE40EdgesEdgeIdDhcpConfigIppoolsPoolID) :<|>
     (coerce -> dELETE40EdgesEdgeIdDhcpConfigRelay) :<|>
     (coerce -> dELETE40EdgesEdgeIdDnsConfig) :<|>
     (coerce -> dELETE40EdgesEdgeIdFirewallConfig) :<|>
     (coerce -> dELETE40EdgesEdgeIdFirewallConfigRulesRuleId) :<|>
     (coerce -> dELETE40EdgesEdgeIdHighavailabilityConfig) :<|>
     (coerce -> dELETE40EdgesEdgeIdInterfaces) :<|>
     (coerce -> dELETE40EdgesEdgeIdInterfacesIndex) :<|>
     (coerce -> dELETE40EdgesEdgeIdIpsecConfig) :<|>
     (coerce -> dELETE40EdgesEdgeIdL2vpnConfig) :<|>
     (coerce -> dELETE40EdgesEdgeIdLoadbalancerConfig) :<|>
     (coerce -> dELETE40EdgesEdgeIdLoadbalancerConfigApplicationprofiles) :<|>
     (coerce -> dELETE40EdgesEdgeIdLoadbalancerConfigApplicationprofilesAppProfileID) :<|>
     (coerce -> dELETE40EdgesEdgeIdLoadbalancerConfigApplicationrules) :<|>
     (coerce -> dELETE40EdgesEdgeIdLoadbalancerConfigApplicationrulesAppruleID) :<|>
     (coerce -> dELETE40EdgesEdgeIdLoadbalancerConfigMonitors) :<|>
     (coerce -> dELETE40EdgesEdgeIdLoadbalancerConfigMonitorsMonitorID) :<|>
     (coerce -> dELETE40EdgesEdgeIdLoadbalancerConfigPools) :<|>
     (coerce -> dELETE40EdgesEdgeIdLoadbalancerConfigPoolsPoolID) :<|>
     (coerce -> dELETE40EdgesEdgeIdLoadbalancerConfigVirtualservers) :<|>
     (coerce -> dELETE40EdgesEdgeIdLoadbalancerConfigVirtualserversVirtualserverID) :<|>
     (coerce -> dELETE40EdgesEdgeIdNatConfig) :<|>
     (coerce -> dELETE40EdgesEdgeIdNatConfigRulesRuleID) :<|>
     (coerce -> dELETE40EdgesEdgeIdRoutingConfig) :<|>
     (coerce -> dELETE40EdgesEdgeIdRoutingConfigBgp) :<|>
     (coerce -> dELETE40EdgesEdgeIdRoutingConfigOspf) :<|>
     (coerce -> dELETE40EdgesEdgeIdRoutingConfigStatic) :<|>
     (coerce -> dELETE40EdgesEdgeIdSslvpnActivesessionsSessionID) :<|>
     (coerce -> dELETE40EdgesEdgeIdSslvpnConfig) :<|>
     (coerce -> dELETE40EdgesEdgeIdSslvpnConfigAuthLocalserverUsers) :<|>
     (coerce -> dELETE40EdgesEdgeIdSslvpnConfigAuthLocalserverUsersUserID) :<|>
     (coerce -> dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages) :<|>
     (coerce -> dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackagesPackageID) :<|>
     (coerce -> dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools) :<|>
     (coerce -> dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppoolsIppoolID) :<|>
     (coerce -> dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks) :<|>
     (coerce -> dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworksNetworkID) :<|>
     (coerce -> dELETE40EdgesEdgeIdSslvpnConfigScript) :<|>
     (coerce -> dELETE40EdgesEdgeIdSslvpnConfigScriptFileID) :<|>
     (coerce -> dELETE40EdgesEdgeIdSyslogConfig) :<|>
     (coerce -> dELETE40EdgesEdgeIdSystemcontrolConfig) :<|>
     (coerce -> dELETE40EdgesEdgeIdTunnels) :<|>
     (coerce -> dELETE40EdgesEdgeIdTunnelsTunnelId) :<|>
     (coerce -> dELETE40EdgesEdgeIdVnicsIndex) :<|>
     (coerce -> dELETE40EdgesEdgeIdVnicsParentVnicIndexSubinterfacesSubInterfaceIndex) :<|>
     (coerce -> dELETE40FirewallConfigSections) :<|>
     (coerce -> dELETE40FirewallGlobalroot0Config) :<|>
     (coerce -> dELETE40FirewallGlobalroot0ConfigIpfix) :<|>
     (coerce -> dELETE40FirewallGlobalroot0ConfigLayer2sectionsSectionId) :<|>
     (coerce -> dELETE40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRulesRuleId) :<|>
     (coerce -> dELETE40FirewallGlobalroot0ConfigLayer3redirectsectionsSection) :<|>
     (coerce -> dELETE40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRulesRuleID) :<|>
     (coerce -> dELETE40FirewallGlobalroot0ConfigLayer3sectionsSectionId) :<|>
     (coerce -> dELETE40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRulesRuleId) :<|>
     (coerce -> dELETE40FirewallGlobalroot0DraftsDraftID) :<|>
     (coerce -> dELETE40FirewallGlobalroot0TimeoutsConfigId) :<|>
     (coerce -> dELETE40ServicesSpoofguardPoliciesPolicyID) :<|>
     (coerce -> gET10ApplianceManagementBackuprestoreBackups) :<|>
     (coerce -> gET10ApplianceManagementBackuprestoreBackupsettings) :<|>
     (coerce -> gET10ApplianceManagementCertificatemanagerCertificatesNsx) :<|>
     (coerce -> gET10ApplianceManagementCertificatemanagerCsrNsx) :<|>
     (coerce -> gET10ApplianceManagementComponents) :<|>
     (coerce -> gET10ApplianceManagementComponentsComponentComponentID) :<|>
     (coerce -> gET10ApplianceManagementComponentsComponentComponentIDDependencies) :<|>
     (coerce -> gET10ApplianceManagementComponentsComponentComponentIDDependents) :<|>
     (coerce -> gET10ApplianceManagementComponentsComponentComponentIDStatus) :<|>
     (coerce -> gET10ApplianceManagementGlobalInfo) :<|>
     (coerce -> gET10ApplianceManagementNotifications) :<|>
     (coerce -> gET10ApplianceManagementSummaryComponents) :<|>
     (coerce -> gET10ApplianceManagementSummarySystem) :<|>
     (coerce -> gET10ApplianceManagementSystemCpuinfo) :<|>
     (coerce -> gET10ApplianceManagementSystemCpuinfoDetails) :<|>
     (coerce -> gET10ApplianceManagementSystemLocale) :<|>
     (coerce -> gET10ApplianceManagementSystemMeminfo) :<|>
     (coerce -> gET10ApplianceManagementSystemNetwork) :<|>
     (coerce -> gET10ApplianceManagementSystemSecuritysettings) :<|>
     (coerce -> gET10ApplianceManagementSystemStorageinfo) :<|>
     (coerce -> gET10ApplianceManagementSystemSyslogserver) :<|>
     (coerce -> gET10ApplianceManagementSystemSyslogservers) :<|>
     (coerce -> gET10ApplianceManagementSystemTimesettings) :<|>
     (coerce -> gET10ApplianceManagementSystemTlssettings) :<|>
     (coerce -> gET10ApplianceManagementSystemUptime) :<|>
     (coerce -> gET10ApplianceManagementTechsupportlogsFilename) :<|>
     (coerce -> gET10ApplianceManagementUpgradeInformationComponentID) :<|>
     (coerce -> gET10ApplianceManagementUpgradeStatusComponentID) :<|>
     (coerce -> gET10DirectoryListDomains) :<|>
     (coerce -> gET10DirectoryListEventLogServersForDomainDomainID) :<|>
     (coerce -> gET10DirectoryListLdapServersForDomainDomainID) :<|>
     (coerce -> gET10EventcontrolConfigVmVmID) :<|>
     (coerce -> gET10IdentityDirectoryGroupsForUser) :<|>
     (coerce -> gET10IdentityHostIpMapping) :<|>
     (coerce -> gET10IdentityIpToUserMapping) :<|>
     (coerce -> gET10IdentityStaticUserMappings) :<|>
     (coerce -> gET10IdentityStaticUserMappingsbyIPIP) :<|>
     (coerce -> gET10IdentityStaticUserMappingsbyUserUserID) :<|>
     (coerce -> gET10IdentityUserIpMapping) :<|>
     (coerce -> gET10TelemetryConfig) :<|>
     (coerce -> gET10TelemetryProxy) :<|>
     (coerce -> gET20Auditlog) :<|>
     (coerce -> gET20CapacityParametersReport) :<|>
     (coerce -> gET20CapacityParametersThresholds) :<|>
     (coerce -> gET20EamStatus) :<|>
     (coerce -> gET20EndpointsecurityActivation) :<|>
     (coerce -> gET20EndpointsecurityActivationVendorIDAltitudeMoid) :<|>
     (coerce -> gET20EndpointsecurityActivationVendorIDSolutionID) :<|>
     (coerce -> gET20EndpointsecurityRegistrationVendorID) :<|>
     (coerce -> gET20EndpointsecurityRegistrationVendorIDAltitude) :<|>
     (coerce -> gET20EndpointsecurityRegistrationVendorIDAltitudeLocation) :<|>
     (coerce -> gET20EndpointsecurityRegistrationVendorIDSolutions) :<|>
     (coerce -> gET20EndpointsecurityRegistrationVendors) :<|>
     (coerce -> gET20EndpointsecurityUsvmstatsUsvmhealththresholds) :<|>
     (coerce -> gET20Hostevents) :<|>
     (coerce -> gET20NwfabricClustersClusterID) :<|>
     (coerce -> gET20NwfabricFeatures) :<|>
     (coerce -> gET20NwfabricHostsHostID) :<|>
     (coerce -> gET20NwfabricStatus) :<|>
     (coerce -> gET20NwfabricStatusAlleligibleResourceType) :<|>
     (coerce -> gET20NwfabricStatusChildParentResourceID) :<|>
     (coerce -> gET20ServicesAlarmsSourceId) :<|>
     (coerce -> gET20ServicesApplicationApplicationId) :<|>
     (coerce -> gET20ServicesApplicationScopeScopeId) :<|>
     (coerce -> gET20ServicesApplicationgroupApplicationgroupId) :<|>
     (coerce -> gET20ServicesApplicationgroupScopeScopeId) :<|>
     (coerce -> gET20ServicesApplicationgroupScopeScopeIdMembers) :<|>
     (coerce -> gET20ServicesAuthTokenexpiration) :<|>
     (coerce -> gET20ServicesConfiguration) :<|>
     (coerce -> gET20ServicesDashboardUiViewsDashboardWidgetconfigurations) :<|>
     (coerce -> gET20ServicesDashboardUiViewsDashboardWidgetconfigurationsWidgetconfigurationId) :<|>
     (coerce -> gET20ServicesHousekeepingManagementIndexMaintenance) :<|>
     (coerce -> gET20ServicesIpamPoolsPoolId) :<|>
     (coerce -> gET20ServicesIpamPoolsPoolIdIpaddresses) :<|>
     (coerce -> gET20ServicesIpamPoolsScopeScopeId) :<|>
     (coerce -> gET20ServicesIpsetIpsetId) :<|>
     (coerce -> gET20ServicesIpsetScopeScopeMoref) :<|>
     (coerce -> gET20ServicesLicensingCapacityusage) :<|>
     (coerce -> gET20ServicesLicensingStatus) :<|>
     (coerce -> gET20ServicesMacsetMacsetId) :<|>
     (coerce -> gET20ServicesMacsetScopeScopeId) :<|>
     (coerce -> gET20ServicesPolicySecurityactionCategoryVirtualmachines) :<|>
     (coerce -> gET20ServicesPolicySecuritygroupIDSecurityactions) :<|>
     (coerce -> gET20ServicesPolicySecuritygroupIDSecuritypolicies) :<|>
     (coerce -> gET20ServicesPolicySecuritypolicyAlarmsAll) :<|>
     (coerce -> gET20ServicesPolicySecuritypolicyAll) :<|>
     (coerce -> gET20ServicesPolicySecuritypolicyHierarchy) :<|>
     (coerce -> gET20ServicesPolicySecuritypolicyID) :<|>
     (coerce -> gET20ServicesPolicySecuritypolicyIDSecurityactions) :<|>
     (coerce -> gET20ServicesPolicySecuritypolicyMaxprecedence) :<|>
     (coerce -> gET20ServicesPolicySecuritypolicyServiceproviderFirewall) :<|>
     (coerce -> gET20ServicesPolicySecuritypolicyStatus) :<|>
     (coerce -> gET20ServicesPolicyServiceproviderFirewall) :<|>
     (coerce -> gET20ServicesPolicyServiceproviderFirewallInfo) :<|>
     (coerce -> gET20ServicesPolicyVirtualmachineIDSecurityactions) :<|>
     (coerce -> gET20ServicesSecuritygroupInternalScopeScopeId) :<|>
     (coerce -> gET20ServicesSecuritygroupLookupIpaddressIpAddress) :<|>
     (coerce -> gET20ServicesSecuritygroupLookupVirtualmachineVirtualMachineId) :<|>
     (coerce -> gET20ServicesSecuritygroupObjectId) :<|>
     (coerce -> gET20ServicesSecuritygroupObjectIdTranslationIpaddresses) :<|>
     (coerce -> gET20ServicesSecuritygroupObjectIdTranslationMacaddresses) :<|>
     (coerce -> gET20ServicesSecuritygroupObjectIdTranslationVirtualmachines) :<|>
     (coerce -> gET20ServicesSecuritygroupObjectIdTranslationVnics) :<|>
     (coerce -> gET20ServicesSecuritygroupScopeScopeId) :<|>
     (coerce -> gET20ServicesSecuritygroupScopeScopeIdMemberTypes) :<|>
     (coerce -> gET20ServicesSecuritygroupScopeScopeIdMembersMemberType) :<|>
     (coerce -> gET20ServicesSecuritytagsSelectionCriteria) :<|>
     (coerce -> gET20ServicesSecuritytagsTag) :<|>
     (coerce -> gET20ServicesSecuritytagsTagTagIdVm) :<|>
     (coerce -> gET20ServicesSecuritytagsTagTagIdVmDetail) :<|>
     (coerce -> gET20ServicesSecuritytagsVmVmId) :<|>
     (coerce -> gET20ServicesSnmpManager) :<|>
     (coerce -> gET20ServicesSnmpManagerManagerId) :<|>
     (coerce -> gET20ServicesSnmpStatus) :<|>
     (coerce -> gET20ServicesSnmpTrap) :<|>
     (coerce -> gET20ServicesSnmpTrapOid) :<|>
     (coerce -> gET20ServicesSsoconfig) :<|>
     (coerce -> gET20ServicesSsoconfigStatus) :<|>
     (coerce -> gET20ServicesSystemalarms) :<|>
     (coerce -> gET20ServicesSystemalarmsAlarmId) :<|>
     (coerce -> gET20ServicesTaskserviceJob) :<|>
     (coerce -> gET20ServicesTaskserviceJobJobId) :<|>
     (coerce -> gET20ServicesTranslationVirtualmachineVmIdIpaddresses) :<|>
     (coerce -> gET20ServicesTruststoreConfig) :<|>
     (coerce -> gET20ServicesTruststoreConfigCertificateId) :<|>
     (coerce -> gET20ServicesTruststoreConfigScopeScopeId) :<|>
     (coerce -> gET20ServicesTruststoreCrlCrlId) :<|>
     (coerce -> gET20ServicesTruststoreCrlScopeScopeId) :<|>
     (coerce -> gET20ServicesTruststoreCsrCsrId) :<|>
     (coerce -> gET20ServicesTruststoreCsrScopeScopeId) :<|>
     (coerce -> gET20ServicesUsermgmtRoleUserId) :<|>
     (coerce -> gET20ServicesUsermgmtRoles) :<|>
     (coerce -> gET20ServicesUsermgmtScopingobjects) :<|>
     (coerce -> gET20ServicesUsermgmtUserUserId) :<|>
     (coerce -> gET20ServicesUsermgmtUsersVsm) :<|>
     (coerce -> gET20ServicesVcconfig) :<|>
     (coerce -> gET20ServicesVcconfigStatus) :<|>
     (coerce -> gET20SiAgentAgentID) :<|>
     (coerce -> gET20SiDeployClusterClusterID) :<|>
     (coerce -> gET20SiDeployClusterClusterIDServiceServiceID) :<|>
     (coerce -> gET20SiDeployServiceServiceID) :<|>
     (coerce -> gET20SiDeployServiceServiceIDDependsOn) :<|>
     (coerce -> gET20SiDeploymentDeploymentunitIDAgents) :<|>
     (coerce -> gET20SiFabricSyncConflicts) :<|>
     (coerce -> gET20SiHostHostIDAgents) :<|>
     (coerce -> gET20SystemMonitorCpuusageDetails) :<|>
     (coerce -> gET20SystemMonitorCpuusageIndicator) :<|>
     (coerce -> gET20Systemevent) :<|>
     (coerce -> gET20TechsupportbundleFilename) :<|>
     (coerce -> gET20TechsupportbundleStatus) :<|>
     (coerce -> gET20UniversalsyncConfigurationNsxmanagers) :<|>
     (coerce -> gET20UniversalsyncConfigurationNsxmanagersNsxManagerID) :<|>
     (coerce -> gET20UniversalsyncConfigurationRole) :<|>
     (coerce -> gET20UniversalsyncEntitystatus) :<|>
     (coerce -> gET20UniversalsyncStatus) :<|>
     (coerce -> gET20VdnBfdConfigurationGlobal) :<|>
     (coerce -> gET20VdnCdo) :<|>
     (coerce -> gET20VdnConfigMulticasts) :<|>
     (coerce -> gET20VdnConfigMulticastsMulticastAddresssRangeId) :<|>
     (coerce -> gET20VdnConfigResourcesAllocated) :<|>
     (coerce -> gET20VdnConfigSegments) :<|>
     (coerce -> gET20VdnConfigSegmentsSegmentPoolId) :<|>
     (coerce -> gET20VdnConfigVxlanUdpPort) :<|>
     (coerce -> gET20VdnConfigVxlanUdpPortTaskStatus) :<|>
     (coerce -> gET20VdnController) :<|>
     (coerce -> gET20VdnControllerCluster) :<|>
     (coerce -> gET20VdnControllerClusterNtp) :<|>
     (coerce -> gET20VdnControllerControllerIdSnapshot) :<|>
     (coerce -> gET20VdnControllerControllerIdSyslog) :<|>
     (coerce -> gET20VdnControllerControllerIdSystemStats) :<|>
     (coerce -> gET20VdnControllerControllerIdTechsupportlogs) :<|>
     (coerce -> gET20VdnControllerProgressJobId) :<|>
     (coerce -> gET20VdnControllerSynchronizeStatus) :<|>
     (coerce -> gET20VdnControllerUpgradeAvailable) :<|>
     (coerce -> gET20VdnHardwaregatewayBfdConfig) :<|>
     (coerce -> gET20VdnHardwaregatewayBfdStatus) :<|>
     (coerce -> gET20VdnHardwaregatewayBindings) :<|>
     (coerce -> gET20VdnHardwaregatewayBindingsBindingId) :<|>
     (coerce -> gET20VdnHardwaregatewayBindingsBindingIdStatistic) :<|>
     (coerce -> gET20VdnHardwaregateways) :<|>
     (coerce -> gET20VdnHardwaregatewaysHardwareGatewayId) :<|>
     (coerce -> gET20VdnHardwaregatewaysHardwareGatewayIdSwitches) :<|>
     (coerce -> gET20VdnHardwaregatewaysHardwareGatewayIdSwitchesSwitchNameSwitchports) :<|>
     (coerce -> gET20VdnHardwaregatewaysReplicationcluster) :<|>
     (coerce -> gET20VdnHostHostIdRemoteHostStatus) :<|>
     (coerce -> gET20VdnHostHostIdStatus) :<|>
     (coerce -> gET20VdnHostHostIdTunnel) :<|>
     (coerce -> gET20VdnHostStatus) :<|>
     (coerce -> gET20VdnInventoryHostHostIdConnectionStatus) :<|>
     (coerce -> gET20VdnInventoryHostsConnectionStatus) :<|>
     (coerce -> gET20VdnPnicCheckConfigurationGlobal) :<|>
     (coerce -> gET20VdnScopes) :<|>
     (coerce -> gET20VdnScopesScopeId) :<|>
     (coerce -> gET20VdnScopesScopeIdVirtualwires) :<|>
     (coerce -> gET20VdnSwitches) :<|>
     (coerce -> gET20VdnSwitchesDatacenterDatacenterID) :<|>
     (coerce -> gET20VdnSwitchesVdsId) :<|>
     (coerce -> gET20VdnTraceflowTraceflowId) :<|>
     (coerce -> gET20VdnTraceflowTraceflowIdObservations) :<|>
     (coerce -> gET20VdnVirtualwires) :<|>
     (coerce -> gET20VdnVirtualwiresVirtualWireID) :<|>
     (coerce -> gET20VdnVirtualwiresVirtualWireIDHardwaregateways) :<|>
     (coerce -> gET20XvsNetworksIDFeatures) :<|>
     (coerce -> gET21AppExcludelist) :<|>
     (coerce -> gET21AppFlowConfig) :<|>
     (coerce -> gET21AppFlowFlowstats) :<|>
     (coerce -> gET21AppFlowFlowstatsInfo) :<|>
     (coerce -> gET30AiApp) :<|>
     (coerce -> gET30AiAppAppID) :<|>
     (coerce -> gET30AiDesktoppool) :<|>
     (coerce -> gET30AiDesktoppoolDesktoppoolID) :<|>
     (coerce -> gET30AiDirectorygroup) :<|>
     (coerce -> gET30AiDirectorygroupDirectorygroupID) :<|>
     (coerce -> gET30AiDirectorygroupUserUserID) :<|>
     (coerce -> gET30AiHost) :<|>
     (coerce -> gET30AiHostHostID) :<|>
     (coerce -> gET30AiRecords) :<|>
     (coerce -> gET30AiSecuritygroup) :<|>
     (coerce -> gET30AiSecuritygroupSecgroupID) :<|>
     (coerce -> gET30AiUserUserID) :<|>
     (coerce -> gET30AiUserdetails) :<|>
     (coerce -> gET30AiVm) :<|>
     (coerce -> gET30AiVmVmID) :<|>
     (coerce -> gET40EdgePublishTuningConfiguration) :<|>
     (coerce -> gET40Edges) :<|>
     (coerce -> gET40EdgesEdgeId) :<|>
     (coerce -> gET40EdgesEdgeIdAppliances) :<|>
     (coerce -> gET40EdgesEdgeIdAppliancesHaIndex) :<|>
     (coerce -> gET40EdgesEdgeIdAutoconfiguration) :<|>
     (coerce -> gET40EdgesEdgeIdBridgingConfig) :<|>
     (coerce -> gET40EdgesEdgeIdDhcpConfig) :<|>
     (coerce -> gET40EdgesEdgeIdDhcpConfigBindings) :<|>
     (coerce -> gET40EdgesEdgeIdDhcpConfigBindingsBindingID) :<|>
     (coerce -> gET40EdgesEdgeIdDhcpConfigRelay) :<|>
     (coerce -> gET40EdgesEdgeIdDhcpLeaseInfo) :<|>
     (coerce -> gET40EdgesEdgeIdDnsConfig) :<|>
     (coerce -> gET40EdgesEdgeIdDnsStatistics) :<|>
     (coerce -> gET40EdgesEdgeIdFirewallConfig) :<|>
     (coerce -> gET40EdgesEdgeIdFirewallConfigDefaultpolicy) :<|>
     (coerce -> gET40EdgesEdgeIdFirewallConfigGlobal) :<|>
     (coerce -> gET40EdgesEdgeIdFirewallConfigRulesRuleId) :<|>
     (coerce -> gET40EdgesEdgeIdFirewallStatisticsRuleId) :<|>
     (coerce -> gET40EdgesEdgeIdHealthsummary) :<|>
     (coerce -> gET40EdgesEdgeIdHighavailabilityConfig) :<|>
     (coerce -> gET40EdgesEdgeIdInterfaces) :<|>
     (coerce -> gET40EdgesEdgeIdInterfacesIndex) :<|>
     (coerce -> gET40EdgesEdgeIdIpsecConfig) :<|>
     (coerce -> gET40EdgesEdgeIdIpsecStatistics) :<|>
     (coerce -> gET40EdgesEdgeIdL2vpnConfig) :<|>
     (coerce -> gET40EdgesEdgeIdL2vpnConfigStatistics) :<|>
     (coerce -> gET40EdgesEdgeIdLoadbalancerConfig) :<|>
     (coerce -> gET40EdgesEdgeIdLoadbalancerConfigApplicationprofiles) :<|>
     (coerce -> gET40EdgesEdgeIdLoadbalancerConfigApplicationprofilesAppProfileID) :<|>
     (coerce -> gET40EdgesEdgeIdLoadbalancerConfigApplicationrules) :<|>
     (coerce -> gET40EdgesEdgeIdLoadbalancerConfigApplicationrulesAppruleID) :<|>
     (coerce -> gET40EdgesEdgeIdLoadbalancerConfigMonitors) :<|>
     (coerce -> gET40EdgesEdgeIdLoadbalancerConfigMonitorsMonitorID) :<|>
     (coerce -> gET40EdgesEdgeIdLoadbalancerConfigPools) :<|>
     (coerce -> gET40EdgesEdgeIdLoadbalancerConfigPoolsPoolID) :<|>
     (coerce -> gET40EdgesEdgeIdLoadbalancerConfigVirtualservers) :<|>
     (coerce -> gET40EdgesEdgeIdLoadbalancerConfigVirtualserversVirtualserverID) :<|>
     (coerce -> gET40EdgesEdgeIdLoadbalancerStatistics) :<|>
     (coerce -> gET40EdgesEdgeIdMgmtinterface) :<|>
     (coerce -> gET40EdgesEdgeIdNatConfig) :<|>
     (coerce -> gET40EdgesEdgeIdRoutingConfig) :<|>
     (coerce -> gET40EdgesEdgeIdRoutingConfigBgp) :<|>
     (coerce -> gET40EdgesEdgeIdRoutingConfigGlobal) :<|>
     (coerce -> gET40EdgesEdgeIdRoutingConfigOspf) :<|>
     (coerce -> gET40EdgesEdgeIdRoutingConfigStatic) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnActivesessions) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfig) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigAdvancedconfig) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigAuthLocalserverUsersUserID) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigAuthSettings) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionClientconfig) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackagesPackageID) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppoolsIppoolID) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworksNetworkID) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigLayout) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigScript) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigScriptFileID) :<|>
     (coerce -> gET40EdgesEdgeIdSslvpnConfigServer) :<|>
     (coerce -> gET40EdgesEdgeIdStatisticsDashboardFirewall) :<|>
     (coerce -> gET40EdgesEdgeIdStatisticsDashboardInterface) :<|>
     (coerce -> gET40EdgesEdgeIdStatisticsDashboardIpsec) :<|>
     (coerce -> gET40EdgesEdgeIdStatisticsDashboardSslvpn) :<|>
     (coerce -> gET40EdgesEdgeIdStatisticsInterfaces) :<|>
     (coerce -> gET40EdgesEdgeIdStatisticsInterfacesInternal) :<|>
     (coerce -> gET40EdgesEdgeIdStatisticsInterfacesUplink) :<|>
     (coerce -> gET40EdgesEdgeIdStatus) :<|>
     (coerce -> gET40EdgesEdgeIdSummary) :<|>
     (coerce -> gET40EdgesEdgeIdSyslogConfig) :<|>
     (coerce -> gET40EdgesEdgeIdSystemcontrolConfig) :<|>
     (coerce -> gET40EdgesEdgeIdTechsupportlogs) :<|>
     (coerce -> gET40EdgesEdgeIdTunnels) :<|>
     (coerce -> gET40EdgesEdgeIdTunnelsTunnelId) :<|>
     (coerce -> gET40EdgesEdgeIdVnics) :<|>
     (coerce -> gET40EdgesEdgeIdVnicsIndex) :<|>
     (coerce -> gET40EdgesEdgeIdVnicsParentVnicIndexSubinterfacesSubInterfaceIndex) :<|>
     (coerce -> gET40EdgesJobs) :<|>
     (coerce -> gET40EdgesJobsJobId) :<|>
     (coerce -> gET40FirewallConfigGlobalconfiguration) :<|>
     (coerce -> gET40FirewallGlobalroot0Config) :<|>
     (coerce -> gET40FirewallGlobalroot0ConfigIpfix) :<|>
     (coerce -> gET40FirewallGlobalroot0ConfigLayer2sections) :<|>
     (coerce -> gET40FirewallGlobalroot0ConfigLayer2sectionsSectionId) :<|>
     (coerce -> gET40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRulesRuleId) :<|>
     (coerce -> gET40FirewallGlobalroot0ConfigLayer3redirectProfiles) :<|>
     (coerce -> gET40FirewallGlobalroot0ConfigLayer3redirectsectionsSection) :<|>
     (coerce -> gET40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRulesRuleID) :<|>
     (coerce -> gET40FirewallGlobalroot0ConfigLayer3sections) :<|>
     (coerce -> gET40FirewallGlobalroot0ConfigLayer3sectionsSectionId) :<|>
     (coerce -> gET40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRulesRuleId) :<|>
     (coerce -> gET40FirewallGlobalroot0Defaultconfig) :<|>
     (coerce -> gET40FirewallGlobalroot0Drafts) :<|>
     (coerce -> gET40FirewallGlobalroot0DraftsDraftID) :<|>
     (coerce -> gET40FirewallGlobalroot0DraftsDraftIDActionExport) :<|>
     (coerce -> gET40FirewallGlobalroot0State) :<|>
     (coerce -> gET40FirewallGlobalroot0Status) :<|>
     (coerce -> gET40FirewallGlobalroot0StatusLayer2sectionsSectionID) :<|>
     (coerce -> gET40FirewallGlobalroot0StatusLayer3sectionsSectionID) :<|>
     (coerce -> gET40FirewallGlobalroot0Timeouts) :<|>
     (coerce -> gET40FirewallGlobalroot0TimeoutsConfigId) :<|>
     (coerce -> gET40FirewallStatsEventthresholds) :<|>
     (coerce -> gET40FirewallStatsThresholdsHostHostId) :<|>
     (coerce -> gET40FirewallStatsThresholdsTypes) :<|>
     (coerce -> gET40ServicesSpoofguardPolicies) :<|>
     (coerce -> gET40ServicesSpoofguardPoliciesPolicyID) :<|>
     (coerce -> gET40ServicesSpoofguardPolicyID) :<|>
     (coerce -> pOST10ApplianceManagementBackuprestoreBackup) :<|>
     (coerce -> pOST10ApplianceManagementBackuprestoreRestore) :<|>
     (coerce -> pOST10ApplianceManagementCertificatemanagerCsrNsx) :<|>
     (coerce -> pOST10ApplianceManagementCertificatemanagerPkcs12keystoreNsx) :<|>
     (coerce -> pOST10ApplianceManagementCertificatemanagerUploadchainNsx) :<|>
     (coerce -> pOST10ApplianceManagementComponentsComponentAPPMGMTRestart) :<|>
     (coerce -> pOST10ApplianceManagementComponentsComponentComponentIDToggleStatusCommand) :<|>
     (coerce -> pOST10ApplianceManagementNotificationsIDAcknowledge) :<|>
     (coerce -> pOST10ApplianceManagementSystemRestart) :<|>
     (coerce -> pOST10ApplianceManagementSystemSecuritysettings) :<|>
     (coerce -> pOST10ApplianceManagementSystemTlssettings) :<|>
     (coerce -> pOST10ApplianceManagementTechsupportlogsComponentID) :<|>
     (coerce -> pOST10ApplianceManagementUpgradeStartComponentID) :<|>
     (coerce -> pOST10ApplianceManagementUpgradeUploadbundleComponentID) :<|>
     (coerce -> pOST10ApplianceManagementUpgradeUploadbundlefromurl) :<|>
     (coerce -> pOST10DirectoryLdapSyncSettings) :<|>
     (coerce -> pOST10DirectoryUpdateDomain) :<|>
     (coerce -> pOST10DirectoryUpdateEventLogServer) :<|>
     (coerce -> pOST10DirectoryUpdateLdapServer) :<|>
     (coerce -> pOST10DirectoryVerifyRootDn) :<|>
     (coerce -> pOST10EventcontrolEventcontrolRootRequest) :<|>
     (coerce -> pOST10EventcontrolVmVmIDRequest) :<|>
     (coerce -> pOST10IdentityStaticUserMappingUserIDIP) :<|>
     (coerce -> pOST10NsxCli) :<|>
     (coerce -> pOST10SamSyslogDisable) :<|>
     (coerce -> pOST10SamSyslogEnable) :<|>
     (coerce -> pOST20EndpointsecurityActivationVendorIDAltitude) :<|>
     (coerce -> pOST20EndpointsecurityRegistration) :<|>
     (coerce -> pOST20EndpointsecurityRegistrationVendorID) :<|>
     (coerce -> pOST20EndpointsecurityRegistrationVendorIDAltitudeLocation) :<|>
     (coerce -> pOST20Hostevents) :<|>
     (coerce -> pOST20NwfabricConfigure) :<|>
     (coerce -> pOST20ServicesAlarmsSourceId) :<|>
     (coerce -> pOST20ServicesApplicationScopeId) :<|>
     (coerce -> pOST20ServicesApplicationgroupScopeId) :<|>
     (coerce -> pOST20ServicesAuthToken) :<|>
     (coerce -> pOST20ServicesAuthTokeninvalidation) :<|>
     (coerce -> pOST20ServicesDashboardUiViewsDashboardWidgetconfigurations) :<|>
     (coerce -> pOST20ServicesHousekeepingManagementIndexMaintenance) :<|>
     (coerce -> pOST20ServicesIpamPoolsPoolIdIpaddresses) :<|>
     (coerce -> pOST20ServicesIpamPoolsScopeScopeId) :<|>
     (coerce -> pOST20ServicesIpsetScopeMoref) :<|>
     (coerce -> pOST20ServicesMacsetScopeScopeId) :<|>
     (coerce -> pOST20ServicesPolicySecuritypolicy) :<|>
     (coerce -> pOST20ServicesPolicySecuritypolicyHierarchy) :<|>
     (coerce -> pOST20ServicesSecuritygroupBulkScopeId) :<|>
     (coerce -> pOST20ServicesSecuritygroupScopeId) :<|>
     (coerce -> pOST20ServicesSecuritytagsTag) :<|>
     (coerce -> pOST20ServicesSecuritytagsTagTagIdVm) :<|>
     (coerce -> pOST20ServicesSecuritytagsVmVmId) :<|>
     (coerce -> pOST20ServicesSnmpManager) :<|>
     (coerce -> pOST20ServicesSsoconfig) :<|>
     (coerce -> pOST20ServicesSystemalarmsAlarmId) :<|>
     (coerce -> pOST20ServicesTruststoreCertificate) :<|>
     (coerce -> pOST20ServicesTruststoreConfigScopeId) :<|>
     (coerce -> pOST20ServicesTruststoreCrlScopeId) :<|>
     (coerce -> pOST20ServicesTruststoreCsrScopeId) :<|>
     (coerce -> pOST20ServicesUsermgmtRoleUserId) :<|>
     (coerce -> pOST20ServicesVcconfigConnectionstatus) :<|>
     (coerce -> pOST20SiDeploy) :<|>
     (coerce -> pOST20Techsupportbundle) :<|>
     (coerce -> pOST20UniversalsyncConfigurationNsxmanagers) :<|>
     (coerce -> pOST20UniversalsyncConfigurationRole) :<|>
     (coerce -> pOST20UniversalsyncSync) :<|>
     (coerce -> pOST20VdnCdo) :<|>
     (coerce -> pOST20VdnConfigHostHostIdVxlanVteps) :<|>
     (coerce -> pOST20VdnConfigMulticasts) :<|>
     (coerce -> pOST20VdnConfigSegments) :<|>
     (coerce -> pOST20VdnConfigVxlanUdpPortResume) :<|>
     (coerce -> pOST20VdnController) :<|>
     (coerce -> pOST20VdnControllerControllerId) :<|>
     (coerce -> pOST20VdnControllerControllerIdSyslog) :<|>
     (coerce -> pOST20VdnHardwaregatewayBindings) :<|>
     (coerce -> pOST20VdnHardwaregatewayBindingsManage) :<|>
     (coerce -> pOST20VdnHardwaregateways) :<|>
     (coerce -> pOST20VdnScopes) :<|>
     (coerce -> pOST20VdnScopesScopeId) :<|>
     (coerce -> pOST20VdnScopesScopeIdCdo) :<|>
     (coerce -> pOST20VdnScopesScopeIdConnCheckMulticast) :<|>
     (coerce -> pOST20VdnScopesScopeIdVirtualwires) :<|>
     (coerce -> pOST20VdnSwitches) :<|>
     (coerce -> pOST20VdnTraceflow) :<|>
     (coerce -> pOST20VdnVirtualwiresVirtualWireIDBacking) :<|>
     (coerce -> pOST20VdnVirtualwiresVirtualWireIDConnCheckMulticast) :<|>
     (coerce -> pOST20VdnVirtualwiresVirtualWireIDConnCheckP2p) :<|>
     (coerce -> pOST20VdnVirtualwiresVirtualWireIDHardwaregatewaysHardwareGatewayBindingId) :<|>
     (coerce -> pOST20VdnVirtualwiresVmVnic) :<|>
     (coerce -> pOST40Edges) :<|>
     (coerce -> pOST40EdgesEdgeId) :<|>
     (coerce -> pOST40EdgesEdgeIdAesni) :<|>
     (coerce -> pOST40EdgesEdgeIdAppliances) :<|>
     (coerce -> pOST40EdgesEdgeIdAppliancesHaIndex) :<|>
     (coerce -> pOST40EdgesEdgeIdCliremoteaccess) :<|>
     (coerce -> pOST40EdgesEdgeIdCoredump) :<|>
     (coerce -> pOST40EdgesEdgeIdDhcpConfigBindings) :<|>
     (coerce -> pOST40EdgesEdgeIdDhcpConfigIppools) :<|>
     (coerce -> pOST40EdgesEdgeIdFips) :<|>
     (coerce -> pOST40EdgesEdgeIdFirewallConfigRules) :<|>
     (coerce -> pOST40EdgesEdgeIdInterfaces) :<|>
     (coerce -> pOST40EdgesEdgeIdL2vpnConfig) :<|>
     (coerce -> pOST40EdgesEdgeIdLoadbalancerAcceleration) :<|>
     (coerce -> pOST40EdgesEdgeIdLoadbalancerConfigApplicationprofiles) :<|>
     (coerce -> pOST40EdgesEdgeIdLoadbalancerConfigApplicationrules) :<|>
     (coerce -> pOST40EdgesEdgeIdLoadbalancerConfigMembersMemberID) :<|>
     (coerce -> pOST40EdgesEdgeIdLoadbalancerConfigMonitors) :<|>
     (coerce -> pOST40EdgesEdgeIdLoadbalancerConfigPools) :<|>
     (coerce -> pOST40EdgesEdgeIdLoadbalancerConfigVirtualservers) :<|>
     (coerce -> pOST40EdgesEdgeIdLogging) :<|>
     (coerce -> pOST40EdgesEdgeIdNatConfigRules) :<|>
     (coerce -> pOST40EdgesEdgeIdParentVnicIndexSubinterfaces) :<|>
     (coerce -> pOST40EdgesEdgeIdSslvpnConfig) :<|>
     (coerce -> pOST40EdgesEdgeIdSslvpnConfigAuthLocalserverUsers) :<|>
     (coerce -> pOST40EdgesEdgeIdSslvpnConfigAuthSettingsRsaconfigfile) :<|>
     (coerce -> pOST40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages) :<|>
     (coerce -> pOST40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools) :<|>
     (coerce -> pOST40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks) :<|>
     (coerce -> pOST40EdgesEdgeIdSslvpnConfigLayoutImagesImageType) :<|>
     (coerce -> pOST40EdgesEdgeIdSslvpnConfigScript) :<|>
     (coerce -> pOST40EdgesEdgeIdSslvpnConfigScriptFile) :<|>
     (coerce -> pOST40EdgesEdgeIdTunnels) :<|>
     (coerce -> pOST40EdgesEdgeIdVnics) :<|>
     (coerce -> pOST40FirewallForceSyncID) :<|>
     (coerce -> pOST40FirewallGlobalroot0ConfigLayer2sections) :<|>
     (coerce -> pOST40FirewallGlobalroot0ConfigLayer2sectionsSectionId) :<|>
     (coerce -> pOST40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRules) :<|>
     (coerce -> pOST40FirewallGlobalroot0ConfigLayer3redirectsections) :<|>
     (coerce -> pOST40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRules) :<|>
     (coerce -> pOST40FirewallGlobalroot0ConfigLayer3sections) :<|>
     (coerce -> pOST40FirewallGlobalroot0ConfigLayer3sectionsSectionId) :<|>
     (coerce -> pOST40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRules) :<|>
     (coerce -> pOST40FirewallGlobalroot0Drafts) :<|>
     (coerce -> pOST40FirewallGlobalroot0DraftsActionImport) :<|>
     (coerce -> pOST40FirewallGlobalroot0Timeouts) :<|>
     (coerce -> pOST40FirewallObjectsStatusVmVmIDContainers) :<|>
     (coerce -> pOST40ServicesSpoofguardPolicies) :<|>
     (coerce -> pOST40ServicesSpoofguardPolicyID) :<|>
     (coerce -> pUT10ApplianceManagementBackuprestoreBackupsettings) :<|>
     (coerce -> pUT10ApplianceManagementBackuprestoreBackupsettingsExcludedata) :<|>
     (coerce -> pUT10ApplianceManagementBackuprestoreBackupsettingsFtpsettings) :<|>
     (coerce -> pUT10ApplianceManagementBackuprestoreBackupsettingsSchedule) :<|>
     (coerce -> pUT10ApplianceManagementSystemLocale) :<|>
     (coerce -> pUT10ApplianceManagementSystemNetwork) :<|>
     (coerce -> pUT10ApplianceManagementSystemNetworkDns) :<|>
     (coerce -> pUT10ApplianceManagementSystemSyslogserver) :<|>
     (coerce -> pUT10ApplianceManagementSystemSyslogservers) :<|>
     (coerce -> pUT10ApplianceManagementSystemTimesettings) :<|>
     (coerce -> pUT10DirectoryDeltaSyncDomainID) :<|>
     (coerce -> pUT10DirectoryFullSyncDomainID) :<|>
     (coerce -> pUT10TelemetryConfig) :<|>
     (coerce -> pUT10TelemetryProxy) :<|>
     (coerce -> pUT20CapacityParametersThresholds) :<|>
     (coerce -> pUT20EndpointsecurityUsvmstatsUsvmhealththresholds) :<|>
     (coerce -> pUT20NwfabricClustersClusterID) :<|>
     (coerce -> pUT20NwfabricConfigure) :<|>
     (coerce -> pUT20NwfabricHostsHostID) :<|>
     (coerce -> pUT20ServicesApplicationApplicationId) :<|>
     (coerce -> pUT20ServicesApplicationgroupApplicationgroupId) :<|>
     (coerce -> pUT20ServicesApplicationgroupApplicationgroupIdMembersMoref) :<|>
     (coerce -> pUT20ServicesAuthTokenexpiration) :<|>
     (coerce -> pUT20ServicesConfiguration) :<|>
     (coerce -> pUT20ServicesDashboardUiViewsDashboardWidgetconfigurationsWidgetconfigurationId) :<|>
     (coerce -> pUT20ServicesHousekeepingManagementIndexMaintenance) :<|>
     (coerce -> pUT20ServicesIpamPoolsPoolId) :<|>
     (coerce -> pUT20ServicesIpsetIpsetId) :<|>
     (coerce -> pUT20ServicesMacsetMacsetId) :<|>
     (coerce -> pUT20ServicesPolicySecuritypolicyID) :<|>
     (coerce -> pUT20ServicesPolicySecuritypolicyIDSgbindingSecurityGroupId) :<|>
     (coerce -> pUT20ServicesPolicySecuritypolicyServiceproviderFirewall) :<|>
     (coerce -> pUT20ServicesSecuritygroupBulkObjectId) :<|>
     (coerce -> pUT20ServicesSecuritygroupObjectId) :<|>
     (coerce -> pUT20ServicesSecuritygroupObjectIdMembersMemberId) :<|>
     (coerce -> pUT20ServicesSecuritytagsSelectionCriteria) :<|>
     (coerce -> pUT20ServicesSecuritytagsTagTagIdVmVmId) :<|>
     (coerce -> pUT20ServicesSnmpManagerManagerId) :<|>
     (coerce -> pUT20ServicesSnmpStatus) :<|>
     (coerce -> pUT20ServicesSnmpTrapOid) :<|>
     (coerce -> pUT20ServicesTruststoreConfig) :<|>
     (coerce -> pUT20ServicesTruststoreCsrCsrId) :<|>
     (coerce -> pUT20ServicesUsermgmtRoleUserId) :<|>
     (coerce -> pUT20ServicesUsermgmtUserUserIdEnablestateValue) :<|>
     (coerce -> pUT20ServicesVcconfig) :<|>
     (coerce -> pUT20SiDeploy) :<|>
     (coerce -> pUT20SiFabricSyncConflicts) :<|>
     (coerce -> pUT20UniversalsyncConfigurationNsxmanagersNsxManagerID) :<|>
     (coerce -> pUT20VdnBfdConfigurationGlobal) :<|>
     (coerce -> pUT20VdnConfigMulticastsMulticastAddresssRangeId) :<|>
     (coerce -> pUT20VdnConfigSegmentsSegmentPoolId) :<|>
     (coerce -> pUT20VdnConfigVxlanUdpPortPortNumber) :<|>
     (coerce -> pUT20VdnControllerCluster) :<|>
     (coerce -> pUT20VdnControllerClusterNtp) :<|>
     (coerce -> pUT20VdnControllerControllerId) :<|>
     (coerce -> pUT20VdnControllerCredential) :<|>
     (coerce -> pUT20VdnControllerSynchronize) :<|>
     (coerce -> pUT20VdnHardwaregatewayBfdConfig) :<|>
     (coerce -> pUT20VdnHardwaregatewayBindingsBindingId) :<|>
     (coerce -> pUT20VdnHardwaregatewaysHardwareGatewayId) :<|>
     (coerce -> pUT20VdnHardwaregatewaysReplicationcluster) :<|>
     (coerce -> pUT20VdnPnicCheckConfigurationGlobal) :<|>
     (coerce -> pUT20VdnScopesScopeIdAttributes) :<|>
     (coerce -> pUT20VdnVirtualwiresVirtualWireID) :<|>
     (coerce -> pUT20XvsNetworksIDFeatures) :<|>
     (coerce -> pUT21AppExcludelistMemberID) :<|>
     (coerce -> pUT21AppFlowConfig) :<|>
     (coerce -> pUT40EdgePublishTuningConfiguration) :<|>
     (coerce -> pUT40EdgesEdgeId) :<|>
     (coerce -> pUT40EdgesEdgeIdAppliances) :<|>
     (coerce -> pUT40EdgesEdgeIdAppliancesHaIndex) :<|>
     (coerce -> pUT40EdgesEdgeIdAutoconfiguration) :<|>
     (coerce -> pUT40EdgesEdgeIdBridgingConfig) :<|>
     (coerce -> pUT40EdgesEdgeIdClisettings) :<|>
     (coerce -> pUT40EdgesEdgeIdDhcpConfig) :<|>
     (coerce -> pUT40EdgesEdgeIdDhcpConfigRelay) :<|>
     (coerce -> pUT40EdgesEdgeIdDnsConfig) :<|>
     (coerce -> pUT40EdgesEdgeIdDnsclient) :<|>
     (coerce -> pUT40EdgesEdgeIdFirewallConfig) :<|>
     (coerce -> pUT40EdgesEdgeIdFirewallConfigDefaultpolicy) :<|>
     (coerce -> pUT40EdgesEdgeIdFirewallConfigGlobal) :<|>
     (coerce -> pUT40EdgesEdgeIdFirewallConfigRulesRuleId) :<|>
     (coerce -> pUT40EdgesEdgeIdHighavailabilityConfig) :<|>
     (coerce -> pUT40EdgesEdgeIdInterfacesIndex) :<|>
     (coerce -> pUT40EdgesEdgeIdIpsecConfig) :<|>
     (coerce -> pUT40EdgesEdgeIdL2vpnConfig) :<|>
     (coerce -> pUT40EdgesEdgeIdLoadbalancerConfig) :<|>
     (coerce -> pUT40EdgesEdgeIdLoadbalancerConfigApplicationprofilesAppProfileID) :<|>
     (coerce -> pUT40EdgesEdgeIdLoadbalancerConfigApplicationrulesAppruleID) :<|>
     (coerce -> pUT40EdgesEdgeIdLoadbalancerConfigMonitorsMonitorID) :<|>
     (coerce -> pUT40EdgesEdgeIdLoadbalancerConfigPoolsPoolID) :<|>
     (coerce -> pUT40EdgesEdgeIdMgmtinterface) :<|>
     (coerce -> pUT40EdgesEdgeIdNatConfig) :<|>
     (coerce -> pUT40EdgesEdgeIdNatConfigRulesRuleID) :<|>
     (coerce -> pUT40EdgesEdgeIdRoutingConfig) :<|>
     (coerce -> pUT40EdgesEdgeIdRoutingConfigBgp) :<|>
     (coerce -> pUT40EdgesEdgeIdRoutingConfigGlobal) :<|>
     (coerce -> pUT40EdgesEdgeIdRoutingConfigOspf) :<|>
     (coerce -> pUT40EdgesEdgeIdRoutingConfigStatic) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnAuthLocalusersUsers) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfig) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigAdvancedconfig) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigAuthLocalserverUsers) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigAuthSettings) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionClientconfig) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackagesPackageID) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppoolsIppoolID) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworksNetworkID) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigLayout) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigScript) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigScriptFileID) :<|>
     (coerce -> pUT40EdgesEdgeIdSslvpnConfigServer) :<|>
     (coerce -> pUT40EdgesEdgeIdSyslogConfig) :<|>
     (coerce -> pUT40EdgesEdgeIdSystemcontrolConfig) :<|>
     (coerce -> pUT40EdgesEdgeIdTunnels) :<|>
     (coerce -> pUT40EdgesEdgeIdTunnelsTunnelId) :<|>
     (coerce -> pUT40EdgesEdgeIdVnicsIndex) :<|>
     (coerce -> pUT40EdgesEdgeIdVnicsParentVnicIndexSubinterfacesSubInterfaceIndex) :<|>
     (coerce -> pUT40FirewallConfigGlobalconfiguration) :<|>
     (coerce -> pUT40FirewallDomainIDEnableTruefalse) :<|>
     (coerce -> pUT40FirewallGlobalroot0Config) :<|>
     (coerce -> pUT40FirewallGlobalroot0ConfigIpfix) :<|>
     (coerce -> pUT40FirewallGlobalroot0ConfigLayer2sectionsSectionId) :<|>
     (coerce -> pUT40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRulesRuleId) :<|>
     (coerce -> pUT40FirewallGlobalroot0ConfigLayer3redirectsectionsSection) :<|>
     (coerce -> pUT40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRulesRuleID) :<|>
     (coerce -> pUT40FirewallGlobalroot0ConfigLayer3sectionsSectionId) :<|>
     (coerce -> pUT40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRulesRuleId) :<|>
     (coerce -> pUT40FirewallGlobalroot0DraftsDraftID) :<|>
     (coerce -> pUT40FirewallGlobalroot0State) :<|>
     (coerce -> pUT40FirewallGlobalroot0TimeoutsConfigId) :<|>
     (coerce -> pUT40FirewallStatsEventthresholds) :<|>
     (coerce -> pUT40FirewallStatsThresholds) :<|>
     (coerce -> pUT40ServicesSpoofguardPoliciesPolicyID)) = client (Proxy :: Proxy VMwareNSXForVSphereAPI)

-- | Run requests in the VMwareNSXForVSphereClient monad.
runVMwareNSXForVSphereClient :: ServerConfig -> VMwareNSXForVSphereClient a -> ExceptT ServantError IO a
runVMwareNSXForVSphereClient clientConfig cl = do
  manager <- liftIO $ newManager defaultManagerSettings
  runVMwareNSXForVSphereClientWithManager manager clientConfig cl

-- | Run requests in the VMwareNSXForVSphereClient monad using a custom manager.
runVMwareNSXForVSphereClientWithManager :: Manager -> ServerConfig -> VMwareNSXForVSphereClient a -> ExceptT ServantError IO a
runVMwareNSXForVSphereClientWithManager manager clientConfig cl =
  runClient cl manager $ BaseUrl Http (configHost clientConfig) (configPort clientConfig) ""

-- | Run the VMwareNSXForVSphere server at the provided host and port.
runVMwareNSXForVSphereServer :: MonadIO m => ServerConfig -> VMwareNSXForVSphereBackend (ExceptT ServantErr IO)  -> m ()
runVMwareNSXForVSphereServer ServerConfig{..} backend =
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy VMwareNSXForVSphereAPI) (serverFromBackend backend)
  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
    serverFromBackend VMwareNSXForVSphereBackend{..} =
      (coerce dELETE10ApplianceManagementBackuprestoreBackupsettings :<|>
       coerce dELETE10ApplianceManagementBackuprestoreBackupsettingsSchedule :<|>
       coerce dELETE10ApplianceManagementNotifications :<|>
       coerce dELETE10ApplianceManagementSystemNetworkDns :<|>
       coerce dELETE10ApplianceManagementSystemSyslogserver :<|>
       coerce dELETE10ApplianceManagementSystemSyslogservers :<|>
       coerce dELETE10ApplianceManagementSystemTimesettingsNtp :<|>
       coerce dELETE10DirectoryDeleteDomainID :<|>
       coerce dELETE10DirectoryDeleteDomainRootDNDomainID :<|>
       coerce dELETE10DirectoryDeleteEventLogServerServerID :<|>
       coerce dELETE10DirectoryDeleteLdapServerServerID :<|>
       coerce dELETE10IdentityStaticUserMappingsbyIPIP :<|>
       coerce dELETE10IdentityStaticUserMappingsbyUserUserID :<|>
       coerce dELETE20EndpointsecurityActivationVendorIDAltitudeMoid :<|>
       coerce dELETE20EndpointsecurityRegistrationVendorID :<|>
       coerce dELETE20EndpointsecurityRegistrationVendorIDAltitude :<|>
       coerce dELETE20EndpointsecurityRegistrationVendorIDAltitudeLocation :<|>
       coerce dELETE20NwfabricClustersClusterID :<|>
       coerce dELETE20NwfabricConfigure :<|>
       coerce dELETE20NwfabricHostsHostID :<|>
       coerce dELETE20ServicesApplicationApplicationId :<|>
       coerce dELETE20ServicesApplicationgroupApplicationgroupId :<|>
       coerce dELETE20ServicesApplicationgroupApplicationgroupIdMembersMoref :<|>
       coerce dELETE20ServicesDashboardUiViewsDashboardWidgetconfigurationsWidgetconfigurationId :<|>
       coerce dELETE20ServicesIpamPoolsPoolId :<|>
       coerce dELETE20ServicesIpamPoolsPoolIdIpaddressesIpAddress :<|>
       coerce dELETE20ServicesIpsetIpsetId :<|>
       coerce dELETE20ServicesMacsetMacsetId :<|>
       coerce dELETE20ServicesPolicySecuritypolicyID :<|>
       coerce dELETE20ServicesSecuritygroupObjectId :<|>
       coerce dELETE20ServicesSecuritygroupObjectIdMembersMemberId :<|>
       coerce dELETE20ServicesSecuritytagsTagTagId :<|>
       coerce dELETE20ServicesSecuritytagsTagTagIdVmVmId :<|>
       coerce dELETE20ServicesSnmpManagerManagerId :<|>
       coerce dELETE20ServicesSsoconfig :<|>
       coerce dELETE20ServicesTruststoreConfigCertificateId :<|>
       coerce dELETE20ServicesTruststoreCrlCrlId :<|>
       coerce dELETE20ServicesUsermgmtRoleUserId :<|>
       coerce dELETE20ServicesUsermgmtUserUserId :<|>
       coerce dELETE20SiDeployClusterClusterID :<|>
       coerce dELETE20SiDeployServiceServiceID :<|>
       coerce dELETE20Techsupportbundle :<|>
       coerce dELETE20UniversalsyncConfigurationNsxmanagers :<|>
       coerce dELETE20UniversalsyncConfigurationNsxmanagersNsxManagerID :<|>
       coerce dELETE20VdnConfigMulticastsMulticastAddresssRangeId :<|>
       coerce dELETE20VdnConfigSegmentsSegmentPoolId :<|>
       coerce dELETE20VdnControllerControllerId :<|>
       coerce dELETE20VdnControllerControllerIdSyslog :<|>
       coerce dELETE20VdnHardwaregatewayBindingsBindingId :<|>
       coerce dELETE20VdnHardwaregatewaysHardwareGatewayId :<|>
       coerce dELETE20VdnScopesScopeId :<|>
       coerce dELETE20VdnSwitchesVdsId :<|>
       coerce dELETE20VdnVirtualwiresVirtualWireID :<|>
       coerce dELETE21AppExcludelistMemberID :<|>
       coerce dELETE21AppFlowContextId :<|>
       coerce dELETE40EdgesEdgeId :<|>
       coerce dELETE40EdgesEdgeIdAppliancesHaIndex :<|>
       coerce dELETE40EdgesEdgeIdBridgingConfig :<|>
       coerce dELETE40EdgesEdgeIdDhcpConfig :<|>
       coerce dELETE40EdgesEdgeIdDhcpConfigBindingsBindingID :<|>
       coerce dELETE40EdgesEdgeIdDhcpConfigIppoolsPoolID :<|>
       coerce dELETE40EdgesEdgeIdDhcpConfigRelay :<|>
       coerce dELETE40EdgesEdgeIdDnsConfig :<|>
       coerce dELETE40EdgesEdgeIdFirewallConfig :<|>
       coerce dELETE40EdgesEdgeIdFirewallConfigRulesRuleId :<|>
       coerce dELETE40EdgesEdgeIdHighavailabilityConfig :<|>
       coerce dELETE40EdgesEdgeIdInterfaces :<|>
       coerce dELETE40EdgesEdgeIdInterfacesIndex :<|>
       coerce dELETE40EdgesEdgeIdIpsecConfig :<|>
       coerce dELETE40EdgesEdgeIdL2vpnConfig :<|>
       coerce dELETE40EdgesEdgeIdLoadbalancerConfig :<|>
       coerce dELETE40EdgesEdgeIdLoadbalancerConfigApplicationprofiles :<|>
       coerce dELETE40EdgesEdgeIdLoadbalancerConfigApplicationprofilesAppProfileID :<|>
       coerce dELETE40EdgesEdgeIdLoadbalancerConfigApplicationrules :<|>
       coerce dELETE40EdgesEdgeIdLoadbalancerConfigApplicationrulesAppruleID :<|>
       coerce dELETE40EdgesEdgeIdLoadbalancerConfigMonitors :<|>
       coerce dELETE40EdgesEdgeIdLoadbalancerConfigMonitorsMonitorID :<|>
       coerce dELETE40EdgesEdgeIdLoadbalancerConfigPools :<|>
       coerce dELETE40EdgesEdgeIdLoadbalancerConfigPoolsPoolID :<|>
       coerce dELETE40EdgesEdgeIdLoadbalancerConfigVirtualservers :<|>
       coerce dELETE40EdgesEdgeIdLoadbalancerConfigVirtualserversVirtualserverID :<|>
       coerce dELETE40EdgesEdgeIdNatConfig :<|>
       coerce dELETE40EdgesEdgeIdNatConfigRulesRuleID :<|>
       coerce dELETE40EdgesEdgeIdRoutingConfig :<|>
       coerce dELETE40EdgesEdgeIdRoutingConfigBgp :<|>
       coerce dELETE40EdgesEdgeIdRoutingConfigOspf :<|>
       coerce dELETE40EdgesEdgeIdRoutingConfigStatic :<|>
       coerce dELETE40EdgesEdgeIdSslvpnActivesessionsSessionID :<|>
       coerce dELETE40EdgesEdgeIdSslvpnConfig :<|>
       coerce dELETE40EdgesEdgeIdSslvpnConfigAuthLocalserverUsers :<|>
       coerce dELETE40EdgesEdgeIdSslvpnConfigAuthLocalserverUsersUserID :<|>
       coerce dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages :<|>
       coerce dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackagesPackageID :<|>
       coerce dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools :<|>
       coerce dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppoolsIppoolID :<|>
       coerce dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks :<|>
       coerce dELETE40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworksNetworkID :<|>
       coerce dELETE40EdgesEdgeIdSslvpnConfigScript :<|>
       coerce dELETE40EdgesEdgeIdSslvpnConfigScriptFileID :<|>
       coerce dELETE40EdgesEdgeIdSyslogConfig :<|>
       coerce dELETE40EdgesEdgeIdSystemcontrolConfig :<|>
       coerce dELETE40EdgesEdgeIdTunnels :<|>
       coerce dELETE40EdgesEdgeIdTunnelsTunnelId :<|>
       coerce dELETE40EdgesEdgeIdVnicsIndex :<|>
       coerce dELETE40EdgesEdgeIdVnicsParentVnicIndexSubinterfacesSubInterfaceIndex :<|>
       coerce dELETE40FirewallConfigSections :<|>
       coerce dELETE40FirewallGlobalroot0Config :<|>
       coerce dELETE40FirewallGlobalroot0ConfigIpfix :<|>
       coerce dELETE40FirewallGlobalroot0ConfigLayer2sectionsSectionId :<|>
       coerce dELETE40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRulesRuleId :<|>
       coerce dELETE40FirewallGlobalroot0ConfigLayer3redirectsectionsSection :<|>
       coerce dELETE40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRulesRuleID :<|>
       coerce dELETE40FirewallGlobalroot0ConfigLayer3sectionsSectionId :<|>
       coerce dELETE40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRulesRuleId :<|>
       coerce dELETE40FirewallGlobalroot0DraftsDraftID :<|>
       coerce dELETE40FirewallGlobalroot0TimeoutsConfigId :<|>
       coerce dELETE40ServicesSpoofguardPoliciesPolicyID :<|>
       coerce gET10ApplianceManagementBackuprestoreBackups :<|>
       coerce gET10ApplianceManagementBackuprestoreBackupsettings :<|>
       coerce gET10ApplianceManagementCertificatemanagerCertificatesNsx :<|>
       coerce gET10ApplianceManagementCertificatemanagerCsrNsx :<|>
       coerce gET10ApplianceManagementComponents :<|>
       coerce gET10ApplianceManagementComponentsComponentComponentID :<|>
       coerce gET10ApplianceManagementComponentsComponentComponentIDDependencies :<|>
       coerce gET10ApplianceManagementComponentsComponentComponentIDDependents :<|>
       coerce gET10ApplianceManagementComponentsComponentComponentIDStatus :<|>
       coerce gET10ApplianceManagementGlobalInfo :<|>
       coerce gET10ApplianceManagementNotifications :<|>
       coerce gET10ApplianceManagementSummaryComponents :<|>
       coerce gET10ApplianceManagementSummarySystem :<|>
       coerce gET10ApplianceManagementSystemCpuinfo :<|>
       coerce gET10ApplianceManagementSystemCpuinfoDetails :<|>
       coerce gET10ApplianceManagementSystemLocale :<|>
       coerce gET10ApplianceManagementSystemMeminfo :<|>
       coerce gET10ApplianceManagementSystemNetwork :<|>
       coerce gET10ApplianceManagementSystemSecuritysettings :<|>
       coerce gET10ApplianceManagementSystemStorageinfo :<|>
       coerce gET10ApplianceManagementSystemSyslogserver :<|>
       coerce gET10ApplianceManagementSystemSyslogservers :<|>
       coerce gET10ApplianceManagementSystemTimesettings :<|>
       coerce gET10ApplianceManagementSystemTlssettings :<|>
       coerce gET10ApplianceManagementSystemUptime :<|>
       coerce gET10ApplianceManagementTechsupportlogsFilename :<|>
       coerce gET10ApplianceManagementUpgradeInformationComponentID :<|>
       coerce gET10ApplianceManagementUpgradeStatusComponentID :<|>
       coerce gET10DirectoryListDomains :<|>
       coerce gET10DirectoryListEventLogServersForDomainDomainID :<|>
       coerce gET10DirectoryListLdapServersForDomainDomainID :<|>
       coerce gET10EventcontrolConfigVmVmID :<|>
       coerce gET10IdentityDirectoryGroupsForUser :<|>
       coerce gET10IdentityHostIpMapping :<|>
       coerce gET10IdentityIpToUserMapping :<|>
       coerce gET10IdentityStaticUserMappings :<|>
       coerce gET10IdentityStaticUserMappingsbyIPIP :<|>
       coerce gET10IdentityStaticUserMappingsbyUserUserID :<|>
       coerce gET10IdentityUserIpMapping :<|>
       coerce gET10TelemetryConfig :<|>
       coerce gET10TelemetryProxy :<|>
       coerce gET20Auditlog :<|>
       coerce gET20CapacityParametersReport :<|>
       coerce gET20CapacityParametersThresholds :<|>
       coerce gET20EamStatus :<|>
       coerce gET20EndpointsecurityActivation :<|>
       coerce gET20EndpointsecurityActivationVendorIDAltitudeMoid :<|>
       coerce gET20EndpointsecurityActivationVendorIDSolutionID :<|>
       coerce gET20EndpointsecurityRegistrationVendorID :<|>
       coerce gET20EndpointsecurityRegistrationVendorIDAltitude :<|>
       coerce gET20EndpointsecurityRegistrationVendorIDAltitudeLocation :<|>
       coerce gET20EndpointsecurityRegistrationVendorIDSolutions :<|>
       coerce gET20EndpointsecurityRegistrationVendors :<|>
       coerce gET20EndpointsecurityUsvmstatsUsvmhealththresholds :<|>
       coerce gET20Hostevents :<|>
       coerce gET20NwfabricClustersClusterID :<|>
       coerce gET20NwfabricFeatures :<|>
       coerce gET20NwfabricHostsHostID :<|>
       coerce gET20NwfabricStatus :<|>
       coerce gET20NwfabricStatusAlleligibleResourceType :<|>
       coerce gET20NwfabricStatusChildParentResourceID :<|>
       coerce gET20ServicesAlarmsSourceId :<|>
       coerce gET20ServicesApplicationApplicationId :<|>
       coerce gET20ServicesApplicationScopeScopeId :<|>
       coerce gET20ServicesApplicationgroupApplicationgroupId :<|>
       coerce gET20ServicesApplicationgroupScopeScopeId :<|>
       coerce gET20ServicesApplicationgroupScopeScopeIdMembers :<|>
       coerce gET20ServicesAuthTokenexpiration :<|>
       coerce gET20ServicesConfiguration :<|>
       coerce gET20ServicesDashboardUiViewsDashboardWidgetconfigurations :<|>
       coerce gET20ServicesDashboardUiViewsDashboardWidgetconfigurationsWidgetconfigurationId :<|>
       coerce gET20ServicesHousekeepingManagementIndexMaintenance :<|>
       coerce gET20ServicesIpamPoolsPoolId :<|>
       coerce gET20ServicesIpamPoolsPoolIdIpaddresses :<|>
       coerce gET20ServicesIpamPoolsScopeScopeId :<|>
       coerce gET20ServicesIpsetIpsetId :<|>
       coerce gET20ServicesIpsetScopeScopeMoref :<|>
       coerce gET20ServicesLicensingCapacityusage :<|>
       coerce gET20ServicesLicensingStatus :<|>
       coerce gET20ServicesMacsetMacsetId :<|>
       coerce gET20ServicesMacsetScopeScopeId :<|>
       coerce gET20ServicesPolicySecurityactionCategoryVirtualmachines :<|>
       coerce gET20ServicesPolicySecuritygroupIDSecurityactions :<|>
       coerce gET20ServicesPolicySecuritygroupIDSecuritypolicies :<|>
       coerce gET20ServicesPolicySecuritypolicyAlarmsAll :<|>
       coerce gET20ServicesPolicySecuritypolicyAll :<|>
       coerce gET20ServicesPolicySecuritypolicyHierarchy :<|>
       coerce gET20ServicesPolicySecuritypolicyID :<|>
       coerce gET20ServicesPolicySecuritypolicyIDSecurityactions :<|>
       coerce gET20ServicesPolicySecuritypolicyMaxprecedence :<|>
       coerce gET20ServicesPolicySecuritypolicyServiceproviderFirewall :<|>
       coerce gET20ServicesPolicySecuritypolicyStatus :<|>
       coerce gET20ServicesPolicyServiceproviderFirewall :<|>
       coerce gET20ServicesPolicyServiceproviderFirewallInfo :<|>
       coerce gET20ServicesPolicyVirtualmachineIDSecurityactions :<|>
       coerce gET20ServicesSecuritygroupInternalScopeScopeId :<|>
       coerce gET20ServicesSecuritygroupLookupIpaddressIpAddress :<|>
       coerce gET20ServicesSecuritygroupLookupVirtualmachineVirtualMachineId :<|>
       coerce gET20ServicesSecuritygroupObjectId :<|>
       coerce gET20ServicesSecuritygroupObjectIdTranslationIpaddresses :<|>
       coerce gET20ServicesSecuritygroupObjectIdTranslationMacaddresses :<|>
       coerce gET20ServicesSecuritygroupObjectIdTranslationVirtualmachines :<|>
       coerce gET20ServicesSecuritygroupObjectIdTranslationVnics :<|>
       coerce gET20ServicesSecuritygroupScopeScopeId :<|>
       coerce gET20ServicesSecuritygroupScopeScopeIdMemberTypes :<|>
       coerce gET20ServicesSecuritygroupScopeScopeIdMembersMemberType :<|>
       coerce gET20ServicesSecuritytagsSelectionCriteria :<|>
       coerce gET20ServicesSecuritytagsTag :<|>
       coerce gET20ServicesSecuritytagsTagTagIdVm :<|>
       coerce gET20ServicesSecuritytagsTagTagIdVmDetail :<|>
       coerce gET20ServicesSecuritytagsVmVmId :<|>
       coerce gET20ServicesSnmpManager :<|>
       coerce gET20ServicesSnmpManagerManagerId :<|>
       coerce gET20ServicesSnmpStatus :<|>
       coerce gET20ServicesSnmpTrap :<|>
       coerce gET20ServicesSnmpTrapOid :<|>
       coerce gET20ServicesSsoconfig :<|>
       coerce gET20ServicesSsoconfigStatus :<|>
       coerce gET20ServicesSystemalarms :<|>
       coerce gET20ServicesSystemalarmsAlarmId :<|>
       coerce gET20ServicesTaskserviceJob :<|>
       coerce gET20ServicesTaskserviceJobJobId :<|>
       coerce gET20ServicesTranslationVirtualmachineVmIdIpaddresses :<|>
       coerce gET20ServicesTruststoreConfig :<|>
       coerce gET20ServicesTruststoreConfigCertificateId :<|>
       coerce gET20ServicesTruststoreConfigScopeScopeId :<|>
       coerce gET20ServicesTruststoreCrlCrlId :<|>
       coerce gET20ServicesTruststoreCrlScopeScopeId :<|>
       coerce gET20ServicesTruststoreCsrCsrId :<|>
       coerce gET20ServicesTruststoreCsrScopeScopeId :<|>
       coerce gET20ServicesUsermgmtRoleUserId :<|>
       coerce gET20ServicesUsermgmtRoles :<|>
       coerce gET20ServicesUsermgmtScopingobjects :<|>
       coerce gET20ServicesUsermgmtUserUserId :<|>
       coerce gET20ServicesUsermgmtUsersVsm :<|>
       coerce gET20ServicesVcconfig :<|>
       coerce gET20ServicesVcconfigStatus :<|>
       coerce gET20SiAgentAgentID :<|>
       coerce gET20SiDeployClusterClusterID :<|>
       coerce gET20SiDeployClusterClusterIDServiceServiceID :<|>
       coerce gET20SiDeployServiceServiceID :<|>
       coerce gET20SiDeployServiceServiceIDDependsOn :<|>
       coerce gET20SiDeploymentDeploymentunitIDAgents :<|>
       coerce gET20SiFabricSyncConflicts :<|>
       coerce gET20SiHostHostIDAgents :<|>
       coerce gET20SystemMonitorCpuusageDetails :<|>
       coerce gET20SystemMonitorCpuusageIndicator :<|>
       coerce gET20Systemevent :<|>
       coerce gET20TechsupportbundleFilename :<|>
       coerce gET20TechsupportbundleStatus :<|>
       coerce gET20UniversalsyncConfigurationNsxmanagers :<|>
       coerce gET20UniversalsyncConfigurationNsxmanagersNsxManagerID :<|>
       coerce gET20UniversalsyncConfigurationRole :<|>
       coerce gET20UniversalsyncEntitystatus :<|>
       coerce gET20UniversalsyncStatus :<|>
       coerce gET20VdnBfdConfigurationGlobal :<|>
       coerce gET20VdnCdo :<|>
       coerce gET20VdnConfigMulticasts :<|>
       coerce gET20VdnConfigMulticastsMulticastAddresssRangeId :<|>
       coerce gET20VdnConfigResourcesAllocated :<|>
       coerce gET20VdnConfigSegments :<|>
       coerce gET20VdnConfigSegmentsSegmentPoolId :<|>
       coerce gET20VdnConfigVxlanUdpPort :<|>
       coerce gET20VdnConfigVxlanUdpPortTaskStatus :<|>
       coerce gET20VdnController :<|>
       coerce gET20VdnControllerCluster :<|>
       coerce gET20VdnControllerClusterNtp :<|>
       coerce gET20VdnControllerControllerIdSnapshot :<|>
       coerce gET20VdnControllerControllerIdSyslog :<|>
       coerce gET20VdnControllerControllerIdSystemStats :<|>
       coerce gET20VdnControllerControllerIdTechsupportlogs :<|>
       coerce gET20VdnControllerProgressJobId :<|>
       coerce gET20VdnControllerSynchronizeStatus :<|>
       coerce gET20VdnControllerUpgradeAvailable :<|>
       coerce gET20VdnHardwaregatewayBfdConfig :<|>
       coerce gET20VdnHardwaregatewayBfdStatus :<|>
       coerce gET20VdnHardwaregatewayBindings :<|>
       coerce gET20VdnHardwaregatewayBindingsBindingId :<|>
       coerce gET20VdnHardwaregatewayBindingsBindingIdStatistic :<|>
       coerce gET20VdnHardwaregateways :<|>
       coerce gET20VdnHardwaregatewaysHardwareGatewayId :<|>
       coerce gET20VdnHardwaregatewaysHardwareGatewayIdSwitches :<|>
       coerce gET20VdnHardwaregatewaysHardwareGatewayIdSwitchesSwitchNameSwitchports :<|>
       coerce gET20VdnHardwaregatewaysReplicationcluster :<|>
       coerce gET20VdnHostHostIdRemoteHostStatus :<|>
       coerce gET20VdnHostHostIdStatus :<|>
       coerce gET20VdnHostHostIdTunnel :<|>
       coerce gET20VdnHostStatus :<|>
       coerce gET20VdnInventoryHostHostIdConnectionStatus :<|>
       coerce gET20VdnInventoryHostsConnectionStatus :<|>
       coerce gET20VdnPnicCheckConfigurationGlobal :<|>
       coerce gET20VdnScopes :<|>
       coerce gET20VdnScopesScopeId :<|>
       coerce gET20VdnScopesScopeIdVirtualwires :<|>
       coerce gET20VdnSwitches :<|>
       coerce gET20VdnSwitchesDatacenterDatacenterID :<|>
       coerce gET20VdnSwitchesVdsId :<|>
       coerce gET20VdnTraceflowTraceflowId :<|>
       coerce gET20VdnTraceflowTraceflowIdObservations :<|>
       coerce gET20VdnVirtualwires :<|>
       coerce gET20VdnVirtualwiresVirtualWireID :<|>
       coerce gET20VdnVirtualwiresVirtualWireIDHardwaregateways :<|>
       coerce gET20XvsNetworksIDFeatures :<|>
       coerce gET21AppExcludelist :<|>
       coerce gET21AppFlowConfig :<|>
       coerce gET21AppFlowFlowstats :<|>
       coerce gET21AppFlowFlowstatsInfo :<|>
       coerce gET30AiApp :<|>
       coerce gET30AiAppAppID :<|>
       coerce gET30AiDesktoppool :<|>
       coerce gET30AiDesktoppoolDesktoppoolID :<|>
       coerce gET30AiDirectorygroup :<|>
       coerce gET30AiDirectorygroupDirectorygroupID :<|>
       coerce gET30AiDirectorygroupUserUserID :<|>
       coerce gET30AiHost :<|>
       coerce gET30AiHostHostID :<|>
       coerce gET30AiRecords :<|>
       coerce gET30AiSecuritygroup :<|>
       coerce gET30AiSecuritygroupSecgroupID :<|>
       coerce gET30AiUserUserID :<|>
       coerce gET30AiUserdetails :<|>
       coerce gET30AiVm :<|>
       coerce gET30AiVmVmID :<|>
       coerce gET40EdgePublishTuningConfiguration :<|>
       coerce gET40Edges :<|>
       coerce gET40EdgesEdgeId :<|>
       coerce gET40EdgesEdgeIdAppliances :<|>
       coerce gET40EdgesEdgeIdAppliancesHaIndex :<|>
       coerce gET40EdgesEdgeIdAutoconfiguration :<|>
       coerce gET40EdgesEdgeIdBridgingConfig :<|>
       coerce gET40EdgesEdgeIdDhcpConfig :<|>
       coerce gET40EdgesEdgeIdDhcpConfigBindings :<|>
       coerce gET40EdgesEdgeIdDhcpConfigBindingsBindingID :<|>
       coerce gET40EdgesEdgeIdDhcpConfigRelay :<|>
       coerce gET40EdgesEdgeIdDhcpLeaseInfo :<|>
       coerce gET40EdgesEdgeIdDnsConfig :<|>
       coerce gET40EdgesEdgeIdDnsStatistics :<|>
       coerce gET40EdgesEdgeIdFirewallConfig :<|>
       coerce gET40EdgesEdgeIdFirewallConfigDefaultpolicy :<|>
       coerce gET40EdgesEdgeIdFirewallConfigGlobal :<|>
       coerce gET40EdgesEdgeIdFirewallConfigRulesRuleId :<|>
       coerce gET40EdgesEdgeIdFirewallStatisticsRuleId :<|>
       coerce gET40EdgesEdgeIdHealthsummary :<|>
       coerce gET40EdgesEdgeIdHighavailabilityConfig :<|>
       coerce gET40EdgesEdgeIdInterfaces :<|>
       coerce gET40EdgesEdgeIdInterfacesIndex :<|>
       coerce gET40EdgesEdgeIdIpsecConfig :<|>
       coerce gET40EdgesEdgeIdIpsecStatistics :<|>
       coerce gET40EdgesEdgeIdL2vpnConfig :<|>
       coerce gET40EdgesEdgeIdL2vpnConfigStatistics :<|>
       coerce gET40EdgesEdgeIdLoadbalancerConfig :<|>
       coerce gET40EdgesEdgeIdLoadbalancerConfigApplicationprofiles :<|>
       coerce gET40EdgesEdgeIdLoadbalancerConfigApplicationprofilesAppProfileID :<|>
       coerce gET40EdgesEdgeIdLoadbalancerConfigApplicationrules :<|>
       coerce gET40EdgesEdgeIdLoadbalancerConfigApplicationrulesAppruleID :<|>
       coerce gET40EdgesEdgeIdLoadbalancerConfigMonitors :<|>
       coerce gET40EdgesEdgeIdLoadbalancerConfigMonitorsMonitorID :<|>
       coerce gET40EdgesEdgeIdLoadbalancerConfigPools :<|>
       coerce gET40EdgesEdgeIdLoadbalancerConfigPoolsPoolID :<|>
       coerce gET40EdgesEdgeIdLoadbalancerConfigVirtualservers :<|>
       coerce gET40EdgesEdgeIdLoadbalancerConfigVirtualserversVirtualserverID :<|>
       coerce gET40EdgesEdgeIdLoadbalancerStatistics :<|>
       coerce gET40EdgesEdgeIdMgmtinterface :<|>
       coerce gET40EdgesEdgeIdNatConfig :<|>
       coerce gET40EdgesEdgeIdRoutingConfig :<|>
       coerce gET40EdgesEdgeIdRoutingConfigBgp :<|>
       coerce gET40EdgesEdgeIdRoutingConfigGlobal :<|>
       coerce gET40EdgesEdgeIdRoutingConfigOspf :<|>
       coerce gET40EdgesEdgeIdRoutingConfigStatic :<|>
       coerce gET40EdgesEdgeIdSslvpnActivesessions :<|>
       coerce gET40EdgesEdgeIdSslvpnConfig :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigAdvancedconfig :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigAuthLocalserverUsersUserID :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigAuthSettings :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionClientconfig :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackagesPackageID :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppoolsIppoolID :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworksNetworkID :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigLayout :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigScript :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigScriptFileID :<|>
       coerce gET40EdgesEdgeIdSslvpnConfigServer :<|>
       coerce gET40EdgesEdgeIdStatisticsDashboardFirewall :<|>
       coerce gET40EdgesEdgeIdStatisticsDashboardInterface :<|>
       coerce gET40EdgesEdgeIdStatisticsDashboardIpsec :<|>
       coerce gET40EdgesEdgeIdStatisticsDashboardSslvpn :<|>
       coerce gET40EdgesEdgeIdStatisticsInterfaces :<|>
       coerce gET40EdgesEdgeIdStatisticsInterfacesInternal :<|>
       coerce gET40EdgesEdgeIdStatisticsInterfacesUplink :<|>
       coerce gET40EdgesEdgeIdStatus :<|>
       coerce gET40EdgesEdgeIdSummary :<|>
       coerce gET40EdgesEdgeIdSyslogConfig :<|>
       coerce gET40EdgesEdgeIdSystemcontrolConfig :<|>
       coerce gET40EdgesEdgeIdTechsupportlogs :<|>
       coerce gET40EdgesEdgeIdTunnels :<|>
       coerce gET40EdgesEdgeIdTunnelsTunnelId :<|>
       coerce gET40EdgesEdgeIdVnics :<|>
       coerce gET40EdgesEdgeIdVnicsIndex :<|>
       coerce gET40EdgesEdgeIdVnicsParentVnicIndexSubinterfacesSubInterfaceIndex :<|>
       coerce gET40EdgesJobs :<|>
       coerce gET40EdgesJobsJobId :<|>
       coerce gET40FirewallConfigGlobalconfiguration :<|>
       coerce gET40FirewallGlobalroot0Config :<|>
       coerce gET40FirewallGlobalroot0ConfigIpfix :<|>
       coerce gET40FirewallGlobalroot0ConfigLayer2sections :<|>
       coerce gET40FirewallGlobalroot0ConfigLayer2sectionsSectionId :<|>
       coerce gET40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRulesRuleId :<|>
       coerce gET40FirewallGlobalroot0ConfigLayer3redirectProfiles :<|>
       coerce gET40FirewallGlobalroot0ConfigLayer3redirectsectionsSection :<|>
       coerce gET40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRulesRuleID :<|>
       coerce gET40FirewallGlobalroot0ConfigLayer3sections :<|>
       coerce gET40FirewallGlobalroot0ConfigLayer3sectionsSectionId :<|>
       coerce gET40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRulesRuleId :<|>
       coerce gET40FirewallGlobalroot0Defaultconfig :<|>
       coerce gET40FirewallGlobalroot0Drafts :<|>
       coerce gET40FirewallGlobalroot0DraftsDraftID :<|>
       coerce gET40FirewallGlobalroot0DraftsDraftIDActionExport :<|>
       coerce gET40FirewallGlobalroot0State :<|>
       coerce gET40FirewallGlobalroot0Status :<|>
       coerce gET40FirewallGlobalroot0StatusLayer2sectionsSectionID :<|>
       coerce gET40FirewallGlobalroot0StatusLayer3sectionsSectionID :<|>
       coerce gET40FirewallGlobalroot0Timeouts :<|>
       coerce gET40FirewallGlobalroot0TimeoutsConfigId :<|>
       coerce gET40FirewallStatsEventthresholds :<|>
       coerce gET40FirewallStatsThresholdsHostHostId :<|>
       coerce gET40FirewallStatsThresholdsTypes :<|>
       coerce gET40ServicesSpoofguardPolicies :<|>
       coerce gET40ServicesSpoofguardPoliciesPolicyID :<|>
       coerce gET40ServicesSpoofguardPolicyID :<|>
       coerce pOST10ApplianceManagementBackuprestoreBackup :<|>
       coerce pOST10ApplianceManagementBackuprestoreRestore :<|>
       coerce pOST10ApplianceManagementCertificatemanagerCsrNsx :<|>
       coerce pOST10ApplianceManagementCertificatemanagerPkcs12keystoreNsx :<|>
       coerce pOST10ApplianceManagementCertificatemanagerUploadchainNsx :<|>
       coerce pOST10ApplianceManagementComponentsComponentAPPMGMTRestart :<|>
       coerce pOST10ApplianceManagementComponentsComponentComponentIDToggleStatusCommand :<|>
       coerce pOST10ApplianceManagementNotificationsIDAcknowledge :<|>
       coerce pOST10ApplianceManagementSystemRestart :<|>
       coerce pOST10ApplianceManagementSystemSecuritysettings :<|>
       coerce pOST10ApplianceManagementSystemTlssettings :<|>
       coerce pOST10ApplianceManagementTechsupportlogsComponentID :<|>
       coerce pOST10ApplianceManagementUpgradeStartComponentID :<|>
       coerce pOST10ApplianceManagementUpgradeUploadbundleComponentID :<|>
       coerce pOST10ApplianceManagementUpgradeUploadbundlefromurl :<|>
       coerce pOST10DirectoryLdapSyncSettings :<|>
       coerce pOST10DirectoryUpdateDomain :<|>
       coerce pOST10DirectoryUpdateEventLogServer :<|>
       coerce pOST10DirectoryUpdateLdapServer :<|>
       coerce pOST10DirectoryVerifyRootDn :<|>
       coerce pOST10EventcontrolEventcontrolRootRequest :<|>
       coerce pOST10EventcontrolVmVmIDRequest :<|>
       coerce pOST10IdentityStaticUserMappingUserIDIP :<|>
       coerce pOST10NsxCli :<|>
       coerce pOST10SamSyslogDisable :<|>
       coerce pOST10SamSyslogEnable :<|>
       coerce pOST20EndpointsecurityActivationVendorIDAltitude :<|>
       coerce pOST20EndpointsecurityRegistration :<|>
       coerce pOST20EndpointsecurityRegistrationVendorID :<|>
       coerce pOST20EndpointsecurityRegistrationVendorIDAltitudeLocation :<|>
       coerce pOST20Hostevents :<|>
       coerce pOST20NwfabricConfigure :<|>
       coerce pOST20ServicesAlarmsSourceId :<|>
       coerce pOST20ServicesApplicationScopeId :<|>
       coerce pOST20ServicesApplicationgroupScopeId :<|>
       coerce pOST20ServicesAuthToken :<|>
       coerce pOST20ServicesAuthTokeninvalidation :<|>
       coerce pOST20ServicesDashboardUiViewsDashboardWidgetconfigurations :<|>
       coerce pOST20ServicesHousekeepingManagementIndexMaintenance :<|>
       coerce pOST20ServicesIpamPoolsPoolIdIpaddresses :<|>
       coerce pOST20ServicesIpamPoolsScopeScopeId :<|>
       coerce pOST20ServicesIpsetScopeMoref :<|>
       coerce pOST20ServicesMacsetScopeScopeId :<|>
       coerce pOST20ServicesPolicySecuritypolicy :<|>
       coerce pOST20ServicesPolicySecuritypolicyHierarchy :<|>
       coerce pOST20ServicesSecuritygroupBulkScopeId :<|>
       coerce pOST20ServicesSecuritygroupScopeId :<|>
       coerce pOST20ServicesSecuritytagsTag :<|>
       coerce pOST20ServicesSecuritytagsTagTagIdVm :<|>
       coerce pOST20ServicesSecuritytagsVmVmId :<|>
       coerce pOST20ServicesSnmpManager :<|>
       coerce pOST20ServicesSsoconfig :<|>
       coerce pOST20ServicesSystemalarmsAlarmId :<|>
       coerce pOST20ServicesTruststoreCertificate :<|>
       coerce pOST20ServicesTruststoreConfigScopeId :<|>
       coerce pOST20ServicesTruststoreCrlScopeId :<|>
       coerce pOST20ServicesTruststoreCsrScopeId :<|>
       coerce pOST20ServicesUsermgmtRoleUserId :<|>
       coerce pOST20ServicesVcconfigConnectionstatus :<|>
       coerce pOST20SiDeploy :<|>
       coerce pOST20Techsupportbundle :<|>
       coerce pOST20UniversalsyncConfigurationNsxmanagers :<|>
       coerce pOST20UniversalsyncConfigurationRole :<|>
       coerce pOST20UniversalsyncSync :<|>
       coerce pOST20VdnCdo :<|>
       coerce pOST20VdnConfigHostHostIdVxlanVteps :<|>
       coerce pOST20VdnConfigMulticasts :<|>
       coerce pOST20VdnConfigSegments :<|>
       coerce pOST20VdnConfigVxlanUdpPortResume :<|>
       coerce pOST20VdnController :<|>
       coerce pOST20VdnControllerControllerId :<|>
       coerce pOST20VdnControllerControllerIdSyslog :<|>
       coerce pOST20VdnHardwaregatewayBindings :<|>
       coerce pOST20VdnHardwaregatewayBindingsManage :<|>
       coerce pOST20VdnHardwaregateways :<|>
       coerce pOST20VdnScopes :<|>
       coerce pOST20VdnScopesScopeId :<|>
       coerce pOST20VdnScopesScopeIdCdo :<|>
       coerce pOST20VdnScopesScopeIdConnCheckMulticast :<|>
       coerce pOST20VdnScopesScopeIdVirtualwires :<|>
       coerce pOST20VdnSwitches :<|>
       coerce pOST20VdnTraceflow :<|>
       coerce pOST20VdnVirtualwiresVirtualWireIDBacking :<|>
       coerce pOST20VdnVirtualwiresVirtualWireIDConnCheckMulticast :<|>
       coerce pOST20VdnVirtualwiresVirtualWireIDConnCheckP2p :<|>
       coerce pOST20VdnVirtualwiresVirtualWireIDHardwaregatewaysHardwareGatewayBindingId :<|>
       coerce pOST20VdnVirtualwiresVmVnic :<|>
       coerce pOST40Edges :<|>
       coerce pOST40EdgesEdgeId :<|>
       coerce pOST40EdgesEdgeIdAesni :<|>
       coerce pOST40EdgesEdgeIdAppliances :<|>
       coerce pOST40EdgesEdgeIdAppliancesHaIndex :<|>
       coerce pOST40EdgesEdgeIdCliremoteaccess :<|>
       coerce pOST40EdgesEdgeIdCoredump :<|>
       coerce pOST40EdgesEdgeIdDhcpConfigBindings :<|>
       coerce pOST40EdgesEdgeIdDhcpConfigIppools :<|>
       coerce pOST40EdgesEdgeIdFips :<|>
       coerce pOST40EdgesEdgeIdFirewallConfigRules :<|>
       coerce pOST40EdgesEdgeIdInterfaces :<|>
       coerce pOST40EdgesEdgeIdL2vpnConfig :<|>
       coerce pOST40EdgesEdgeIdLoadbalancerAcceleration :<|>
       coerce pOST40EdgesEdgeIdLoadbalancerConfigApplicationprofiles :<|>
       coerce pOST40EdgesEdgeIdLoadbalancerConfigApplicationrules :<|>
       coerce pOST40EdgesEdgeIdLoadbalancerConfigMembersMemberID :<|>
       coerce pOST40EdgesEdgeIdLoadbalancerConfigMonitors :<|>
       coerce pOST40EdgesEdgeIdLoadbalancerConfigPools :<|>
       coerce pOST40EdgesEdgeIdLoadbalancerConfigVirtualservers :<|>
       coerce pOST40EdgesEdgeIdLogging :<|>
       coerce pOST40EdgesEdgeIdNatConfigRules :<|>
       coerce pOST40EdgesEdgeIdParentVnicIndexSubinterfaces :<|>
       coerce pOST40EdgesEdgeIdSslvpnConfig :<|>
       coerce pOST40EdgesEdgeIdSslvpnConfigAuthLocalserverUsers :<|>
       coerce pOST40EdgesEdgeIdSslvpnConfigAuthSettingsRsaconfigfile :<|>
       coerce pOST40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages :<|>
       coerce pOST40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools :<|>
       coerce pOST40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks :<|>
       coerce pOST40EdgesEdgeIdSslvpnConfigLayoutImagesImageType :<|>
       coerce pOST40EdgesEdgeIdSslvpnConfigScript :<|>
       coerce pOST40EdgesEdgeIdSslvpnConfigScriptFile :<|>
       coerce pOST40EdgesEdgeIdTunnels :<|>
       coerce pOST40EdgesEdgeIdVnics :<|>
       coerce pOST40FirewallForceSyncID :<|>
       coerce pOST40FirewallGlobalroot0ConfigLayer2sections :<|>
       coerce pOST40FirewallGlobalroot0ConfigLayer2sectionsSectionId :<|>
       coerce pOST40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRules :<|>
       coerce pOST40FirewallGlobalroot0ConfigLayer3redirectsections :<|>
       coerce pOST40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRules :<|>
       coerce pOST40FirewallGlobalroot0ConfigLayer3sections :<|>
       coerce pOST40FirewallGlobalroot0ConfigLayer3sectionsSectionId :<|>
       coerce pOST40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRules :<|>
       coerce pOST40FirewallGlobalroot0Drafts :<|>
       coerce pOST40FirewallGlobalroot0DraftsActionImport :<|>
       coerce pOST40FirewallGlobalroot0Timeouts :<|>
       coerce pOST40FirewallObjectsStatusVmVmIDContainers :<|>
       coerce pOST40ServicesSpoofguardPolicies :<|>
       coerce pOST40ServicesSpoofguardPolicyID :<|>
       coerce pUT10ApplianceManagementBackuprestoreBackupsettings :<|>
       coerce pUT10ApplianceManagementBackuprestoreBackupsettingsExcludedata :<|>
       coerce pUT10ApplianceManagementBackuprestoreBackupsettingsFtpsettings :<|>
       coerce pUT10ApplianceManagementBackuprestoreBackupsettingsSchedule :<|>
       coerce pUT10ApplianceManagementSystemLocale :<|>
       coerce pUT10ApplianceManagementSystemNetwork :<|>
       coerce pUT10ApplianceManagementSystemNetworkDns :<|>
       coerce pUT10ApplianceManagementSystemSyslogserver :<|>
       coerce pUT10ApplianceManagementSystemSyslogservers :<|>
       coerce pUT10ApplianceManagementSystemTimesettings :<|>
       coerce pUT10DirectoryDeltaSyncDomainID :<|>
       coerce pUT10DirectoryFullSyncDomainID :<|>
       coerce pUT10TelemetryConfig :<|>
       coerce pUT10TelemetryProxy :<|>
       coerce pUT20CapacityParametersThresholds :<|>
       coerce pUT20EndpointsecurityUsvmstatsUsvmhealththresholds :<|>
       coerce pUT20NwfabricClustersClusterID :<|>
       coerce pUT20NwfabricConfigure :<|>
       coerce pUT20NwfabricHostsHostID :<|>
       coerce pUT20ServicesApplicationApplicationId :<|>
       coerce pUT20ServicesApplicationgroupApplicationgroupId :<|>
       coerce pUT20ServicesApplicationgroupApplicationgroupIdMembersMoref :<|>
       coerce pUT20ServicesAuthTokenexpiration :<|>
       coerce pUT20ServicesConfiguration :<|>
       coerce pUT20ServicesDashboardUiViewsDashboardWidgetconfigurationsWidgetconfigurationId :<|>
       coerce pUT20ServicesHousekeepingManagementIndexMaintenance :<|>
       coerce pUT20ServicesIpamPoolsPoolId :<|>
       coerce pUT20ServicesIpsetIpsetId :<|>
       coerce pUT20ServicesMacsetMacsetId :<|>
       coerce pUT20ServicesPolicySecuritypolicyID :<|>
       coerce pUT20ServicesPolicySecuritypolicyIDSgbindingSecurityGroupId :<|>
       coerce pUT20ServicesPolicySecuritypolicyServiceproviderFirewall :<|>
       coerce pUT20ServicesSecuritygroupBulkObjectId :<|>
       coerce pUT20ServicesSecuritygroupObjectId :<|>
       coerce pUT20ServicesSecuritygroupObjectIdMembersMemberId :<|>
       coerce pUT20ServicesSecuritytagsSelectionCriteria :<|>
       coerce pUT20ServicesSecuritytagsTagTagIdVmVmId :<|>
       coerce pUT20ServicesSnmpManagerManagerId :<|>
       coerce pUT20ServicesSnmpStatus :<|>
       coerce pUT20ServicesSnmpTrapOid :<|>
       coerce pUT20ServicesTruststoreConfig :<|>
       coerce pUT20ServicesTruststoreCsrCsrId :<|>
       coerce pUT20ServicesUsermgmtRoleUserId :<|>
       coerce pUT20ServicesUsermgmtUserUserIdEnablestateValue :<|>
       coerce pUT20ServicesVcconfig :<|>
       coerce pUT20SiDeploy :<|>
       coerce pUT20SiFabricSyncConflicts :<|>
       coerce pUT20UniversalsyncConfigurationNsxmanagersNsxManagerID :<|>
       coerce pUT20VdnBfdConfigurationGlobal :<|>
       coerce pUT20VdnConfigMulticastsMulticastAddresssRangeId :<|>
       coerce pUT20VdnConfigSegmentsSegmentPoolId :<|>
       coerce pUT20VdnConfigVxlanUdpPortPortNumber :<|>
       coerce pUT20VdnControllerCluster :<|>
       coerce pUT20VdnControllerClusterNtp :<|>
       coerce pUT20VdnControllerControllerId :<|>
       coerce pUT20VdnControllerCredential :<|>
       coerce pUT20VdnControllerSynchronize :<|>
       coerce pUT20VdnHardwaregatewayBfdConfig :<|>
       coerce pUT20VdnHardwaregatewayBindingsBindingId :<|>
       coerce pUT20VdnHardwaregatewaysHardwareGatewayId :<|>
       coerce pUT20VdnHardwaregatewaysReplicationcluster :<|>
       coerce pUT20VdnPnicCheckConfigurationGlobal :<|>
       coerce pUT20VdnScopesScopeIdAttributes :<|>
       coerce pUT20VdnVirtualwiresVirtualWireID :<|>
       coerce pUT20XvsNetworksIDFeatures :<|>
       coerce pUT21AppExcludelistMemberID :<|>
       coerce pUT21AppFlowConfig :<|>
       coerce pUT40EdgePublishTuningConfiguration :<|>
       coerce pUT40EdgesEdgeId :<|>
       coerce pUT40EdgesEdgeIdAppliances :<|>
       coerce pUT40EdgesEdgeIdAppliancesHaIndex :<|>
       coerce pUT40EdgesEdgeIdAutoconfiguration :<|>
       coerce pUT40EdgesEdgeIdBridgingConfig :<|>
       coerce pUT40EdgesEdgeIdClisettings :<|>
       coerce pUT40EdgesEdgeIdDhcpConfig :<|>
       coerce pUT40EdgesEdgeIdDhcpConfigRelay :<|>
       coerce pUT40EdgesEdgeIdDnsConfig :<|>
       coerce pUT40EdgesEdgeIdDnsclient :<|>
       coerce pUT40EdgesEdgeIdFirewallConfig :<|>
       coerce pUT40EdgesEdgeIdFirewallConfigDefaultpolicy :<|>
       coerce pUT40EdgesEdgeIdFirewallConfigGlobal :<|>
       coerce pUT40EdgesEdgeIdFirewallConfigRulesRuleId :<|>
       coerce pUT40EdgesEdgeIdHighavailabilityConfig :<|>
       coerce pUT40EdgesEdgeIdInterfacesIndex :<|>
       coerce pUT40EdgesEdgeIdIpsecConfig :<|>
       coerce pUT40EdgesEdgeIdL2vpnConfig :<|>
       coerce pUT40EdgesEdgeIdLoadbalancerConfig :<|>
       coerce pUT40EdgesEdgeIdLoadbalancerConfigApplicationprofilesAppProfileID :<|>
       coerce pUT40EdgesEdgeIdLoadbalancerConfigApplicationrulesAppruleID :<|>
       coerce pUT40EdgesEdgeIdLoadbalancerConfigMonitorsMonitorID :<|>
       coerce pUT40EdgesEdgeIdLoadbalancerConfigPoolsPoolID :<|>
       coerce pUT40EdgesEdgeIdMgmtinterface :<|>
       coerce pUT40EdgesEdgeIdNatConfig :<|>
       coerce pUT40EdgesEdgeIdNatConfigRulesRuleID :<|>
       coerce pUT40EdgesEdgeIdRoutingConfig :<|>
       coerce pUT40EdgesEdgeIdRoutingConfigBgp :<|>
       coerce pUT40EdgesEdgeIdRoutingConfigGlobal :<|>
       coerce pUT40EdgesEdgeIdRoutingConfigOspf :<|>
       coerce pUT40EdgesEdgeIdRoutingConfigStatic :<|>
       coerce pUT40EdgesEdgeIdSslvpnAuthLocalusersUsers :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfig :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigAdvancedconfig :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigAuthLocalserverUsers :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigAuthSettings :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionClientconfig :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackages :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionInstallpackagesPackageID :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppools :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionIppoolsIppoolID :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworks :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigClientNetworkextensionPrivatenetworksNetworkID :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigLayout :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigScript :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigScriptFileID :<|>
       coerce pUT40EdgesEdgeIdSslvpnConfigServer :<|>
       coerce pUT40EdgesEdgeIdSyslogConfig :<|>
       coerce pUT40EdgesEdgeIdSystemcontrolConfig :<|>
       coerce pUT40EdgesEdgeIdTunnels :<|>
       coerce pUT40EdgesEdgeIdTunnelsTunnelId :<|>
       coerce pUT40EdgesEdgeIdVnicsIndex :<|>
       coerce pUT40EdgesEdgeIdVnicsParentVnicIndexSubinterfacesSubInterfaceIndex :<|>
       coerce pUT40FirewallConfigGlobalconfiguration :<|>
       coerce pUT40FirewallDomainIDEnableTruefalse :<|>
       coerce pUT40FirewallGlobalroot0Config :<|>
       coerce pUT40FirewallGlobalroot0ConfigIpfix :<|>
       coerce pUT40FirewallGlobalroot0ConfigLayer2sectionsSectionId :<|>
       coerce pUT40FirewallGlobalroot0ConfigLayer2sectionsSectionIdRulesRuleId :<|>
       coerce pUT40FirewallGlobalroot0ConfigLayer3redirectsectionsSection :<|>
       coerce pUT40FirewallGlobalroot0ConfigLayer3redirectsectionsSectionRulesRuleID :<|>
       coerce pUT40FirewallGlobalroot0ConfigLayer3sectionsSectionId :<|>
       coerce pUT40FirewallGlobalroot0ConfigLayer3sectionsSectionIdRulesRuleId :<|>
       coerce pUT40FirewallGlobalroot0DraftsDraftID :<|>
       coerce pUT40FirewallGlobalroot0State :<|>
       coerce pUT40FirewallGlobalroot0TimeoutsConfigId :<|>
       coerce pUT40FirewallStatsEventthresholds :<|>
       coerce pUT40FirewallStatsThresholds :<|>
       coerce pUT40ServicesSpoofguardPoliciesPolicyID)
