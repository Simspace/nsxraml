{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module VMwareNSXForVSphere.Types (
  AdvancedConfigUpdate (..),
  AdvancedConfigUpdate_timeout (..),
  AllUsersUpdate (..),
  AppRuleUpdate (..),
  AppRulesCreate (..),
  ApplianceDnsClientUpdate (..),
  ApplianceMgrBackupSettingsUpdate (..),
  ApplianceUpdate (..),
  ApplicationProfileUpdate (..),
  ApplicationProfilesCreate (..),
  ArpMACUpdate (..),
  AuthSettingsUpdate (..),
  AutoConfigUpdate (..),
  BridingUpdate (..),
  CertificateCreate (..),
  CertificateSelfSignedCreate (..),
  CliSettingsUpdate (..),
  ClientConfigUpdate (..),
  ClusterUpdate (..),
  Controller (..),
  ControllerSyslog (..),
  CrlCreate (..),
  CsrCreate (..),
  DataCollectionKillSwitchToggle (..),
  DataCollectionVMCreate (..),
  DefaultFirewallPolicyUpdate (..),
  DfwConfigImport (..),
  DfwDraftUpdate (..),
  DfwDraftsCreate (..),
  DfwIPFixUpdate (..),
  DfwPerformanceUpdate (..),
  DfwRule (..),
  DfwSection (..),
  DfwThresholdsUpdate (..),
  DhcpPoolCreate (..),
  DhcpRelayUpdate (..),
  DhcpStaticBindingCreate (..),
  DhcpUpdate (..),
  DomainCreate (..),
  ELogServerCreate (..),
  EdgeDnsClientUpdate (..),
  EdgeDnsUpdate (..),
  EdgeNatConfig (..),
  EdgeNatRuleUpdate (..),
  EdgeNatRulesCreate (..),
  FirewallRuleUpdate (..),
  FirewallRulesCreate (..),
  FlowsExcludeCreate (..),
  GlobalFirewallConfigUpdate (..),
  HierarchyCreate (..),
  HighAvailabilityCreate (..),
  InstallPackageUpdate (..),
  InstallPackagesCreate (..),
  InterfacesCreate (..),
  IpAddressRequest (..),
  IpPool (..),
  IpPoolUpdate (..),
  IpsetCreate (..),
  IpsetUpdate (..),
  Layer3RedirectSectionUpdate (..),
  Layer3RedirectSectionsCreate (..),
  LayoutUpdate (..),
  LbMonitorUpdate (..),
  LbMonitorsCreate (..),
  LdapServerCreate (..),
  LoadBalancerConfig (..),
  LogicalSwitchConnCheck (..),
  LogicalSwitchCreate (..),
  LogicalSwitchPing (..),
  LogicalSwitchUpdate (..),
  LogicalSwitchVmAttach (..),
  MacSetCreateUpdate (..),
  MgmtInterfaceUpdate (..),
  NetExtipPoolsCreate (..),
  NetExtipPoolsUpdate (..),
  NsxCliExecute (..),
  NsxControllerPasswordUpdate (..),
  NsxEdgeFirewallConfigUpdate (..),
  NsxEdgeUpdate (..),
  NsxEdgesCreate (..),
  NwFabricConfig (..),
  NwfabricClustersUpdate (..),
  NwfabricHostsUpdate (..),
  PoolUpdate (..),
  PoolsCreate (..),
  PrivateNetworkUpdate (..),
  PrivateNetworksCreate (..),
  RoutingBGPUpdate (..),
  RoutingConfigStaticUpdate (..),
  RoutingConfigUpdate (..),
  RoutingGlobalConfigUpdate (..),
  RoutingOSPFUpdate (..),
  RuleUpdate (..),
  RulesCreate (..),
  ScriptCreate (..),
  ScriptFileIDUpdate (..),
  ScriptUpdate (..),
  SecGroupBulkCreate (..),
  SecGroupBulkUpdate (..),
  SecGroupObjectUpdate (..),
  SecurityFabricCreate (..),
  SecurityPolicyCreate (..),
  SecurityPolicyIDUpdate (..),
  SecurityTagCreate (..),
  ServerSettingsUpdate (..),
  ServiceGroupUpdate (..),
  ServiceGroupsCreate (..),
  ServiceUpdate (..),
  ServiceUpgrade (..),
  ServicesScopeCreate (..),
  SolutionIPPortSet (..),
  SpoofGuardPoliciesCreate (..),
  SpoofGuardPolicyApprove (..),
  SpoofGuardPolicyUpdate (..),
  SslVPNUpdate (..),
  SsoConfig (..),
  SyslogUpdate (..),
  SystemLocaleUpdate (..),
  SystemSyslogServerUpdate (..),
  SystemTimeUpdate (..),
  TraceflowCreate (..),
  UniversalSyncConfigurationManagersUpdate (..),
  UniversalSyncConfigurationNsxManagersCreate (..),
  UserRoleMgmtCreate (..),
  UserRoleMgmtUpdate (..),
  UsersCreate (..),
  UsersUpdate (..),
  VShieldSolutionActivate (..),
  VShieldSolutionCreate (..),
  VShieldVendorCreate (..),
  VcConfig (..),
  VdnMulticast (..),
  VdnMulticastUpdate (..),
  VdnScopeCreate (..),
  VdnScopeEdit (..),
  VdnScopeUpdate (..),
  VdnSegment (..),
  VdnSegmentUpdate (..),
  VdsContext (..),
  VirtualServersCreate (..),
  WebResourcesCreate (..),
  ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data AdvancedConfigUpdate = AdvancedConfigUpdate
  { advancedConfigUpdateEnableCompression :: Text -- ^ 
  , advancedConfigUpdateForceVirtualKeyboard :: Text -- ^ 
  , advancedConfigUpdatePreventMultipleLogon :: Text -- ^ 
  , advancedConfigUpdateRandomizeVirtualkeys :: Text -- ^ 
  , advancedConfigUpdateClientNotification :: Text -- ^ 
  , advancedConfigUpdateEnableLogging :: Text -- ^ 
  , advancedConfigUpdateTimeout :: AdvancedConfigUpdate_timeout -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON AdvancedConfigUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "advancedConfigUpdate")
instance ToJSON AdvancedConfigUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "advancedConfigUpdate")

-- | 
data AdvancedConfigUpdate_timeout = AdvancedConfigUpdate_timeout
  { advancedConfigUpdateTimeoutForcedTimeout :: Text -- ^ 
  , advancedConfigUpdateTimeoutSessionIdleTimeout :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON AdvancedConfigUpdate_timeout where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "advancedConfigUpdateTimeout")
instance ToJSON AdvancedConfigUpdate_timeout where
  toJSON = genericToJSON (removeFieldLabelPrefix False "advancedConfigUpdateTimeout")

-- | 
data AllUsersUpdate = AllUsersUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON AllUsersUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "allUsersUpdate")
instance ToJSON AllUsersUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "allUsersUpdate")

-- | 
data AppRuleUpdate = AppRuleUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON AppRuleUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "appRuleUpdate")
instance ToJSON AppRuleUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "appRuleUpdate")

-- | 
data AppRulesCreate = AppRulesCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON AppRulesCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "appRulesCreate")
instance ToJSON AppRulesCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "appRulesCreate")

-- | 
data ApplianceDnsClientUpdate = ApplianceDnsClientUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ApplianceDnsClientUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "applianceDnsClientUpdate")
instance ToJSON ApplianceDnsClientUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "applianceDnsClientUpdate")

-- | 
data ApplianceMgrBackupSettingsUpdate = ApplianceMgrBackupSettingsUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ApplianceMgrBackupSettingsUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "applianceMgrBackupSettingsUpdate")
instance ToJSON ApplianceMgrBackupSettingsUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "applianceMgrBackupSettingsUpdate")

-- | 
data ApplianceUpdate = ApplianceUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ApplianceUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "applianceUpdate")
instance ToJSON ApplianceUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "applianceUpdate")

-- | 
data ApplicationProfileUpdate = ApplicationProfileUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ApplicationProfileUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "applicationProfileUpdate")
instance ToJSON ApplicationProfileUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "applicationProfileUpdate")

-- | 
data ApplicationProfilesCreate = ApplicationProfilesCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ApplicationProfilesCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "applicationProfilesCreate")
instance ToJSON ApplicationProfilesCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "applicationProfilesCreate")

-- | 
data ArpMACUpdate = ArpMACUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ArpMACUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "arpMACUpdate")
instance ToJSON ArpMACUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "arpMACUpdate")

-- | 
data AuthSettingsUpdate = AuthSettingsUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON AuthSettingsUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "authSettingsUpdate")
instance ToJSON AuthSettingsUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "authSettingsUpdate")

-- | 
data AutoConfigUpdate = AutoConfigUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON AutoConfigUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "autoConfigUpdate")
instance ToJSON AutoConfigUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "autoConfigUpdate")

-- | 
data BridingUpdate = BridingUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON BridingUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "bridingUpdate")
instance ToJSON BridingUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "bridingUpdate")

-- | 
data CertificateCreate = CertificateCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON CertificateCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "certificateCreate")
instance ToJSON CertificateCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "certificateCreate")

-- | 
data CertificateSelfSignedCreate = CertificateSelfSignedCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON CertificateSelfSignedCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "certificateSelfSignedCreate")
instance ToJSON CertificateSelfSignedCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "certificateSelfSignedCreate")

-- | 
data CliSettingsUpdate = CliSettingsUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON CliSettingsUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "cliSettingsUpdate")
instance ToJSON CliSettingsUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "cliSettingsUpdate")

-- | 
data ClientConfigUpdate = ClientConfigUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ClientConfigUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "clientConfigUpdate")
instance ToJSON ClientConfigUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "clientConfigUpdate")

-- | 
data ClusterUpdate = ClusterUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ClusterUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "clusterUpdate")
instance ToJSON ClusterUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "clusterUpdate")

-- | 
data Controller = Controller
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON Controller where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "controller")
instance ToJSON Controller where
  toJSON = genericToJSON (removeFieldLabelPrefix False "controller")

-- | 
data ControllerSyslog = ControllerSyslog
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ControllerSyslog where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "controllerSyslog")
instance ToJSON ControllerSyslog where
  toJSON = genericToJSON (removeFieldLabelPrefix False "controllerSyslog")

-- | 
data CrlCreate = CrlCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON CrlCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "crlCreate")
instance ToJSON CrlCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "crlCreate")

-- | 
data CsrCreate = CsrCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON CsrCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "csrCreate")
instance ToJSON CsrCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "csrCreate")

-- | 
data DataCollectionKillSwitchToggle = DataCollectionKillSwitchToggle
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DataCollectionKillSwitchToggle where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataCollectionKillSwitchToggle")
instance ToJSON DataCollectionKillSwitchToggle where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataCollectionKillSwitchToggle")

-- | 
data DataCollectionVMCreate = DataCollectionVMCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DataCollectionVMCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dataCollectionVMCreate")
instance ToJSON DataCollectionVMCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dataCollectionVMCreate")

-- | 
data DefaultFirewallPolicyUpdate = DefaultFirewallPolicyUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DefaultFirewallPolicyUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "defaultFirewallPolicyUpdate")
instance ToJSON DefaultFirewallPolicyUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "defaultFirewallPolicyUpdate")

-- | 
data DfwConfigImport = DfwConfigImport
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DfwConfigImport where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dfwConfigImport")
instance ToJSON DfwConfigImport where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dfwConfigImport")

-- | 
data DfwDraftUpdate = DfwDraftUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DfwDraftUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dfwDraftUpdate")
instance ToJSON DfwDraftUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dfwDraftUpdate")

-- | 
data DfwDraftsCreate = DfwDraftsCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DfwDraftsCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dfwDraftsCreate")
instance ToJSON DfwDraftsCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dfwDraftsCreate")

-- | 
data DfwIPFixUpdate = DfwIPFixUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DfwIPFixUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dfwIPFixUpdate")
instance ToJSON DfwIPFixUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dfwIPFixUpdate")

-- | 
data DfwPerformanceUpdate = DfwPerformanceUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DfwPerformanceUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dfwPerformanceUpdate")
instance ToJSON DfwPerformanceUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dfwPerformanceUpdate")

-- | 
data DfwRule = DfwRule
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DfwRule where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dfwRule")
instance ToJSON DfwRule where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dfwRule")

-- | 
data DfwSection = DfwSection
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DfwSection where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dfwSection")
instance ToJSON DfwSection where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dfwSection")

-- | 
data DfwThresholdsUpdate = DfwThresholdsUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DfwThresholdsUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dfwThresholdsUpdate")
instance ToJSON DfwThresholdsUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dfwThresholdsUpdate")

-- | 
data DhcpPoolCreate = DhcpPoolCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DhcpPoolCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dhcpPoolCreate")
instance ToJSON DhcpPoolCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dhcpPoolCreate")

-- | 
data DhcpRelayUpdate = DhcpRelayUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DhcpRelayUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dhcpRelayUpdate")
instance ToJSON DhcpRelayUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dhcpRelayUpdate")

-- | 
data DhcpStaticBindingCreate = DhcpStaticBindingCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DhcpStaticBindingCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dhcpStaticBindingCreate")
instance ToJSON DhcpStaticBindingCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dhcpStaticBindingCreate")

-- | 
data DhcpUpdate = DhcpUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DhcpUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "dhcpUpdate")
instance ToJSON DhcpUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "dhcpUpdate")

-- | 
data DomainCreate = DomainCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON DomainCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "domainCreate")
instance ToJSON DomainCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "domainCreate")

-- | 
data ELogServerCreate = ELogServerCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ELogServerCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "eLogServerCreate")
instance ToJSON ELogServerCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "eLogServerCreate")

-- | 
data EdgeDnsClientUpdate = EdgeDnsClientUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON EdgeDnsClientUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "edgeDnsClientUpdate")
instance ToJSON EdgeDnsClientUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "edgeDnsClientUpdate")

-- | 
data EdgeDnsUpdate = EdgeDnsUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON EdgeDnsUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "edgeDnsUpdate")
instance ToJSON EdgeDnsUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "edgeDnsUpdate")

-- | 
data EdgeNatConfig = EdgeNatConfig
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON EdgeNatConfig where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "edgeNatConfig")
instance ToJSON EdgeNatConfig where
  toJSON = genericToJSON (removeFieldLabelPrefix False "edgeNatConfig")

-- | 
data EdgeNatRuleUpdate = EdgeNatRuleUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON EdgeNatRuleUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "edgeNatRuleUpdate")
instance ToJSON EdgeNatRuleUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "edgeNatRuleUpdate")

-- | 
data EdgeNatRulesCreate = EdgeNatRulesCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON EdgeNatRulesCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "edgeNatRulesCreate")
instance ToJSON EdgeNatRulesCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "edgeNatRulesCreate")

-- | 
data FirewallRuleUpdate = FirewallRuleUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON FirewallRuleUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "firewallRuleUpdate")
instance ToJSON FirewallRuleUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "firewallRuleUpdate")

-- | 
data FirewallRulesCreate = FirewallRulesCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON FirewallRulesCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "firewallRulesCreate")
instance ToJSON FirewallRulesCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "firewallRulesCreate")

-- | 
data FlowsExcludeCreate = FlowsExcludeCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON FlowsExcludeCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "flowsExcludeCreate")
instance ToJSON FlowsExcludeCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "flowsExcludeCreate")

-- | 
data GlobalFirewallConfigUpdate = GlobalFirewallConfigUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON GlobalFirewallConfigUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "globalFirewallConfigUpdate")
instance ToJSON GlobalFirewallConfigUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "globalFirewallConfigUpdate")

-- | 
data HierarchyCreate = HierarchyCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON HierarchyCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "hierarchyCreate")
instance ToJSON HierarchyCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "hierarchyCreate")

-- | 
data HighAvailabilityCreate = HighAvailabilityCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON HighAvailabilityCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "highAvailabilityCreate")
instance ToJSON HighAvailabilityCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "highAvailabilityCreate")

-- | 
data InstallPackageUpdate = InstallPackageUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON InstallPackageUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "installPackageUpdate")
instance ToJSON InstallPackageUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "installPackageUpdate")

-- | 
data InstallPackagesCreate = InstallPackagesCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON InstallPackagesCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "installPackagesCreate")
instance ToJSON InstallPackagesCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "installPackagesCreate")

-- | 
data InterfacesCreate = InterfacesCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON InterfacesCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "interfacesCreate")
instance ToJSON InterfacesCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "interfacesCreate")

-- | 
data IpAddressRequest = IpAddressRequest
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON IpAddressRequest where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "ipAddressRequest")
instance ToJSON IpAddressRequest where
  toJSON = genericToJSON (removeFieldLabelPrefix False "ipAddressRequest")

-- | 
data IpPool = IpPool
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON IpPool where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "ipPool")
instance ToJSON IpPool where
  toJSON = genericToJSON (removeFieldLabelPrefix False "ipPool")

-- | 
data IpPoolUpdate = IpPoolUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON IpPoolUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "ipPoolUpdate")
instance ToJSON IpPoolUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "ipPoolUpdate")

-- | 
data IpsetCreate = IpsetCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON IpsetCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "ipsetCreate")
instance ToJSON IpsetCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "ipsetCreate")

-- | 
data IpsetUpdate = IpsetUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON IpsetUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "ipsetUpdate")
instance ToJSON IpsetUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "ipsetUpdate")

-- | 
data Layer3RedirectSectionUpdate = Layer3RedirectSectionUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON Layer3RedirectSectionUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "layer3RedirectSectionUpdate")
instance ToJSON Layer3RedirectSectionUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "layer3RedirectSectionUpdate")

-- | 
data Layer3RedirectSectionsCreate = Layer3RedirectSectionsCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON Layer3RedirectSectionsCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "layer3RedirectSectionsCreate")
instance ToJSON Layer3RedirectSectionsCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "layer3RedirectSectionsCreate")

-- | 
data LayoutUpdate = LayoutUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON LayoutUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "layoutUpdate")
instance ToJSON LayoutUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "layoutUpdate")

-- | 
data LbMonitorUpdate = LbMonitorUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON LbMonitorUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "lbMonitorUpdate")
instance ToJSON LbMonitorUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "lbMonitorUpdate")

-- | 
data LbMonitorsCreate = LbMonitorsCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON LbMonitorsCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "lbMonitorsCreate")
instance ToJSON LbMonitorsCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "lbMonitorsCreate")

-- | 
data LdapServerCreate = LdapServerCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON LdapServerCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "ldapServerCreate")
instance ToJSON LdapServerCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "ldapServerCreate")

-- | 
data LoadBalancerConfig = LoadBalancerConfig
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON LoadBalancerConfig where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "loadBalancerConfig")
instance ToJSON LoadBalancerConfig where
  toJSON = genericToJSON (removeFieldLabelPrefix False "loadBalancerConfig")

-- | 
data LogicalSwitchConnCheck = LogicalSwitchConnCheck
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON LogicalSwitchConnCheck where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "logicalSwitchConnCheck")
instance ToJSON LogicalSwitchConnCheck where
  toJSON = genericToJSON (removeFieldLabelPrefix False "logicalSwitchConnCheck")

-- | 
data LogicalSwitchCreate = LogicalSwitchCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON LogicalSwitchCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "logicalSwitchCreate")
instance ToJSON LogicalSwitchCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "logicalSwitchCreate")

-- | 
data LogicalSwitchPing = LogicalSwitchPing
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON LogicalSwitchPing where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "logicalSwitchPing")
instance ToJSON LogicalSwitchPing where
  toJSON = genericToJSON (removeFieldLabelPrefix False "logicalSwitchPing")

-- | 
data LogicalSwitchUpdate = LogicalSwitchUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON LogicalSwitchUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "logicalSwitchUpdate")
instance ToJSON LogicalSwitchUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "logicalSwitchUpdate")

-- | 
data LogicalSwitchVmAttach = LogicalSwitchVmAttach
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON LogicalSwitchVmAttach where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "logicalSwitchVmAttach")
instance ToJSON LogicalSwitchVmAttach where
  toJSON = genericToJSON (removeFieldLabelPrefix False "logicalSwitchVmAttach")

-- | 
data MacSetCreateUpdate = MacSetCreateUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON MacSetCreateUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "macSetCreateUpdate")
instance ToJSON MacSetCreateUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "macSetCreateUpdate")

-- | 
data MgmtInterfaceUpdate = MgmtInterfaceUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON MgmtInterfaceUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "mgmtInterfaceUpdate")
instance ToJSON MgmtInterfaceUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "mgmtInterfaceUpdate")

-- | 
data NetExtipPoolsCreate = NetExtipPoolsCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON NetExtipPoolsCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "netExtipPoolsCreate")
instance ToJSON NetExtipPoolsCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "netExtipPoolsCreate")

-- | 
data NetExtipPoolsUpdate = NetExtipPoolsUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON NetExtipPoolsUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "netExtipPoolsUpdate")
instance ToJSON NetExtipPoolsUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "netExtipPoolsUpdate")

-- | 
data NsxCliExecute = NsxCliExecute
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON NsxCliExecute where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "nsxCliExecute")
instance ToJSON NsxCliExecute where
  toJSON = genericToJSON (removeFieldLabelPrefix False "nsxCliExecute")

-- | 
data NsxControllerPasswordUpdate = NsxControllerPasswordUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON NsxControllerPasswordUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "nsxControllerPasswordUpdate")
instance ToJSON NsxControllerPasswordUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "nsxControllerPasswordUpdate")

-- | 
data NsxEdgeFirewallConfigUpdate = NsxEdgeFirewallConfigUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON NsxEdgeFirewallConfigUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "nsxEdgeFirewallConfigUpdate")
instance ToJSON NsxEdgeFirewallConfigUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "nsxEdgeFirewallConfigUpdate")

-- | 
data NsxEdgeUpdate = NsxEdgeUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON NsxEdgeUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "nsxEdgeUpdate")
instance ToJSON NsxEdgeUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "nsxEdgeUpdate")

-- | 
data NsxEdgesCreate = NsxEdgesCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON NsxEdgesCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "nsxEdgesCreate")
instance ToJSON NsxEdgesCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "nsxEdgesCreate")

-- | 
data NwFabricConfig = NwFabricConfig
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON NwFabricConfig where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "nwFabricConfig")
instance ToJSON NwFabricConfig where
  toJSON = genericToJSON (removeFieldLabelPrefix False "nwFabricConfig")

-- | 
data NwfabricClustersUpdate = NwfabricClustersUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON NwfabricClustersUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "nwfabricClustersUpdate")
instance ToJSON NwfabricClustersUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "nwfabricClustersUpdate")

-- | 
data NwfabricHostsUpdate = NwfabricHostsUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON NwfabricHostsUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "nwfabricHostsUpdate")
instance ToJSON NwfabricHostsUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "nwfabricHostsUpdate")

-- | 
data PoolUpdate = PoolUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON PoolUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "poolUpdate")
instance ToJSON PoolUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "poolUpdate")

-- | 
data PoolsCreate = PoolsCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON PoolsCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "poolsCreate")
instance ToJSON PoolsCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "poolsCreate")

-- | 
data PrivateNetworkUpdate = PrivateNetworkUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON PrivateNetworkUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "privateNetworkUpdate")
instance ToJSON PrivateNetworkUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "privateNetworkUpdate")

-- | 
data PrivateNetworksCreate = PrivateNetworksCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON PrivateNetworksCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "privateNetworksCreate")
instance ToJSON PrivateNetworksCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "privateNetworksCreate")

-- | 
data RoutingBGPUpdate = RoutingBGPUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON RoutingBGPUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "routingBGPUpdate")
instance ToJSON RoutingBGPUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "routingBGPUpdate")

-- | 
data RoutingConfigStaticUpdate = RoutingConfigStaticUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON RoutingConfigStaticUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "routingConfigStaticUpdate")
instance ToJSON RoutingConfigStaticUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "routingConfigStaticUpdate")

-- | 
data RoutingConfigUpdate = RoutingConfigUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON RoutingConfigUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "routingConfigUpdate")
instance ToJSON RoutingConfigUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "routingConfigUpdate")

-- | 
data RoutingGlobalConfigUpdate = RoutingGlobalConfigUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON RoutingGlobalConfigUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "routingGlobalConfigUpdate")
instance ToJSON RoutingGlobalConfigUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "routingGlobalConfigUpdate")

-- | 
data RoutingOSPFUpdate = RoutingOSPFUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON RoutingOSPFUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "routingOSPFUpdate")
instance ToJSON RoutingOSPFUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "routingOSPFUpdate")

-- | 
data RuleUpdate = RuleUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON RuleUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "ruleUpdate")
instance ToJSON RuleUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "ruleUpdate")

-- | 
data RulesCreate = RulesCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON RulesCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "rulesCreate")
instance ToJSON RulesCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "rulesCreate")

-- | 
data ScriptCreate = ScriptCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ScriptCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scriptCreate")
instance ToJSON ScriptCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scriptCreate")

-- | 
data ScriptFileIDUpdate = ScriptFileIDUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ScriptFileIDUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scriptFileIDUpdate")
instance ToJSON ScriptFileIDUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scriptFileIDUpdate")

-- | 
data ScriptUpdate = ScriptUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ScriptUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scriptUpdate")
instance ToJSON ScriptUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scriptUpdate")

-- | 
data SecGroupBulkCreate = SecGroupBulkCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SecGroupBulkCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "secGroupBulkCreate")
instance ToJSON SecGroupBulkCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "secGroupBulkCreate")

-- | 
data SecGroupBulkUpdate = SecGroupBulkUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SecGroupBulkUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "secGroupBulkUpdate")
instance ToJSON SecGroupBulkUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "secGroupBulkUpdate")

-- | 
data SecGroupObjectUpdate = SecGroupObjectUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SecGroupObjectUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "secGroupObjectUpdate")
instance ToJSON SecGroupObjectUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "secGroupObjectUpdate")

-- | 
data SecurityFabricCreate = SecurityFabricCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SecurityFabricCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "securityFabricCreate")
instance ToJSON SecurityFabricCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "securityFabricCreate")

-- | 
data SecurityPolicyCreate = SecurityPolicyCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SecurityPolicyCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "securityPolicyCreate")
instance ToJSON SecurityPolicyCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "securityPolicyCreate")

-- | 
data SecurityPolicyIDUpdate = SecurityPolicyIDUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SecurityPolicyIDUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "securityPolicyIDUpdate")
instance ToJSON SecurityPolicyIDUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "securityPolicyIDUpdate")

-- | 
data SecurityTagCreate = SecurityTagCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SecurityTagCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "securityTagCreate")
instance ToJSON SecurityTagCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "securityTagCreate")

-- | 
data ServerSettingsUpdate = ServerSettingsUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ServerSettingsUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "serverSettingsUpdate")
instance ToJSON ServerSettingsUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "serverSettingsUpdate")

-- | 
data ServiceGroupUpdate = ServiceGroupUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ServiceGroupUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "serviceGroupUpdate")
instance ToJSON ServiceGroupUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "serviceGroupUpdate")

-- | 
data ServiceGroupsCreate = ServiceGroupsCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ServiceGroupsCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "serviceGroupsCreate")
instance ToJSON ServiceGroupsCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "serviceGroupsCreate")

-- | 
data ServiceUpdate = ServiceUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ServiceUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "serviceUpdate")
instance ToJSON ServiceUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "serviceUpdate")

-- | 
data ServiceUpgrade = ServiceUpgrade
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ServiceUpgrade where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "serviceUpgrade")
instance ToJSON ServiceUpgrade where
  toJSON = genericToJSON (removeFieldLabelPrefix False "serviceUpgrade")

-- | 
data ServicesScopeCreate = ServicesScopeCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON ServicesScopeCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "servicesScopeCreate")
instance ToJSON ServicesScopeCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "servicesScopeCreate")

-- | 
data SolutionIPPortSet = SolutionIPPortSet
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SolutionIPPortSet where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "solutionIPPortSet")
instance ToJSON SolutionIPPortSet where
  toJSON = genericToJSON (removeFieldLabelPrefix False "solutionIPPortSet")

-- | 
data SpoofGuardPoliciesCreate = SpoofGuardPoliciesCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SpoofGuardPoliciesCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "spoofGuardPoliciesCreate")
instance ToJSON SpoofGuardPoliciesCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "spoofGuardPoliciesCreate")

-- | 
data SpoofGuardPolicyApprove = SpoofGuardPolicyApprove
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SpoofGuardPolicyApprove where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "spoofGuardPolicyApprove")
instance ToJSON SpoofGuardPolicyApprove where
  toJSON = genericToJSON (removeFieldLabelPrefix False "spoofGuardPolicyApprove")

-- | 
data SpoofGuardPolicyUpdate = SpoofGuardPolicyUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SpoofGuardPolicyUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "spoofGuardPolicyUpdate")
instance ToJSON SpoofGuardPolicyUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "spoofGuardPolicyUpdate")

-- | 
data SslVPNUpdate = SslVPNUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SslVPNUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sslVPNUpdate")
instance ToJSON SslVPNUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sslVPNUpdate")

-- | 
data SsoConfig = SsoConfig
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SsoConfig where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "ssoConfig")
instance ToJSON SsoConfig where
  toJSON = genericToJSON (removeFieldLabelPrefix False "ssoConfig")

-- | 
data SyslogUpdate = SyslogUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SyslogUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "syslogUpdate")
instance ToJSON SyslogUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "syslogUpdate")

-- | 
data SystemLocaleUpdate = SystemLocaleUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SystemLocaleUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "systemLocaleUpdate")
instance ToJSON SystemLocaleUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "systemLocaleUpdate")

-- | 
data SystemSyslogServerUpdate = SystemSyslogServerUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SystemSyslogServerUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "systemSyslogServerUpdate")
instance ToJSON SystemSyslogServerUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "systemSyslogServerUpdate")

-- | 
data SystemTimeUpdate = SystemTimeUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON SystemTimeUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "systemTimeUpdate")
instance ToJSON SystemTimeUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "systemTimeUpdate")

-- | 
data TraceflowCreate = TraceflowCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON TraceflowCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "traceflowCreate")
instance ToJSON TraceflowCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "traceflowCreate")

-- | 
data UniversalSyncConfigurationManagersUpdate = UniversalSyncConfigurationManagersUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON UniversalSyncConfigurationManagersUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "universalSyncConfigurationManagersUpdate")
instance ToJSON UniversalSyncConfigurationManagersUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "universalSyncConfigurationManagersUpdate")

-- | 
data UniversalSyncConfigurationNsxManagersCreate = UniversalSyncConfigurationNsxManagersCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON UniversalSyncConfigurationNsxManagersCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "universalSyncConfigurationNsxManagersCreate")
instance ToJSON UniversalSyncConfigurationNsxManagersCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "universalSyncConfigurationNsxManagersCreate")

-- | 
data UserRoleMgmtCreate = UserRoleMgmtCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON UserRoleMgmtCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userRoleMgmtCreate")
instance ToJSON UserRoleMgmtCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userRoleMgmtCreate")

-- | 
data UserRoleMgmtUpdate = UserRoleMgmtUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON UserRoleMgmtUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "userRoleMgmtUpdate")
instance ToJSON UserRoleMgmtUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "userRoleMgmtUpdate")

-- | 
data UsersCreate = UsersCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON UsersCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "usersCreate")
instance ToJSON UsersCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "usersCreate")

-- | 
data UsersUpdate = UsersUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON UsersUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "usersUpdate")
instance ToJSON UsersUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "usersUpdate")

-- | 
data VShieldSolutionActivate = VShieldSolutionActivate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON VShieldSolutionActivate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "vShieldSolutionActivate")
instance ToJSON VShieldSolutionActivate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "vShieldSolutionActivate")

-- | 
data VShieldSolutionCreate = VShieldSolutionCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON VShieldSolutionCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "vShieldSolutionCreate")
instance ToJSON VShieldSolutionCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "vShieldSolutionCreate")

-- | 
data VShieldVendorCreate = VShieldVendorCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON VShieldVendorCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "vShieldVendorCreate")
instance ToJSON VShieldVendorCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "vShieldVendorCreate")

-- | 
data VcConfig = VcConfig
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON VcConfig where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "vcConfig")
instance ToJSON VcConfig where
  toJSON = genericToJSON (removeFieldLabelPrefix False "vcConfig")

-- | 
data VdnMulticast = VdnMulticast
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON VdnMulticast where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "vdnMulticast")
instance ToJSON VdnMulticast where
  toJSON = genericToJSON (removeFieldLabelPrefix False "vdnMulticast")

-- | 
data VdnMulticastUpdate = VdnMulticastUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON VdnMulticastUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "vdnMulticastUpdate")
instance ToJSON VdnMulticastUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "vdnMulticastUpdate")

-- | 
data VdnScopeCreate = VdnScopeCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON VdnScopeCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "vdnScopeCreate")
instance ToJSON VdnScopeCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "vdnScopeCreate")

-- | 
data VdnScopeEdit = VdnScopeEdit
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON VdnScopeEdit where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "vdnScopeEdit")
instance ToJSON VdnScopeEdit where
  toJSON = genericToJSON (removeFieldLabelPrefix False "vdnScopeEdit")

-- | 
data VdnScopeUpdate = VdnScopeUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON VdnScopeUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "vdnScopeUpdate")
instance ToJSON VdnScopeUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "vdnScopeUpdate")

-- | 
data VdnSegment = VdnSegment
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON VdnSegment where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "vdnSegment")
instance ToJSON VdnSegment where
  toJSON = genericToJSON (removeFieldLabelPrefix False "vdnSegment")

-- | 
data VdnSegmentUpdate = VdnSegmentUpdate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON VdnSegmentUpdate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "vdnSegmentUpdate")
instance ToJSON VdnSegmentUpdate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "vdnSegmentUpdate")

-- | 
data VdsContext = VdsContext
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON VdsContext where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "vdsContext")
instance ToJSON VdsContext where
  toJSON = genericToJSON (removeFieldLabelPrefix False "vdsContext")

-- | 
data VirtualServersCreate = VirtualServersCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON VirtualServersCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "virtualServersCreate")
instance ToJSON VirtualServersCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "virtualServersCreate")

-- | 
data WebResourcesCreate = WebResourcesCreate
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON WebResourcesCreate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "webResourcesCreate")
instance ToJSON WebResourcesCreate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "webResourcesCreate")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars}
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
