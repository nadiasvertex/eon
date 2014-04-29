// Code generated by protoc-gen-go.
// source: configuration.proto
// DO NOT EDIT!

/*
Package infinisql_configuration is a generated protocol buffer package.

It is generated from these files:
	configuration.proto

It has these top-level messages:
	ResponseData
	Request
	Response
*/
package configuration

import proto "code.google.com/p/goprotobuf/proto"
import math "math"

// Reference imports to suppress errors if they are not otherwise used.
var _ = proto.Marshal
var _ = math.Inf

type Actor int32

const (
	Actor_ACTOR_NONE                Actor = 0
	Actor_ACTOR_IB_GATEWAY          Actor = 1
	Actor_ACTOR_LISTENER            Actor = 2
	Actor_ACTOR_LOG_PLAYER          Actor = 3
	Actor_ACTOR_OB_GATEWAY          Actor = 4
	Actor_ACTOR_PARTITION_WRITER    Actor = 5
	Actor_ACTOR_TOPOLOGY_MANAGER    Actor = 6
	Actor_ACTOR_TRANSACTION_AGENT   Actor = 7
	Actor_ACTOR_TRANSACTION_LOGGER  Actor = 8
	Actor_ACTOR_USER_SCHEMA_MANAGER Actor = 9
)

var Actor_name = map[int32]string{
	0: "ACTOR_NONE",
	1: "ACTOR_IB_GATEWAY",
	2: "ACTOR_LISTENER",
	3: "ACTOR_LOG_PLAYER",
	4: "ACTOR_OB_GATEWAY",
	5: "ACTOR_PARTITION_WRITER",
	6: "ACTOR_TOPOLOGY_MANAGER",
	7: "ACTOR_TRANSACTION_AGENT",
	8: "ACTOR_TRANSACTION_LOGGER",
	9: "ACTOR_USER_SCHEMA_MANAGER",
}
var Actor_value = map[string]int32{
	"ACTOR_NONE":                0,
	"ACTOR_IB_GATEWAY":          1,
	"ACTOR_LISTENER":            2,
	"ACTOR_LOG_PLAYER":          3,
	"ACTOR_OB_GATEWAY":          4,
	"ACTOR_PARTITION_WRITER":    5,
	"ACTOR_TOPOLOGY_MANAGER":    6,
	"ACTOR_TRANSACTION_AGENT":   7,
	"ACTOR_TRANSACTION_LOGGER":  8,
	"ACTOR_USER_SCHEMA_MANAGER": 9,
}

func (x Actor) Enum() *Actor {
	p := new(Actor)
	*p = x
	return p
}
func (x Actor) String() string {
	return proto.EnumName(Actor_name, int32(x))
}
func (x *Actor) UnmarshalJSON(data []byte) error {
	value, err := proto.UnmarshalJSONEnum(Actor_value, data, "Actor")
	if err != nil {
		return err
	}
	*x = Actor(value)
	return nil
}

type Sync int32

const (
	Sync_SYNC_NONE   Sync = 0
	Sync_SYNC_SYNC   Sync = 1
	Sync_SYNC_ASYNC  Sync = 2
	Sync_SYNC_NOSYNC Sync = 3
)

var Sync_name = map[int32]string{
	0: "SYNC_NONE",
	1: "SYNC_SYNC",
	2: "SYNC_ASYNC",
	3: "SYNC_NOSYNC",
}
var Sync_value = map[string]int32{
	"SYNC_NONE":   0,
	"SYNC_SYNC":   1,
	"SYNC_ASYNC":  2,
	"SYNC_NOSYNC": 3,
}

func (x Sync) Enum() *Sync {
	p := new(Sync)
	*p = x
	return p
}
func (x Sync) String() string {
	return proto.EnumName(Sync_name, int32(x))
}
func (x *Sync) UnmarshalJSON(data []byte) error {
	value, err := proto.UnmarshalJSONEnum(Sync_value, data, "Sync")
	if err != nil {
		return err
	}
	*x = Sync(value)
	return nil
}

type Request_Cmd int32

const (
	Request_CMD_NONE                          Request_Cmd = 0
	Request_CMD_ADD_NODE                      Request_Cmd = 1
	Request_CMD_ADD_PARTITION                 Request_Cmd = 2
	Request_CMD_ADD_PARTITION_GROUP           Request_Cmd = 3
	Request_CMD_CHANGE_MANAGER                Request_Cmd = 4
	Request_CMD_CHANGE_REPLICA                Request_Cmd = 5
	Request_CMD_DELETE_PARTITION              Request_Cmd = 6
	Request_CMD_DELETE_VERSION                Request_Cmd = 7
	Request_CMD_FINISH_AND_ABORT_TRANSACTIONS Request_Cmd = 8
	Request_CMD_FINISH_TRANSACTIONS           Request_Cmd = 9
	Request_CMD_GET_CONFIG                    Request_Cmd = 10
	Request_CMD_NEW_PARTITION                 Request_Cmd = 11
	Request_CMD_NODE_PROBLEM                  Request_Cmd = 12
	Request_CMD_NOTIFY_WHEN_LOG_COMMITTED     Request_Cmd = 13
	Request_CMD_NOTIFY_WHEN_LOG_SENT          Request_Cmd = 14
	Request_CMD_PERSIST_PARTITION             Request_Cmd = 15
	Request_CMD_RELAYOUT                      Request_Cmd = 16
	Request_CMD_REMOVE_NODE                   Request_Cmd = 17
	Request_CMD_REPLICATE_PARTITION           Request_Cmd = 18
	Request_CMD_SET_NODE_TYPE                 Request_Cmd = 19
	Request_CMD_SET_SYNC                      Request_Cmd = 20
	Request_CMD_START                         Request_Cmd = 21
	Request_CMD_START_PARTITION               Request_Cmd = 22
	Request_CMD_START_TRANSACTION_LOG         Request_Cmd = 23
	Request_CMD_START_TRANSACTIONS            Request_Cmd = 24
	Request_CMD_STOP                          Request_Cmd = 25
	Request_CMD_STOP_TRANSACTION_LOG          Request_Cmd = 26
	Request_CMD_STOP_NODE                     Request_Cmd = 27
	Request_CMD_STOP_PARTITION                Request_Cmd = 28
	Request_CMD_STOP_SYNC                     Request_Cmd = 29
	Request_CMD_SYNC_PARTITION                Request_Cmd = 30
)

var Request_Cmd_name = map[int32]string{
	0:  "CMD_NONE",
	1:  "CMD_ADD_NODE",
	2:  "CMD_ADD_PARTITION",
	3:  "CMD_ADD_PARTITION_GROUP",
	4:  "CMD_CHANGE_MANAGER",
	5:  "CMD_CHANGE_REPLICA",
	6:  "CMD_DELETE_PARTITION",
	7:  "CMD_DELETE_VERSION",
	8:  "CMD_FINISH_AND_ABORT_TRANSACTIONS",
	9:  "CMD_FINISH_TRANSACTIONS",
	10: "CMD_GET_CONFIG",
	11: "CMD_NEW_PARTITION",
	12: "CMD_NODE_PROBLEM",
	13: "CMD_NOTIFY_WHEN_LOG_COMMITTED",
	14: "CMD_NOTIFY_WHEN_LOG_SENT",
	15: "CMD_PERSIST_PARTITION",
	16: "CMD_RELAYOUT",
	17: "CMD_REMOVE_NODE",
	18: "CMD_REPLICATE_PARTITION",
	19: "CMD_SET_NODE_TYPE",
	20: "CMD_SET_SYNC",
	21: "CMD_START",
	22: "CMD_START_PARTITION",
	23: "CMD_START_TRANSACTION_LOG",
	24: "CMD_START_TRANSACTIONS",
	25: "CMD_STOP",
	26: "CMD_STOP_TRANSACTION_LOG",
	27: "CMD_STOP_NODE",
	28: "CMD_STOP_PARTITION",
	29: "CMD_STOP_SYNC",
	30: "CMD_SYNC_PARTITION",
}
var Request_Cmd_value = map[string]int32{
	"CMD_NONE":                          0,
	"CMD_ADD_NODE":                      1,
	"CMD_ADD_PARTITION":                 2,
	"CMD_ADD_PARTITION_GROUP":           3,
	"CMD_CHANGE_MANAGER":                4,
	"CMD_CHANGE_REPLICA":                5,
	"CMD_DELETE_PARTITION":              6,
	"CMD_DELETE_VERSION":                7,
	"CMD_FINISH_AND_ABORT_TRANSACTIONS": 8,
	"CMD_FINISH_TRANSACTIONS":           9,
	"CMD_GET_CONFIG":                    10,
	"CMD_NEW_PARTITION":                 11,
	"CMD_NODE_PROBLEM":                  12,
	"CMD_NOTIFY_WHEN_LOG_COMMITTED":     13,
	"CMD_NOTIFY_WHEN_LOG_SENT":          14,
	"CMD_PERSIST_PARTITION":             15,
	"CMD_RELAYOUT":                      16,
	"CMD_REMOVE_NODE":                   17,
	"CMD_REPLICATE_PARTITION":           18,
	"CMD_SET_NODE_TYPE":                 19,
	"CMD_SET_SYNC":                      20,
	"CMD_START":                         21,
	"CMD_START_PARTITION":               22,
	"CMD_START_TRANSACTION_LOG":         23,
	"CMD_START_TRANSACTIONS":            24,
	"CMD_STOP":                          25,
	"CMD_STOP_TRANSACTION_LOG":          26,
	"CMD_STOP_NODE":                     27,
	"CMD_STOP_PARTITION":                28,
	"CMD_STOP_SYNC":                     29,
	"CMD_SYNC_PARTITION":                30,
}

func (x Request_Cmd) Enum() *Request_Cmd {
	p := new(Request_Cmd)
	*p = x
	return p
}
func (x Request_Cmd) String() string {
	return proto.EnumName(Request_Cmd_name, int32(x))
}
func (x *Request_Cmd) UnmarshalJSON(data []byte) error {
	value, err := proto.UnmarshalJSONEnum(Request_Cmd_value, data, "Request_Cmd")
	if err != nil {
		return err
	}
	*x = Request_Cmd(value)
	return nil
}

type Response_Status int32

const (
	Response_STATUS_NONE Response_Status = 0
	Response_STATUS_OK   Response_Status = 1
	Response_STATUS_NOK  Response_Status = 2
)

var Response_Status_name = map[int32]string{
	0: "STATUS_NONE",
	1: "STATUS_OK",
	2: "STATUS_NOK",
}
var Response_Status_value = map[string]int32{
	"STATUS_NONE": 0,
	"STATUS_OK":   1,
	"STATUS_NOK":  2,
}

func (x Response_Status) Enum() *Response_Status {
	p := new(Response_Status)
	*p = x
	return p
}
func (x Response_Status) String() string {
	return proto.EnumName(Response_Status_name, int32(x))
}
func (x *Response_Status) UnmarshalJSON(data []byte) error {
	value, err := proto.UnmarshalJSONEnum(Response_Status_value, data, "Response_Status")
	if err != nil {
		return err
	}
	*x = Response_Status(value)
	return nil
}

type Response_Reason int32

const (
	Response_REASON_NONE            Response_Reason = 0
	Response_REASON_GENERIC         Response_Reason = 1
	Response_REASON_UNKNOWN_REQUEST Response_Reason = 2
	Response_REASON_NOT_READY       Response_Reason = 3
)

var Response_Reason_name = map[int32]string{
	0: "REASON_NONE",
	1: "REASON_GENERIC",
	2: "REASON_UNKNOWN_REQUEST",
	3: "REASON_NOT_READY",
}
var Response_Reason_value = map[string]int32{
	"REASON_NONE":            0,
	"REASON_GENERIC":         1,
	"REASON_UNKNOWN_REQUEST": 2,
	"REASON_NOT_READY":       3,
}

func (x Response_Reason) Enum() *Response_Reason {
	p := new(Response_Reason)
	*p = x
	return p
}
func (x Response_Reason) String() string {
	return proto.EnumName(Response_Reason_name, int32(x))
}
func (x *Response_Reason) UnmarshalJSON(data []byte) error {
	value, err := proto.UnmarshalJSONEnum(Response_Reason_value, data, "Response_Reason")
	if err != nil {
		return err
	}
	*x = Response_Reason(value)
	return nil
}

// messages
type ResponseData struct {
	Somesuch         *int32 `protobuf:"varint,1,req,name=somesuch" json:"somesuch,omitempty"`
	XXX_unrecognized []byte `json:"-"`
}

func (m *ResponseData) Reset()         { *m = ResponseData{} }
func (m *ResponseData) String() string { return proto.CompactTextString(m) }
func (*ResponseData) ProtoMessage()    {}

func (m *ResponseData) GetSomesuch() int32 {
	if m != nil && m.Somesuch != nil {
		return *m.Somesuch
	}
	return 0
}

type Request struct {
	Cmd                *Request_Cmd `protobuf:"varint,1,req,name=cmd,enum=infinisql.configuration.Request_Cmd" json:"cmd,omitempty"`
	CommandId          *int32       `protobuf:"varint,27,req,name=command_id" json:"command_id,omitempty"`
	Actor              *Actor       `protobuf:"varint,2,opt,name=actor,enum=infinisql.configuration.Actor" json:"actor,omitempty"`
	Dbs                *int32       `protobuf:"varint,3,opt,name=dbs" json:"dbs,omitempty"`
	DestNodeId         *int32       `protobuf:"varint,4,opt,name=dest_node_id" json:"dest_node_id,omitempty"`
	DestPartitionId    *int32       `protobuf:"varint,5,opt,name=dest_partition_id" json:"dest_partition_id,omitempty"`
	PartitionGroupId   *int32       `protobuf:"varint,6,opt,name=partition_group_id" json:"partition_group_id,omitempty"`
	Instance           *int32       `protobuf:"varint,7,opt,name=instance" json:"instance,omitempty"`
	LogPlayerId        *int32       `protobuf:"varint,8,opt,name=log_player_id" json:"log_player_id,omitempty"`
	MemberId           *int32       `protobuf:"varint,9,opt,name=member_id" json:"member_id,omitempty"`
	NodeId             *int32       `protobuf:"varint,10,opt,name=node_id" json:"node_id,omitempty"`
	NodeIds            []int32      `protobuf:"varint,11,rep,name=node_ids" json:"node_ids,omitempty"`
	PartitionId        *int32       `protobuf:"varint,12,opt,name=partition_id" json:"partition_id,omitempty"`
	Readers            *int32       `protobuf:"varint,13,opt,name=readers" json:"readers,omitempty"`
	ReplicaId          *int32       `protobuf:"varint,14,opt,name=replica_id" json:"replica_id,omitempty"`
	SegmentId          *int32       `protobuf:"varint,15,opt,name=segment_id" json:"segment_id,omitempty"`
	Size               *int64       `protobuf:"varint,16,opt,name=size" json:"size,omitempty"`
	SrcPartitionId     *int32       `protobuf:"varint,17,opt,name=src_partition_id" json:"src_partition_id,omitempty"`
	VersionId          *int32       `protobuf:"varint,18,opt,name=version_id" json:"version_id,omitempty"`
	Host               *string      `protobuf:"bytes,19,req,name=host" json:"host,omitempty"`
	PartitionGroupName *string      `protobuf:"bytes,20,req,name=partition_group_name" json:"partition_group_name,omitempty"`
	Path               *string      `protobuf:"bytes,21,req,name=path" json:"path,omitempty"`
	Port               *string      `protobuf:"bytes,22,req,name=port" json:"port,omitempty"`
	IsActive           *bool        `protobuf:"varint,23,opt,name=is_active" json:"is_active,omitempty"`
	IsSync             *bool        `protobuf:"varint,24,opt,name=is_sync" json:"is_sync,omitempty"`
	IsTrue             *bool        `protobuf:"varint,25,opt,name=is_true" json:"is_true,omitempty"`
	Sync               *Sync        `protobuf:"varint,26,req,name=sync,enum=infinisql.configuration.Sync" json:"sync,omitempty"`
	XXX_unrecognized   []byte       `json:"-"`
}

func (m *Request) Reset()         { *m = Request{} }
func (m *Request) String() string { return proto.CompactTextString(m) }
func (*Request) ProtoMessage()    {}

func (m *Request) GetCmd() Request_Cmd {
	if m != nil && m.Cmd != nil {
		return *m.Cmd
	}
	return Request_CMD_NONE
}

func (m *Request) GetCommandId() int32 {
	if m != nil && m.CommandId != nil {
		return *m.CommandId
	}
	return 0
}

func (m *Request) GetActor() Actor {
	if m != nil && m.Actor != nil {
		return *m.Actor
	}
	return Actor_ACTOR_NONE
}

func (m *Request) GetDbs() int32 {
	if m != nil && m.Dbs != nil {
		return *m.Dbs
	}
	return 0
}

func (m *Request) GetDestNodeId() int32 {
	if m != nil && m.DestNodeId != nil {
		return *m.DestNodeId
	}
	return 0
}

func (m *Request) GetDestPartitionId() int32 {
	if m != nil && m.DestPartitionId != nil {
		return *m.DestPartitionId
	}
	return 0
}

func (m *Request) GetPartitionGroupId() int32 {
	if m != nil && m.PartitionGroupId != nil {
		return *m.PartitionGroupId
	}
	return 0
}

func (m *Request) GetInstance() int32 {
	if m != nil && m.Instance != nil {
		return *m.Instance
	}
	return 0
}

func (m *Request) GetLogPlayerId() int32 {
	if m != nil && m.LogPlayerId != nil {
		return *m.LogPlayerId
	}
	return 0
}

func (m *Request) GetMemberId() int32 {
	if m != nil && m.MemberId != nil {
		return *m.MemberId
	}
	return 0
}

func (m *Request) GetNodeId() int32 {
	if m != nil && m.NodeId != nil {
		return *m.NodeId
	}
	return 0
}

func (m *Request) GetNodeIds() []int32 {
	if m != nil {
		return m.NodeIds
	}
	return nil
}

func (m *Request) GetPartitionId() int32 {
	if m != nil && m.PartitionId != nil {
		return *m.PartitionId
	}
	return 0
}

func (m *Request) GetReaders() int32 {
	if m != nil && m.Readers != nil {
		return *m.Readers
	}
	return 0
}

func (m *Request) GetReplicaId() int32 {
	if m != nil && m.ReplicaId != nil {
		return *m.ReplicaId
	}
	return 0
}

func (m *Request) GetSegmentId() int32 {
	if m != nil && m.SegmentId != nil {
		return *m.SegmentId
	}
	return 0
}

func (m *Request) GetSize() int64 {
	if m != nil && m.Size != nil {
		return *m.Size
	}
	return 0
}

func (m *Request) GetSrcPartitionId() int32 {
	if m != nil && m.SrcPartitionId != nil {
		return *m.SrcPartitionId
	}
	return 0
}

func (m *Request) GetVersionId() int32 {
	if m != nil && m.VersionId != nil {
		return *m.VersionId
	}
	return 0
}

func (m *Request) GetHost() string {
	if m != nil && m.Host != nil {
		return *m.Host
	}
	return ""
}

func (m *Request) GetPartitionGroupName() string {
	if m != nil && m.PartitionGroupName != nil {
		return *m.PartitionGroupName
	}
	return ""
}

func (m *Request) GetPath() string {
	if m != nil && m.Path != nil {
		return *m.Path
	}
	return ""
}

func (m *Request) GetPort() string {
	if m != nil && m.Port != nil {
		return *m.Port
	}
	return ""
}

func (m *Request) GetIsActive() bool {
	if m != nil && m.IsActive != nil {
		return *m.IsActive
	}
	return false
}

func (m *Request) GetIsSync() bool {
	if m != nil && m.IsSync != nil {
		return *m.IsSync
	}
	return false
}

func (m *Request) GetIsTrue() bool {
	if m != nil && m.IsTrue != nil {
		return *m.IsTrue
	}
	return false
}

func (m *Request) GetSync() Sync {
	if m != nil && m.Sync != nil {
		return *m.Sync
	}
	return Sync_SYNC_NONE
}

type Response struct {
	Status           *Response_Status `protobuf:"varint,1,opt,name=status,enum=infinisql.configuration.Response_Status" json:"status,omitempty"`
	CommandId        *int32           `protobuf:"varint,2,opt,name=command_id" json:"command_id,omitempty"`
	Data             *ResponseData    `protobuf:"bytes,3,opt,name=data" json:"data,omitempty"`
	Reason           *Response_Reason `protobuf:"varint,4,opt,name=reason,enum=infinisql.configuration.Response_Reason" json:"reason,omitempty"`
	XXX_unrecognized []byte           `json:"-"`
}

func (m *Response) Reset()         { *m = Response{} }
func (m *Response) String() string { return proto.CompactTextString(m) }
func (*Response) ProtoMessage()    {}

func (m *Response) GetStatus() Response_Status {
	if m != nil && m.Status != nil {
		return *m.Status
	}
	return Response_STATUS_NONE
}

func (m *Response) GetCommandId() int32 {
	if m != nil && m.CommandId != nil {
		return *m.CommandId
	}
	return 0
}

func (m *Response) GetData() *ResponseData {
	if m != nil {
		return m.Data
	}
	return nil
}

func (m *Response) GetReason() Response_Reason {
	if m != nil && m.Reason != nil {
		return *m.Reason
	}
	return Response_REASON_NONE
}

func init() {
	proto.RegisterEnum("infinisql.configuration.Actor", Actor_name, Actor_value)
	proto.RegisterEnum("infinisql.configuration.Sync", Sync_name, Sync_value)
	proto.RegisterEnum("infinisql.configuration.Request_Cmd", Request_Cmd_name, Request_Cmd_value)
	proto.RegisterEnum("infinisql.configuration.Response_Status", Response_Status_name, Response_Status_value)
	proto.RegisterEnum("infinisql.configuration.Response_Reason", Response_Reason_name, Response_Reason_value)
}
