??1
??
D
AddV2
x"T
y"T
z"T"
Ttype:
2	??
B
AssignVariableOp
resource
value"dtype"
dtypetype?
~
BiasAdd

value"T	
bias"T
output"T" 
Ttype:
2	"-
data_formatstringNHWC:
NHWCNCHW
8
Const
output"dtype"
valuetensor"
dtypetype
.
Identity

input"T
output"T"	
Ttype
q
MatMul
a"T
b"T
product"T"
transpose_abool( "
transpose_bbool( "
Ttype:

2	
>
Maximum
x"T
y"T
z"T"
Ttype:
2	
e
MergeV2Checkpoints
checkpoint_prefixes
destination_prefix"
delete_old_dirsbool(?
>
Minimum
x"T
y"T
z"T"
Ttype:
2	
?
Mul
x"T
y"T
z"T"
Ttype:
2	?

NoOp
M
Pack
values"T*N
output"T"
Nint(0"	
Ttype"
axisint 
C
Placeholder
output"dtype"
dtypetype"
shapeshape:
@
ReadVariableOp
resource
value"dtype"
dtypetype?
[
Reshape
tensor"T
shape"Tshape
output"T"	
Ttype"
Tshapetype0:
2	
o
	RestoreV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
l
SaveV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
?
Select
	condition

t"T
e"T
output"T"	
Ttype
H
ShardedFilename
basename	
shard

num_shards
filename
[
Split
	split_dim

value"T
output"T*	num_split"
	num_splitint(0"	
Ttype
?
StatefulPartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring ?
@
StaticRegexFullMatch	
input

output
"
patternstring
?
StridedSlice

input"T
begin"Index
end"Index
strides"Index
output"T"	
Ttype"
Indextype:
2	"

begin_maskint "
end_maskint "
ellipsis_maskint "
new_axis_maskint "
shrink_axis_maskint 
N

StringJoin
inputs*N

output"
Nint(0"
	separatorstring 
-
Tanh
x"T
y"T"
Ttype:

2
?
TensorListFromTensor
tensor"element_dtype
element_shape"
shape_type*
output_handle??element_dtype"
element_dtypetype"

shape_typetype:
2	
?
TensorListReserve
element_shape"
shape_type
num_elements#
handle??element_dtype"
element_dtypetype"

shape_typetype:
2	
?
TensorListStack
input_handle
element_shape
tensor"element_dtype"
element_dtypetype" 
num_elementsint?????????
P
	Transpose
x"T
perm"Tperm
y"T"	
Ttype"
Tpermtype0:
2	
?
VarHandleOp
resource"
	containerstring "
shared_namestring "
dtypetype"
shapeshape"#
allowed_deviceslist(string)
 ?
?
While

input2T
output2T"
T
list(type)("
condfunc"
bodyfunc" 
output_shapeslist(shape)
 "
parallel_iterationsint
?"serve*2.6.02v2.6.0-rc2-32-g919f693420e8??0
?
lstm_7/lstm_cell_9/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?**
shared_namelstm_7/lstm_cell_9/kernel
?
-lstm_7/lstm_cell_9/kernel/Read/ReadVariableOpReadVariableOplstm_7/lstm_cell_9/kernel*
_output_shapes
:	?*
dtype0
?
#lstm_7/lstm_cell_9/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2?*4
shared_name%#lstm_7/lstm_cell_9/recurrent_kernel
?
7lstm_7/lstm_cell_9/recurrent_kernel/Read/ReadVariableOpReadVariableOp#lstm_7/lstm_cell_9/recurrent_kernel*
_output_shapes
:	2?*
dtype0
?
lstm_7/lstm_cell_9/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*(
shared_namelstm_7/lstm_cell_9/bias
?
+lstm_7/lstm_cell_9/bias/Read/ReadVariableOpReadVariableOplstm_7/lstm_cell_9/bias*
_output_shapes	
:?*
dtype0
?
lstm_6/lstm_cell_8/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2?**
shared_namelstm_6/lstm_cell_8/kernel
?
-lstm_6/lstm_cell_8/kernel/Read/ReadVariableOpReadVariableOplstm_6/lstm_cell_8/kernel*
_output_shapes
:	2?*
dtype0
?
#lstm_6/lstm_cell_8/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2?*4
shared_name%#lstm_6/lstm_cell_8/recurrent_kernel
?
7lstm_6/lstm_cell_8/recurrent_kernel/Read/ReadVariableOpReadVariableOp#lstm_6/lstm_cell_8/recurrent_kernel*
_output_shapes
:	2?*
dtype0
?
lstm_6/lstm_cell_8/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*(
shared_namelstm_6/lstm_cell_8/bias
?
+lstm_6/lstm_cell_8/bias/Read/ReadVariableOpReadVariableOplstm_6/lstm_cell_8/bias*
_output_shapes	
:?*
dtype0
?
time_distributed_3/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:2**
shared_nametime_distributed_3/kernel
?
-time_distributed_3/kernel/Read/ReadVariableOpReadVariableOptime_distributed_3/kernel*
_output_shapes

:2*
dtype0
?
time_distributed_3/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*(
shared_nametime_distributed_3/bias

+time_distributed_3/bias/Read/ReadVariableOpReadVariableOptime_distributed_3/bias*
_output_shapes
:*
dtype0
z
lstm_7/VariableVarHandleOp*
_output_shapes
: *
dtype0*
shape
:2* 
shared_namelstm_7/Variable
s
#lstm_7/Variable/Read/ReadVariableOpReadVariableOplstm_7/Variable*
_output_shapes

:2*
dtype0
~
lstm_7/Variable_1VarHandleOp*
_output_shapes
: *
dtype0*
shape
:2*"
shared_namelstm_7/Variable_1
w
%lstm_7/Variable_1/Read/ReadVariableOpReadVariableOplstm_7/Variable_1*
_output_shapes

:2*
dtype0
z
lstm_6/VariableVarHandleOp*
_output_shapes
: *
dtype0*
shape
:2* 
shared_namelstm_6/Variable
s
#lstm_6/Variable/Read/ReadVariableOpReadVariableOplstm_6/Variable*
_output_shapes

:2*
dtype0
~
lstm_6/Variable_1VarHandleOp*
_output_shapes
: *
dtype0*
shape
:2*"
shared_namelstm_6/Variable_1
w
%lstm_6/Variable_1/Read/ReadVariableOpReadVariableOplstm_6/Variable_1*
_output_shapes

:2*
dtype0
^
totalVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nametotal
W
total/Read/ReadVariableOpReadVariableOptotal*
_output_shapes
: *
dtype0
^
countVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namecount
W
count/Read/ReadVariableOpReadVariableOpcount*
_output_shapes
: *
dtype0
b
total_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	total_1
[
total_1/Read/ReadVariableOpReadVariableOptotal_1*
_output_shapes
: *
dtype0
b
count_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name	count_1
[
count_1/Read/ReadVariableOpReadVariableOpcount_1*
_output_shapes
: *
dtype0

NoOpNoOp
?#
ConstConst"/device:CPU:0*
_output_shapes
: *
dtype0*?"
value?"B?" B?"
?
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
layer_with_weights-2
layer-2
	optimizer
	variables
trainable_variables
regularization_losses
	keras_api
	
signatures
l

cell

state_spec
	variables
trainable_variables
regularization_losses
	keras_api
l
cell

state_spec
	variables
trainable_variables
regularization_losses
	keras_api
]
	layer
	variables
trainable_variables
regularization_losses
	keras_api
 
8
0
1
2
3
4
 5
!6
"7
8
0
1
2
3
4
 5
!6
"7
 
?
#layer_regularization_losses
	variables
$non_trainable_variables
trainable_variables

%layers
&layer_metrics
regularization_losses
'metrics
 
?
(
state_size

kernel
recurrent_kernel
bias
)	variables
*trainable_variables
+regularization_losses
,	keras_api
 

0
1
2

0
1
2
 
?
-layer_regularization_losses
	variables
.non_trainable_variables
trainable_variables

/layers
0layer_metrics

1states
regularization_losses
2metrics
?
3
state_size

kernel
recurrent_kernel
 bias
4	variables
5trainable_variables
6regularization_losses
7	keras_api
 

0
1
 2

0
1
 2
 
?
8layer_regularization_losses
	variables
9non_trainable_variables
trainable_variables

:layers
;layer_metrics

<states
regularization_losses
=metrics
h

!kernel
"bias
>	variables
?trainable_variables
@regularization_losses
A	keras_api

!0
"1

!0
"1
 
?
Blayer_regularization_losses
Cnon_trainable_variables
	variables
trainable_variables

Dlayers
Elayer_metrics
regularization_losses
Fmetrics
US
VARIABLE_VALUElstm_7/lstm_cell_9/kernel&variables/0/.ATTRIBUTES/VARIABLE_VALUE
_]
VARIABLE_VALUE#lstm_7/lstm_cell_9/recurrent_kernel&variables/1/.ATTRIBUTES/VARIABLE_VALUE
SQ
VARIABLE_VALUElstm_7/lstm_cell_9/bias&variables/2/.ATTRIBUTES/VARIABLE_VALUE
US
VARIABLE_VALUElstm_6/lstm_cell_8/kernel&variables/3/.ATTRIBUTES/VARIABLE_VALUE
_]
VARIABLE_VALUE#lstm_6/lstm_cell_8/recurrent_kernel&variables/4/.ATTRIBUTES/VARIABLE_VALUE
SQ
VARIABLE_VALUElstm_6/lstm_cell_8/bias&variables/5/.ATTRIBUTES/VARIABLE_VALUE
US
VARIABLE_VALUEtime_distributed_3/kernel&variables/6/.ATTRIBUTES/VARIABLE_VALUE
SQ
VARIABLE_VALUEtime_distributed_3/bias&variables/7/.ATTRIBUTES/VARIABLE_VALUE
 
 

0
1
2
 

G0
H1
 

0
1
2

0
1
2
 
?
Ilayer_regularization_losses
Jnon_trainable_variables
)	variables
*trainable_variables

Klayers
Llayer_metrics
+regularization_losses
Mmetrics
 
 


0
 

N0
O1
 
 

0
1
 2

0
1
 2
 
?
Player_regularization_losses
Qnon_trainable_variables
4	variables
5trainable_variables

Rlayers
Slayer_metrics
6regularization_losses
Tmetrics
 
 

0
 

U0
V1
 

!0
"1

!0
"1
 
?
Wlayer_regularization_losses
Xnon_trainable_variables
>	variables
?trainable_variables

Ylayers
Zlayer_metrics
@regularization_losses
[metrics
 
 

0
 
 
4
	\total
	]count
^	variables
_	keras_api
D
	`total
	acount
b
_fn_kwargs
c	variables
d	keras_api
 
 
 
 
 
ge
VARIABLE_VALUElstm_7/VariableBlayer_with_weights-0/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUE
ig
VARIABLE_VALUElstm_7/Variable_1Blayer_with_weights-0/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUE
 
 
 
 
 
ge
VARIABLE_VALUElstm_6/VariableBlayer_with_weights-1/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUE
ig
VARIABLE_VALUElstm_6/Variable_1Blayer_with_weights-1/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUE
 
 
 
 
 
OM
VARIABLE_VALUEtotal4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUE
OM
VARIABLE_VALUEcount4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUE

\0
]1

^	variables
QO
VARIABLE_VALUEtotal_14keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUE
QO
VARIABLE_VALUEcount_14keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUE
 

`0
a1

c	variables
u
serving_default_lstm_7_inputPlaceholder*"
_output_shapes
:
*
dtype0*
shape:

?
StatefulPartitionedCallStatefulPartitionedCallserving_default_lstm_7_inputlstm_7/lstm_cell_9/kernellstm_7/Variable#lstm_7/lstm_cell_9/recurrent_kernellstm_7/lstm_cell_9/biaslstm_7/Variable_1lstm_6/lstm_cell_8/kernellstm_6/Variable#lstm_6/lstm_cell_8/recurrent_kernellstm_6/lstm_cell_8/biaslstm_6/Variable_1time_distributed_3/kerneltime_distributed_3/bias*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8? *,
f'R%
#__inference_signature_wrapper_46517
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
?
StatefulPartitionedCall_1StatefulPartitionedCallsaver_filename-lstm_7/lstm_cell_9/kernel/Read/ReadVariableOp7lstm_7/lstm_cell_9/recurrent_kernel/Read/ReadVariableOp+lstm_7/lstm_cell_9/bias/Read/ReadVariableOp-lstm_6/lstm_cell_8/kernel/Read/ReadVariableOp7lstm_6/lstm_cell_8/recurrent_kernel/Read/ReadVariableOp+lstm_6/lstm_cell_8/bias/Read/ReadVariableOp-time_distributed_3/kernel/Read/ReadVariableOp+time_distributed_3/bias/Read/ReadVariableOp#lstm_7/Variable/Read/ReadVariableOp%lstm_7/Variable_1/Read/ReadVariableOp#lstm_6/Variable/Read/ReadVariableOp%lstm_6/Variable_1/Read/ReadVariableOptotal/Read/ReadVariableOpcount/Read/ReadVariableOptotal_1/Read/ReadVariableOpcount_1/Read/ReadVariableOpConst*
Tin
2*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *'
f"R 
__inference__traced_save_49847
?
StatefulPartitionedCall_2StatefulPartitionedCallsaver_filenamelstm_7/lstm_cell_9/kernel#lstm_7/lstm_cell_9/recurrent_kernellstm_7/lstm_cell_9/biaslstm_6/lstm_cell_8/kernel#lstm_6/lstm_cell_8/recurrent_kernellstm_6/lstm_cell_8/biastime_distributed_3/kerneltime_distributed_3/biaslstm_7/Variablelstm_7/Variable_1lstm_6/Variablelstm_6/Variable_1totalcounttotal_1count_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? **
f%R#
!__inference__traced_restore_49905Ѭ/
?X
?
while_body_45686
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_8_matmul_readvariableop_resource_0:	2?G
4while_lstm_cell_8_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_8_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_8_matmul_readvariableop_resource:	2?E
2while_lstm_cell_8_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_8_biasadd_readvariableop_resource:	???(while/lstm_cell_8/BiasAdd/ReadVariableOp?'while/lstm_cell_8/MatMul/ReadVariableOp?)while/lstm_cell_8/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_8/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_8_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02)
'while/lstm_cell_8/MatMul/ReadVariableOp?
while/lstm_cell_8/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/MatMul?
)while/lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_8_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_8/MatMul_1/ReadVariableOp?
while/lstm_cell_8/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_8/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/MatMul_1?
while/lstm_cell_8/addAddV2"while/lstm_cell_8/MatMul:product:0$while/lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/add?
(while/lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_8_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_8/BiasAdd/ReadVariableOp?
while/lstm_cell_8/BiasAddBiasAddwhile/lstm_cell_8/add:z:00while/lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/BiasAdd?
!while/lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_8/split/split_dim?
while/lstm_cell_8/splitSplit*while/lstm_cell_8/split/split_dim:output:0"while/lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_8/splitw
while/lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const{
while/lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_1?
while/lstm_cell_8/MulMul while/lstm_cell_8/split:output:0 while/lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul?
while/lstm_cell_8/Add_1AddV2while/lstm_cell_8/Mul:z:0"while/lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_1?
)while/lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_8/clip_by_value/Minimum/y?
'while/lstm_cell_8/clip_by_value/MinimumMinimumwhile/lstm_cell_8/Add_1:z:02while/lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22)
'while/lstm_cell_8/clip_by_value/Minimum?
!while/lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_8/clip_by_value/y?
while/lstm_cell_8/clip_by_valueMaximum+while/lstm_cell_8/clip_by_value/Minimum:z:0*while/lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22!
while/lstm_cell_8/clip_by_value{
while/lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const_2{
while/lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_3?
while/lstm_cell_8/Mul_1Mul while/lstm_cell_8/split:output:1"while/lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul_1?
while/lstm_cell_8/Add_2AddV2while/lstm_cell_8/Mul_1:z:0"while/lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_2?
+while/lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_8/clip_by_value_1/Minimum/y?
)while/lstm_cell_8/clip_by_value_1/MinimumMinimumwhile/lstm_cell_8/Add_2:z:04while/lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_8/clip_by_value_1/Minimum?
#while/lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_8/clip_by_value_1/y?
!while/lstm_cell_8/clip_by_value_1Maximum-while/lstm_cell_8/clip_by_value_1/Minimum:z:0,while/lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_8/clip_by_value_1?
while/lstm_cell_8/mul_2Mul%while/lstm_cell_8/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_2?
while/lstm_cell_8/TanhTanh while/lstm_cell_8/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_8/Tanh?
while/lstm_cell_8/mul_3Mul#while/lstm_cell_8/clip_by_value:z:0while/lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_3?
while/lstm_cell_8/add_3AddV2while/lstm_cell_8/mul_2:z:0while/lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_8/add_3{
while/lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const_4{
while/lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_5?
while/lstm_cell_8/Mul_4Mul while/lstm_cell_8/split:output:3"while/lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul_4?
while/lstm_cell_8/Add_4AddV2while/lstm_cell_8/Mul_4:z:0"while/lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_4?
+while/lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_8/clip_by_value_2/Minimum/y?
)while/lstm_cell_8/clip_by_value_2/MinimumMinimumwhile/lstm_cell_8/Add_4:z:04while/lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_8/clip_by_value_2/Minimum?
#while/lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_8/clip_by_value_2/y?
!while/lstm_cell_8/clip_by_value_2Maximum-while/lstm_cell_8/clip_by_value_2/Minimum:z:0,while/lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_8/clip_by_value_2?
while/lstm_cell_8/Tanh_1Tanhwhile/lstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Tanh_1?
while/lstm_cell_8/mul_5Mul%while/lstm_cell_8/clip_by_value_2:z:0while/lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_8/mul_5:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_8/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_8/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_8/BiasAdd/ReadVariableOp(^while/lstm_cell_8/MatMul/ReadVariableOp*^while/lstm_cell_8/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"h
1while_lstm_cell_8_biasadd_readvariableop_resource3while_lstm_cell_8_biasadd_readvariableop_resource_0"j
2while_lstm_cell_8_matmul_1_readvariableop_resource4while_lstm_cell_8_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_8_matmul_readvariableop_resource2while_lstm_cell_8_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2T
(while/lstm_cell_8/BiasAdd/ReadVariableOp(while/lstm_cell_8/BiasAdd/ReadVariableOp2R
'while/lstm_cell_8/MatMul/ReadVariableOp'while/lstm_cell_8/MatMul/ReadVariableOp2V
)while/lstm_cell_8/MatMul_1/ReadVariableOp)while/lstm_cell_8/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?m
?
A__inference_lstm_7_layer_call_and_return_conditional_losses_47659
inputs_0=
*lstm_cell_9_matmul_readvariableop_resource:	?>
,lstm_cell_9_matmul_1_readvariableop_resource:2A
.lstm_cell_9_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_9_biasadd_readvariableop_resource:	?;
)lstm_cell_9_mul_2_readvariableop_resource:2
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_9/BiasAdd/ReadVariableOp?!lstm_cell_9/MatMul/ReadVariableOp?#lstm_cell_9/MatMul_1/ReadVariableOp?%lstm_cell_9/MatMul_1/ReadVariableOp_1? lstm_cell_9/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:?????????2
	transposeK
ShapeShapetranspose:y:0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_9/MatMul/ReadVariableOpReadVariableOp*lstm_cell_9_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02#
!lstm_cell_9/MatMul/ReadVariableOp?
lstm_cell_9/MatMulMatMulstrided_slice_1:output:0)lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/MatMul?
#lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02%
#lstm_cell_9/MatMul_1/ReadVariableOp?
%lstm_cell_9/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_9_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_9/MatMul_1/ReadVariableOp_1?
lstm_cell_9/MatMul_1MatMul+lstm_cell_9/MatMul_1/ReadVariableOp:value:0-lstm_cell_9/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/MatMul_1?
lstm_cell_9/addAddV2lstm_cell_9/MatMul:product:0lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_cell_9/add?
"lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_9_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_9/BiasAdd/ReadVariableOp?
lstm_cell_9/BiasAddBiasAddlstm_cell_9/add:z:0*lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/BiasAdd|
lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_9/split/split_dim?
lstm_cell_9/splitSplit$lstm_cell_9/split/split_dim:output:0lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_9/splitk
lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Consto
lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_1?
lstm_cell_9/MulMullstm_cell_9/split:output:0lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul?
lstm_cell_9/Add_1AddV2lstm_cell_9/Mul:z:0lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_1?
#lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_9/clip_by_value/Minimum/y?
!lstm_cell_9/clip_by_value/MinimumMinimumlstm_cell_9/Add_1:z:0,lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22#
!lstm_cell_9/clip_by_value/Minimum
lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value/y?
lstm_cell_9/clip_by_valueMaximum%lstm_cell_9/clip_by_value/Minimum:z:0$lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_valueo
lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Const_2o
lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_3?
lstm_cell_9/Mul_1Mullstm_cell_9/split:output:1lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul_1?
lstm_cell_9/Add_2AddV2lstm_cell_9/Mul_1:z:0lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_2?
%lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_9/clip_by_value_1/Minimum/y?
#lstm_cell_9/clip_by_value_1/MinimumMinimumlstm_cell_9/Add_2:z:0.lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_9/clip_by_value_1/Minimum?
lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value_1/y?
lstm_cell_9/clip_by_value_1Maximum'lstm_cell_9/clip_by_value_1/Minimum:z:0&lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_value_1?
 lstm_cell_9/mul_2/ReadVariableOpReadVariableOp)lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02"
 lstm_cell_9/mul_2/ReadVariableOp?
lstm_cell_9/mul_2Mullstm_cell_9/clip_by_value_1:z:0(lstm_cell_9/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_2q
lstm_cell_9/TanhTanhlstm_cell_9/split:output:2*
T0*
_output_shapes

:22
lstm_cell_9/Tanh?
lstm_cell_9/mul_3Mullstm_cell_9/clip_by_value:z:0lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_3?
lstm_cell_9/add_3AddV2lstm_cell_9/mul_2:z:0lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_9/add_3o
lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Const_4o
lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_5?
lstm_cell_9/Mul_4Mullstm_cell_9/split:output:3lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul_4?
lstm_cell_9/Add_4AddV2lstm_cell_9/Mul_4:z:0lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_4?
%lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_9/clip_by_value_2/Minimum/y?
#lstm_cell_9/clip_by_value_2/MinimumMinimumlstm_cell_9/Add_4:z:0.lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_9/clip_by_value_2/Minimum?
lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value_2/y?
lstm_cell_9/clip_by_value_2Maximum'lstm_cell_9/clip_by_value_2/Minimum:z:0&lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_value_2p
lstm_cell_9/Tanh_1Tanhlstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_9/Tanh_1?
lstm_cell_9/mul_5Mullstm_cell_9/clip_by_value_2:z:0lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time?
ReadVariableOpReadVariableOp,lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_9_matmul_readvariableop_resource.lstm_cell_9_matmul_1_readvariableop_1_resource+lstm_cell_9_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_47554*
condR
while_cond_47553*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_9_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_9/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_9_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_9/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_9/BiasAdd/ReadVariableOp"^lstm_cell_9/MatMul/ReadVariableOp$^lstm_cell_9/MatMul_1/ReadVariableOp&^lstm_cell_9/MatMul_1/ReadVariableOp_1!^lstm_cell_9/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:?????????: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_9/BiasAdd/ReadVariableOp"lstm_cell_9/BiasAdd/ReadVariableOp2F
!lstm_cell_9/MatMul/ReadVariableOp!lstm_cell_9/MatMul/ReadVariableOp2J
#lstm_cell_9/MatMul_1/ReadVariableOp#lstm_cell_9/MatMul_1/ReadVariableOp2N
%lstm_cell_9/MatMul_1/ReadVariableOp_1%lstm_cell_9/MatMul_1/ReadVariableOp_12D
 lstm_cell_9/mul_2/ReadVariableOp lstm_cell_9/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:?????????
"
_user_specified_name
inputs/0
?X
?
while_body_48504
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_8_matmul_readvariableop_resource_0:	2?G
4while_lstm_cell_8_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_8_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_8_matmul_readvariableop_resource:	2?E
2while_lstm_cell_8_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_8_biasadd_readvariableop_resource:	???(while/lstm_cell_8/BiasAdd/ReadVariableOp?'while/lstm_cell_8/MatMul/ReadVariableOp?)while/lstm_cell_8/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_8/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_8_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02)
'while/lstm_cell_8/MatMul/ReadVariableOp?
while/lstm_cell_8/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/MatMul?
)while/lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_8_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_8/MatMul_1/ReadVariableOp?
while/lstm_cell_8/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_8/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/MatMul_1?
while/lstm_cell_8/addAddV2"while/lstm_cell_8/MatMul:product:0$while/lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/add?
(while/lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_8_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_8/BiasAdd/ReadVariableOp?
while/lstm_cell_8/BiasAddBiasAddwhile/lstm_cell_8/add:z:00while/lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/BiasAdd?
!while/lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_8/split/split_dim?
while/lstm_cell_8/splitSplit*while/lstm_cell_8/split/split_dim:output:0"while/lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_8/splitw
while/lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const{
while/lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_1?
while/lstm_cell_8/MulMul while/lstm_cell_8/split:output:0 while/lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul?
while/lstm_cell_8/Add_1AddV2while/lstm_cell_8/Mul:z:0"while/lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_1?
)while/lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_8/clip_by_value/Minimum/y?
'while/lstm_cell_8/clip_by_value/MinimumMinimumwhile/lstm_cell_8/Add_1:z:02while/lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22)
'while/lstm_cell_8/clip_by_value/Minimum?
!while/lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_8/clip_by_value/y?
while/lstm_cell_8/clip_by_valueMaximum+while/lstm_cell_8/clip_by_value/Minimum:z:0*while/lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22!
while/lstm_cell_8/clip_by_value{
while/lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const_2{
while/lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_3?
while/lstm_cell_8/Mul_1Mul while/lstm_cell_8/split:output:1"while/lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul_1?
while/lstm_cell_8/Add_2AddV2while/lstm_cell_8/Mul_1:z:0"while/lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_2?
+while/lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_8/clip_by_value_1/Minimum/y?
)while/lstm_cell_8/clip_by_value_1/MinimumMinimumwhile/lstm_cell_8/Add_2:z:04while/lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_8/clip_by_value_1/Minimum?
#while/lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_8/clip_by_value_1/y?
!while/lstm_cell_8/clip_by_value_1Maximum-while/lstm_cell_8/clip_by_value_1/Minimum:z:0,while/lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_8/clip_by_value_1?
while/lstm_cell_8/mul_2Mul%while/lstm_cell_8/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_2?
while/lstm_cell_8/TanhTanh while/lstm_cell_8/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_8/Tanh?
while/lstm_cell_8/mul_3Mul#while/lstm_cell_8/clip_by_value:z:0while/lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_3?
while/lstm_cell_8/add_3AddV2while/lstm_cell_8/mul_2:z:0while/lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_8/add_3{
while/lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const_4{
while/lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_5?
while/lstm_cell_8/Mul_4Mul while/lstm_cell_8/split:output:3"while/lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul_4?
while/lstm_cell_8/Add_4AddV2while/lstm_cell_8/Mul_4:z:0"while/lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_4?
+while/lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_8/clip_by_value_2/Minimum/y?
)while/lstm_cell_8/clip_by_value_2/MinimumMinimumwhile/lstm_cell_8/Add_4:z:04while/lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_8/clip_by_value_2/Minimum?
#while/lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_8/clip_by_value_2/y?
!while/lstm_cell_8/clip_by_value_2Maximum-while/lstm_cell_8/clip_by_value_2/Minimum:z:0,while/lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_8/clip_by_value_2?
while/lstm_cell_8/Tanh_1Tanhwhile/lstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Tanh_1?
while/lstm_cell_8/mul_5Mul%while/lstm_cell_8/clip_by_value_2:z:0while/lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_8/mul_5:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_8/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_8/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_8/BiasAdd/ReadVariableOp(^while/lstm_cell_8/MatMul/ReadVariableOp*^while/lstm_cell_8/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"h
1while_lstm_cell_8_biasadd_readvariableop_resource3while_lstm_cell_8_biasadd_readvariableop_resource_0"j
2while_lstm_cell_8_matmul_1_readvariableop_resource4while_lstm_cell_8_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_8_matmul_readvariableop_resource2while_lstm_cell_8_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2T
(while/lstm_cell_8/BiasAdd/ReadVariableOp(while/lstm_cell_8/BiasAdd/ReadVariableOp2R
'while/lstm_cell_8/MatMul/ReadVariableOp'while/lstm_cell_8/MatMul/ReadVariableOp2V
)while/lstm_cell_8/MatMul_1/ReadVariableOp)while/lstm_cell_8/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?	
?
lstm_7_while_cond_46589*
&lstm_7_while_lstm_7_while_loop_counter0
,lstm_7_while_lstm_7_while_maximum_iterations
lstm_7_while_placeholder
lstm_7_while_placeholder_1
lstm_7_while_placeholder_2
lstm_7_while_placeholder_3*
&lstm_7_while_less_lstm_7_strided_sliceA
=lstm_7_while_lstm_7_while_cond_46589___redundant_placeholder0A
=lstm_7_while_lstm_7_while_cond_46589___redundant_placeholder1A
=lstm_7_while_lstm_7_while_cond_46589___redundant_placeholder2A
=lstm_7_while_lstm_7_while_cond_46589___redundant_placeholder3
lstm_7_while_identity
?
lstm_7/while/LessLesslstm_7_while_placeholder&lstm_7_while_less_lstm_7_strided_slice*
T0*
_output_shapes
: 2
lstm_7/while/Lessr
lstm_7/while/IdentityIdentitylstm_7/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_7/while/Identity"7
lstm_7_while_identitylstm_7/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?
?
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_48903

inputs8
&dense_3_matmul_readvariableop_resource:25
'dense_3_biasadd_readvariableop_resource:
identity??dense_3/BiasAdd/ReadVariableOp?dense_3/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2
Reshape/shapef
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes

:
22	
Reshape?
dense_3/MatMul/ReadVariableOpReadVariableOp&dense_3_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_3/MatMul/ReadVariableOp?
dense_3/MatMulMatMulReshape:output:0%dense_3/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_3/MatMul?
dense_3/BiasAdd/ReadVariableOpReadVariableOp'dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_3/BiasAdd/ReadVariableOp?
dense_3/BiasAddBiasAdddense_3/MatMul:product:0&dense_3/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_3/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      2
Reshape_1/shape?
	Reshape_1Reshapedense_3/BiasAdd:output:0Reshape_1/shape:output:0*
T0*"
_output_shapes
:
2
	Reshape_1h
IdentityIdentityReshape_1:output:0^NoOp*
T0*"
_output_shapes
:
2

Identity?
NoOpNoOp^dense_3/BiasAdd/ReadVariableOp^dense_3/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:
2: : 2@
dense_3/BiasAdd/ReadVariableOpdense_3/BiasAdd/ReadVariableOp2>
dense_3/MatMul/ReadVariableOpdense_3/MatMul/ReadVariableOp:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
?

?
B__inference_dense_3_layer_call_and_return_conditional_losses_49767

inputs0
matmul_readvariableop_resource:2-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:2*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2	
BiasAddk
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identity
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????2: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????2
 
_user_specified_nameinputs
?e
?

lstm_6_while_body_47128*
&lstm_6_while_lstm_6_while_loop_counter0
,lstm_6_while_lstm_6_while_maximum_iterations
lstm_6_while_placeholder
lstm_6_while_placeholder_1
lstm_6_while_placeholder_2
lstm_6_while_placeholder_3'
#lstm_6_while_lstm_6_strided_slice_0e
alstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor_0L
9lstm_6_while_lstm_cell_8_matmul_readvariableop_resource_0:	2?N
;lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource_0:	2?I
:lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource_0:	?
lstm_6_while_identity
lstm_6_while_identity_1
lstm_6_while_identity_2
lstm_6_while_identity_3
lstm_6_while_identity_4
lstm_6_while_identity_5%
!lstm_6_while_lstm_6_strided_slicec
_lstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensorJ
7lstm_6_while_lstm_cell_8_matmul_readvariableop_resource:	2?L
9lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource:	2?G
8lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource:	???/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp?.lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp?0lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp?
>lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2@
>lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shape?
0lstm_6/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemalstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor_0lstm_6_while_placeholderGlstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype022
0lstm_6/while/TensorArrayV2Read/TensorListGetItem?
.lstm_6/while/lstm_cell_8/MatMul/ReadVariableOpReadVariableOp9lstm_6_while_lstm_cell_8_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype020
.lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp?
lstm_6/while/lstm_cell_8/MatMulMatMul7lstm_6/while/TensorArrayV2Read/TensorListGetItem:item:06lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2!
lstm_6/while/lstm_cell_8/MatMul?
0lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp;lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype022
0lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp?
!lstm_6/while/lstm_cell_8/MatMul_1MatMullstm_6_while_placeholder_28lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2#
!lstm_6/while/lstm_cell_8/MatMul_1?
lstm_6/while/lstm_cell_8/addAddV2)lstm_6/while/lstm_cell_8/MatMul:product:0+lstm_6/while/lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_6/while/lstm_cell_8/add?
/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp:lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype021
/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp?
 lstm_6/while/lstm_cell_8/BiasAddBiasAdd lstm_6/while/lstm_cell_8/add:z:07lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2"
 lstm_6/while/lstm_cell_8/BiasAdd?
(lstm_6/while/lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2*
(lstm_6/while/lstm_cell_8/split/split_dim?
lstm_6/while/lstm_cell_8/splitSplit1lstm_6/while/lstm_cell_8/split/split_dim:output:0)lstm_6/while/lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2 
lstm_6/while/lstm_cell_8/split?
lstm_6/while/lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2 
lstm_6/while/lstm_cell_8/Const?
 lstm_6/while/lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_6/while/lstm_cell_8/Const_1?
lstm_6/while/lstm_cell_8/MulMul'lstm_6/while/lstm_cell_8/split:output:0'lstm_6/while/lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
lstm_6/while/lstm_cell_8/Mul?
lstm_6/while/lstm_cell_8/Add_1AddV2 lstm_6/while/lstm_cell_8/Mul:z:0)lstm_6/while/lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/Add_1?
0lstm_6/while/lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??22
0lstm_6/while/lstm_cell_8/clip_by_value/Minimum/y?
.lstm_6/while/lstm_cell_8/clip_by_value/MinimumMinimum"lstm_6/while/lstm_cell_8/Add_1:z:09lstm_6/while/lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:220
.lstm_6/while/lstm_cell_8/clip_by_value/Minimum?
(lstm_6/while/lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2*
(lstm_6/while/lstm_cell_8/clip_by_value/y?
&lstm_6/while/lstm_cell_8/clip_by_valueMaximum2lstm_6/while/lstm_cell_8/clip_by_value/Minimum:z:01lstm_6/while/lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22(
&lstm_6/while/lstm_cell_8/clip_by_value?
 lstm_6/while/lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_6/while/lstm_cell_8/Const_2?
 lstm_6/while/lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_6/while/lstm_cell_8/Const_3?
lstm_6/while/lstm_cell_8/Mul_1Mul'lstm_6/while/lstm_cell_8/split:output:1)lstm_6/while/lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/Mul_1?
lstm_6/while/lstm_cell_8/Add_2AddV2"lstm_6/while/lstm_cell_8/Mul_1:z:0)lstm_6/while/lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/Add_2?
2lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum/y?
0lstm_6/while/lstm_cell_8/clip_by_value_1/MinimumMinimum"lstm_6/while/lstm_cell_8/Add_2:z:0;lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222
0lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum?
*lstm_6/while/lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_6/while/lstm_cell_8/clip_by_value_1/y?
(lstm_6/while/lstm_cell_8/clip_by_value_1Maximum4lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum:z:03lstm_6/while/lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22*
(lstm_6/while/lstm_cell_8/clip_by_value_1?
lstm_6/while/lstm_cell_8/mul_2Mul,lstm_6/while/lstm_cell_8/clip_by_value_1:z:0lstm_6_while_placeholder_3*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/mul_2?
lstm_6/while/lstm_cell_8/TanhTanh'lstm_6/while/lstm_cell_8/split:output:2*
T0*
_output_shapes

:22
lstm_6/while/lstm_cell_8/Tanh?
lstm_6/while/lstm_cell_8/mul_3Mul*lstm_6/while/lstm_cell_8/clip_by_value:z:0!lstm_6/while/lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/mul_3?
lstm_6/while/lstm_cell_8/add_3AddV2"lstm_6/while/lstm_cell_8/mul_2:z:0"lstm_6/while/lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/add_3?
 lstm_6/while/lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_6/while/lstm_cell_8/Const_4?
 lstm_6/while/lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_6/while/lstm_cell_8/Const_5?
lstm_6/while/lstm_cell_8/Mul_4Mul'lstm_6/while/lstm_cell_8/split:output:3)lstm_6/while/lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/Mul_4?
lstm_6/while/lstm_cell_8/Add_4AddV2"lstm_6/while/lstm_cell_8/Mul_4:z:0)lstm_6/while/lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/Add_4?
2lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum/y?
0lstm_6/while/lstm_cell_8/clip_by_value_2/MinimumMinimum"lstm_6/while/lstm_cell_8/Add_4:z:0;lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222
0lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum?
*lstm_6/while/lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_6/while/lstm_cell_8/clip_by_value_2/y?
(lstm_6/while/lstm_cell_8/clip_by_value_2Maximum4lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum:z:03lstm_6/while/lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22*
(lstm_6/while/lstm_cell_8/clip_by_value_2?
lstm_6/while/lstm_cell_8/Tanh_1Tanh"lstm_6/while/lstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22!
lstm_6/while/lstm_cell_8/Tanh_1?
lstm_6/while/lstm_cell_8/mul_5Mul,lstm_6/while/lstm_cell_8/clip_by_value_2:z:0#lstm_6/while/lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/mul_5?
1lstm_6/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_6_while_placeholder_1lstm_6_while_placeholder"lstm_6/while/lstm_cell_8/mul_5:z:0*
_output_shapes
: *
element_dtype023
1lstm_6/while/TensorArrayV2Write/TensorListSetItemj
lstm_6/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/while/add/y?
lstm_6/while/addAddV2lstm_6_while_placeholderlstm_6/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_6/while/addn
lstm_6/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/while/add_1/y?
lstm_6/while/add_1AddV2&lstm_6_while_lstm_6_while_loop_counterlstm_6/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_6/while/add_1?
lstm_6/while/IdentityIdentitylstm_6/while/add_1:z:0^lstm_6/while/NoOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity?
lstm_6/while/Identity_1Identity,lstm_6_while_lstm_6_while_maximum_iterations^lstm_6/while/NoOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity_1?
lstm_6/while/Identity_2Identitylstm_6/while/add:z:0^lstm_6/while/NoOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity_2?
lstm_6/while/Identity_3IdentityAlstm_6/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_6/while/NoOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity_3?
lstm_6/while/Identity_4Identity"lstm_6/while/lstm_cell_8/mul_5:z:0^lstm_6/while/NoOp*
T0*
_output_shapes

:22
lstm_6/while/Identity_4?
lstm_6/while/Identity_5Identity"lstm_6/while/lstm_cell_8/add_3:z:0^lstm_6/while/NoOp*
T0*
_output_shapes

:22
lstm_6/while/Identity_5?
lstm_6/while/NoOpNoOp0^lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp/^lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp1^lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_6/while/NoOp"7
lstm_6_while_identitylstm_6/while/Identity:output:0";
lstm_6_while_identity_1 lstm_6/while/Identity_1:output:0";
lstm_6_while_identity_2 lstm_6/while/Identity_2:output:0";
lstm_6_while_identity_3 lstm_6/while/Identity_3:output:0";
lstm_6_while_identity_4 lstm_6/while/Identity_4:output:0";
lstm_6_while_identity_5 lstm_6/while/Identity_5:output:0"H
!lstm_6_while_lstm_6_strided_slice#lstm_6_while_lstm_6_strided_slice_0"v
8lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource:lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource_0"x
9lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource;lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource_0"t
7lstm_6_while_lstm_cell_8_matmul_readvariableop_resource9lstm_6_while_lstm_cell_8_matmul_readvariableop_resource_0"?
_lstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensoralstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2b
/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp2`
.lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp.lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp2d
0lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp0lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?
?
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_45314

inputs
dense_3_45304:2
dense_3_45306:
identity??dense_3/StatefulPartitionedCallD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_sliceo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2
Reshape/shapeo
ReshapeReshapeinputsReshape/shape:output:0*
T0*'
_output_shapes
:?????????22	
Reshape?
dense_3/StatefulPartitionedCallStatefulPartitionedCallReshape:output:0dense_3_45304dense_3_45306*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_3_layer_call_and_return_conditional_losses_453032!
dense_3/StatefulPartitionedCallq
Reshape_1/shape/0Const*
_output_shapes
: *
dtype0*
valueB :
?????????2
Reshape_1/shape/0h
Reshape_1/shape/2Const*
_output_shapes
: *
dtype0*
value	B :2
Reshape_1/shape/2?
Reshape_1/shapePackReshape_1/shape/0:output:0strided_slice:output:0Reshape_1/shape/2:output:0*
N*
T0*
_output_shapes
:2
Reshape_1/shape?
	Reshape_1Reshape(dense_3/StatefulPartitionedCall:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :??????????????????2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :??????????????????2

Identityp
NoOpNoOp ^dense_3/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:??????????????????2: : 2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall:\ X
4
_output_shapes"
 :??????????????????2
 
_user_specified_nameinputs
?
?
G__inference_sequential_3_layer_call_and_return_conditional_losses_45825

inputs
lstm_7_45603:	?
lstm_7_45605:2
lstm_7_45607:	2?
lstm_7_45609:	?
lstm_7_45611:2
lstm_6_45792:	2?
lstm_6_45794:2
lstm_6_45796:	2?
lstm_6_45798:	?
lstm_6_45800:2*
time_distributed_3_45817:2&
time_distributed_3_45819:
identity??lstm_6/StatefulPartitionedCall?lstm_7/StatefulPartitionedCall?*time_distributed_3/StatefulPartitionedCall?
lstm_7/StatefulPartitionedCallStatefulPartitionedCallinputslstm_7_45603lstm_7_45605lstm_7_45607lstm_7_45609lstm_7_45611*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_7_layer_call_and_return_conditional_losses_456022 
lstm_7/StatefulPartitionedCall?
lstm_6/StatefulPartitionedCallStatefulPartitionedCall'lstm_7/StatefulPartitionedCall:output:0lstm_6_45792lstm_6_45794lstm_6_45796lstm_6_45798lstm_6_45800*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_6_layer_call_and_return_conditional_losses_457912 
lstm_6/StatefulPartitionedCall?
*time_distributed_3/StatefulPartitionedCallStatefulPartitionedCall'lstm_6/StatefulPartitionedCall:output:0time_distributed_3_45817time_distributed_3_45819*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_458162,
*time_distributed_3/StatefulPartitionedCall?
 time_distributed_3/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2"
 time_distributed_3/Reshape/shape?
time_distributed_3/ReshapeReshape'lstm_6/StatefulPartitionedCall:output:0)time_distributed_3/Reshape/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_3/Reshape?
IdentityIdentity3time_distributed_3/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

Identity?
NoOpNoOp^lstm_6/StatefulPartitionedCall^lstm_7/StatefulPartitionedCall+^time_distributed_3/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 2@
lstm_6/StatefulPartitionedCalllstm_6/StatefulPartitionedCall2@
lstm_7/StatefulPartitionedCalllstm_7/StatefulPartitionedCall2X
*time_distributed_3/StatefulPartitionedCall*time_distributed_3/StatefulPartitionedCall:J F
"
_output_shapes
:

 
_user_specified_nameinputs
?
?
2__inference_time_distributed_3_layer_call_fn_48944

inputs
unknown:2
	unknown_0:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_458162
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:
2: : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
?.
?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49575

inputs
states_0
states_11
matmul_readvariableop_resource:	2?5
"matmul_1_readvariableop_1_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?MatMul_1/ReadVariableOp_1?mul_2/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMulu
MatMul_1/ReadVariableOpReadVariableOpstates_0*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp?
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOp_1?
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpi
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*$
_output_shapes
::::*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1T
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes
:2
MulU
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes
:2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes
:2
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3Z
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes
:2
Mul_1W
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes
:2
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1o
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes
:*
dtype02
mul_2/ReadVariableOpk
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes
:2
mul_2G
TanhTanhsplit:output:2*
T0*
_output_shapes
:2
TanhU
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes
:2
mul_3P
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes
:2
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5Z
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes
:2
Mul_4W
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes
:2
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2F
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes
:2
Tanh_1Y
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes
:2
mul_5U
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

IdentityY

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

Identity_1Y

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes
:2

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^MatMul_1/ReadVariableOp_1^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:2:2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
?
?
,__inference_sequential_3_layer_call_fn_47274

inputs
unknown:	?
	unknown_0:2
	unknown_1:	2?
	unknown_2:	?
	unknown_3:2
	unknown_4:	2?
	unknown_5:2
	unknown_6:	2?
	unknown_7:	?
	unknown_8:2
	unknown_9:2

unknown_10:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_sequential_3_layer_call_and_return_conditional_losses_458252
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:

 
_user_specified_nameinputs
??
?
 __inference__wrapped_model_43727
lstm_7_inputQ
>sequential_3_lstm_7_lstm_cell_9_matmul_readvariableop_resource:	?R
@sequential_3_lstm_7_lstm_cell_9_matmul_1_readvariableop_resource:2U
Bsequential_3_lstm_7_lstm_cell_9_matmul_1_readvariableop_1_resource:	2?N
?sequential_3_lstm_7_lstm_cell_9_biasadd_readvariableop_resource:	?O
=sequential_3_lstm_7_lstm_cell_9_mul_2_readvariableop_resource:2Q
>sequential_3_lstm_6_lstm_cell_8_matmul_readvariableop_resource:	2?R
@sequential_3_lstm_6_lstm_cell_8_matmul_1_readvariableop_resource:2U
Bsequential_3_lstm_6_lstm_cell_8_matmul_1_readvariableop_1_resource:	2?N
?sequential_3_lstm_6_lstm_cell_8_biasadd_readvariableop_resource:	?O
=sequential_3_lstm_6_lstm_cell_8_mul_2_readvariableop_resource:2X
Fsequential_3_time_distributed_3_dense_3_matmul_readvariableop_resource:2U
Gsequential_3_time_distributed_3_dense_3_biasadd_readvariableop_resource:
identity??$sequential_3/lstm_6/AssignVariableOp?&sequential_3/lstm_6/AssignVariableOp_1?"sequential_3/lstm_6/ReadVariableOp?$sequential_3/lstm_6/ReadVariableOp_1?6sequential_3/lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp?5sequential_3/lstm_6/lstm_cell_8/MatMul/ReadVariableOp?7sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp?9sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1?4sequential_3/lstm_6/lstm_cell_8/mul_2/ReadVariableOp?sequential_3/lstm_6/while?$sequential_3/lstm_7/AssignVariableOp?&sequential_3/lstm_7/AssignVariableOp_1?"sequential_3/lstm_7/ReadVariableOp?$sequential_3/lstm_7/ReadVariableOp_1?6sequential_3/lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp?5sequential_3/lstm_7/lstm_cell_9/MatMul/ReadVariableOp?7sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp?9sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1?4sequential_3/lstm_7/lstm_cell_9/mul_2/ReadVariableOp?sequential_3/lstm_7/while?>sequential_3/time_distributed_3/dense_3/BiasAdd/ReadVariableOp?=sequential_3/time_distributed_3/dense_3/MatMul/ReadVariableOp?
"sequential_3/lstm_7/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2$
"sequential_3/lstm_7/transpose/perm?
sequential_3/lstm_7/transpose	Transposelstm_7_input+sequential_3/lstm_7/transpose/perm:output:0*
T0*"
_output_shapes
:
2
sequential_3/lstm_7/transpose?
sequential_3/lstm_7/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
         2
sequential_3/lstm_7/Shape?
'sequential_3/lstm_7/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2)
'sequential_3/lstm_7/strided_slice/stack?
)sequential_3/lstm_7/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2+
)sequential_3/lstm_7/strided_slice/stack_1?
)sequential_3/lstm_7/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2+
)sequential_3/lstm_7/strided_slice/stack_2?
!sequential_3/lstm_7/strided_sliceStridedSlice"sequential_3/lstm_7/Shape:output:00sequential_3/lstm_7/strided_slice/stack:output:02sequential_3/lstm_7/strided_slice/stack_1:output:02sequential_3/lstm_7/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2#
!sequential_3/lstm_7/strided_slice?
/sequential_3/lstm_7/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????21
/sequential_3/lstm_7/TensorArrayV2/element_shape?
!sequential_3/lstm_7/TensorArrayV2TensorListReserve8sequential_3/lstm_7/TensorArrayV2/element_shape:output:0*sequential_3/lstm_7/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02#
!sequential_3/lstm_7/TensorArrayV2?
Isequential_3/lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      2K
Isequential_3/lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shape?
;sequential_3/lstm_7/TensorArrayUnstack/TensorListFromTensorTensorListFromTensor!sequential_3/lstm_7/transpose:y:0Rsequential_3/lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02=
;sequential_3/lstm_7/TensorArrayUnstack/TensorListFromTensor?
)sequential_3/lstm_7/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2+
)sequential_3/lstm_7/strided_slice_1/stack?
+sequential_3/lstm_7/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_3/lstm_7/strided_slice_1/stack_1?
+sequential_3/lstm_7/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_3/lstm_7/strided_slice_1/stack_2?
#sequential_3/lstm_7/strided_slice_1StridedSlice!sequential_3/lstm_7/transpose:y:02sequential_3/lstm_7/strided_slice_1/stack:output:04sequential_3/lstm_7/strided_slice_1/stack_1:output:04sequential_3/lstm_7/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2%
#sequential_3/lstm_7/strided_slice_1?
5sequential_3/lstm_7/lstm_cell_9/MatMul/ReadVariableOpReadVariableOp>sequential_3_lstm_7_lstm_cell_9_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype027
5sequential_3/lstm_7/lstm_cell_9/MatMul/ReadVariableOp?
&sequential_3/lstm_7/lstm_cell_9/MatMulMatMul,sequential_3/lstm_7/strided_slice_1:output:0=sequential_3/lstm_7/lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2(
&sequential_3/lstm_7/lstm_cell_9/MatMul?
7sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp@sequential_3_lstm_7_lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype029
7sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp?
9sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1ReadVariableOpBsequential_3_lstm_7_lstm_cell_9_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02;
9sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1?
(sequential_3/lstm_7/lstm_cell_9/MatMul_1MatMul?sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp:value:0Asequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2*
(sequential_3/lstm_7/lstm_cell_9/MatMul_1?
#sequential_3/lstm_7/lstm_cell_9/addAddV20sequential_3/lstm_7/lstm_cell_9/MatMul:product:02sequential_3/lstm_7/lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2%
#sequential_3/lstm_7/lstm_cell_9/add?
6sequential_3/lstm_7/lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp?sequential_3_lstm_7_lstm_cell_9_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype028
6sequential_3/lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp?
'sequential_3/lstm_7/lstm_cell_9/BiasAddBiasAdd'sequential_3/lstm_7/lstm_cell_9/add:z:0>sequential_3/lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2)
'sequential_3/lstm_7/lstm_cell_9/BiasAdd?
/sequential_3/lstm_7/lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :21
/sequential_3/lstm_7/lstm_cell_9/split/split_dim?
%sequential_3/lstm_7/lstm_cell_9/splitSplit8sequential_3/lstm_7/lstm_cell_9/split/split_dim:output:00sequential_3/lstm_7/lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2'
%sequential_3/lstm_7/lstm_cell_9/split?
%sequential_3/lstm_7/lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2'
%sequential_3/lstm_7/lstm_cell_9/Const?
'sequential_3/lstm_7/lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2)
'sequential_3/lstm_7/lstm_cell_9/Const_1?
#sequential_3/lstm_7/lstm_cell_9/MulMul.sequential_3/lstm_7/lstm_cell_9/split:output:0.sequential_3/lstm_7/lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22%
#sequential_3/lstm_7/lstm_cell_9/Mul?
%sequential_3/lstm_7/lstm_cell_9/Add_1AddV2'sequential_3/lstm_7/lstm_cell_9/Mul:z:00sequential_3/lstm_7/lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_7/lstm_cell_9/Add_1?
7sequential_3/lstm_7/lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??29
7sequential_3/lstm_7/lstm_cell_9/clip_by_value/Minimum/y?
5sequential_3/lstm_7/lstm_cell_9/clip_by_value/MinimumMinimum)sequential_3/lstm_7/lstm_cell_9/Add_1:z:0@sequential_3/lstm_7/lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:227
5sequential_3/lstm_7/lstm_cell_9/clip_by_value/Minimum?
/sequential_3/lstm_7/lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    21
/sequential_3/lstm_7/lstm_cell_9/clip_by_value/y?
-sequential_3/lstm_7/lstm_cell_9/clip_by_valueMaximum9sequential_3/lstm_7/lstm_cell_9/clip_by_value/Minimum:z:08sequential_3/lstm_7/lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22/
-sequential_3/lstm_7/lstm_cell_9/clip_by_value?
'sequential_3/lstm_7/lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2)
'sequential_3/lstm_7/lstm_cell_9/Const_2?
'sequential_3/lstm_7/lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2)
'sequential_3/lstm_7/lstm_cell_9/Const_3?
%sequential_3/lstm_7/lstm_cell_9/Mul_1Mul.sequential_3/lstm_7/lstm_cell_9/split:output:10sequential_3/lstm_7/lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_7/lstm_cell_9/Mul_1?
%sequential_3/lstm_7/lstm_cell_9/Add_2AddV2)sequential_3/lstm_7/lstm_cell_9/Mul_1:z:00sequential_3/lstm_7/lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_7/lstm_cell_9/Add_2?
9sequential_3/lstm_7/lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2;
9sequential_3/lstm_7/lstm_cell_9/clip_by_value_1/Minimum/y?
7sequential_3/lstm_7/lstm_cell_9/clip_by_value_1/MinimumMinimum)sequential_3/lstm_7/lstm_cell_9/Add_2:z:0Bsequential_3/lstm_7/lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:229
7sequential_3/lstm_7/lstm_cell_9/clip_by_value_1/Minimum?
1sequential_3/lstm_7/lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    23
1sequential_3/lstm_7/lstm_cell_9/clip_by_value_1/y?
/sequential_3/lstm_7/lstm_cell_9/clip_by_value_1Maximum;sequential_3/lstm_7/lstm_cell_9/clip_by_value_1/Minimum:z:0:sequential_3/lstm_7/lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:221
/sequential_3/lstm_7/lstm_cell_9/clip_by_value_1?
4sequential_3/lstm_7/lstm_cell_9/mul_2/ReadVariableOpReadVariableOp=sequential_3_lstm_7_lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype026
4sequential_3/lstm_7/lstm_cell_9/mul_2/ReadVariableOp?
%sequential_3/lstm_7/lstm_cell_9/mul_2Mul3sequential_3/lstm_7/lstm_cell_9/clip_by_value_1:z:0<sequential_3/lstm_7/lstm_cell_9/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_7/lstm_cell_9/mul_2?
$sequential_3/lstm_7/lstm_cell_9/TanhTanh.sequential_3/lstm_7/lstm_cell_9/split:output:2*
T0*
_output_shapes

:22&
$sequential_3/lstm_7/lstm_cell_9/Tanh?
%sequential_3/lstm_7/lstm_cell_9/mul_3Mul1sequential_3/lstm_7/lstm_cell_9/clip_by_value:z:0(sequential_3/lstm_7/lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_7/lstm_cell_9/mul_3?
%sequential_3/lstm_7/lstm_cell_9/add_3AddV2)sequential_3/lstm_7/lstm_cell_9/mul_2:z:0)sequential_3/lstm_7/lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_7/lstm_cell_9/add_3?
'sequential_3/lstm_7/lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2)
'sequential_3/lstm_7/lstm_cell_9/Const_4?
'sequential_3/lstm_7/lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2)
'sequential_3/lstm_7/lstm_cell_9/Const_5?
%sequential_3/lstm_7/lstm_cell_9/Mul_4Mul.sequential_3/lstm_7/lstm_cell_9/split:output:30sequential_3/lstm_7/lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_7/lstm_cell_9/Mul_4?
%sequential_3/lstm_7/lstm_cell_9/Add_4AddV2)sequential_3/lstm_7/lstm_cell_9/Mul_4:z:00sequential_3/lstm_7/lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_7/lstm_cell_9/Add_4?
9sequential_3/lstm_7/lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2;
9sequential_3/lstm_7/lstm_cell_9/clip_by_value_2/Minimum/y?
7sequential_3/lstm_7/lstm_cell_9/clip_by_value_2/MinimumMinimum)sequential_3/lstm_7/lstm_cell_9/Add_4:z:0Bsequential_3/lstm_7/lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:229
7sequential_3/lstm_7/lstm_cell_9/clip_by_value_2/Minimum?
1sequential_3/lstm_7/lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    23
1sequential_3/lstm_7/lstm_cell_9/clip_by_value_2/y?
/sequential_3/lstm_7/lstm_cell_9/clip_by_value_2Maximum;sequential_3/lstm_7/lstm_cell_9/clip_by_value_2/Minimum:z:0:sequential_3/lstm_7/lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:221
/sequential_3/lstm_7/lstm_cell_9/clip_by_value_2?
&sequential_3/lstm_7/lstm_cell_9/Tanh_1Tanh)sequential_3/lstm_7/lstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22(
&sequential_3/lstm_7/lstm_cell_9/Tanh_1?
%sequential_3/lstm_7/lstm_cell_9/mul_5Mul3sequential_3/lstm_7/lstm_cell_9/clip_by_value_2:z:0*sequential_3/lstm_7/lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_7/lstm_cell_9/mul_5?
1sequential_3/lstm_7/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   23
1sequential_3/lstm_7/TensorArrayV2_1/element_shape?
#sequential_3/lstm_7/TensorArrayV2_1TensorListReserve:sequential_3/lstm_7/TensorArrayV2_1/element_shape:output:0*sequential_3/lstm_7/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02%
#sequential_3/lstm_7/TensorArrayV2_1v
sequential_3/lstm_7/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential_3/lstm_7/time?
"sequential_3/lstm_7/ReadVariableOpReadVariableOp@sequential_3_lstm_7_lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02$
"sequential_3/lstm_7/ReadVariableOp?
$sequential_3/lstm_7/ReadVariableOp_1ReadVariableOp=sequential_3_lstm_7_lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02&
$sequential_3/lstm_7/ReadVariableOp_1?
,sequential_3/lstm_7/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2.
,sequential_3/lstm_7/while/maximum_iterations?
&sequential_3/lstm_7/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2(
&sequential_3/lstm_7/while/loop_counter?
sequential_3/lstm_7/whileWhile/sequential_3/lstm_7/while/loop_counter:output:05sequential_3/lstm_7/while/maximum_iterations:output:0!sequential_3/lstm_7/time:output:0,sequential_3/lstm_7/TensorArrayV2_1:handle:0*sequential_3/lstm_7/ReadVariableOp:value:0,sequential_3/lstm_7/ReadVariableOp_1:value:0*sequential_3/lstm_7/strided_slice:output:0Ksequential_3/lstm_7/TensorArrayUnstack/TensorListFromTensor:output_handle:0>sequential_3_lstm_7_lstm_cell_9_matmul_readvariableop_resourceBsequential_3_lstm_7_lstm_cell_9_matmul_1_readvariableop_1_resource?sequential_3_lstm_7_lstm_cell_9_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *0
body(R&
$sequential_3_lstm_7_while_body_43436*0
cond(R&
$sequential_3_lstm_7_while_cond_43435*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
sequential_3/lstm_7/while?
Dsequential_3/lstm_7/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2F
Dsequential_3/lstm_7/TensorArrayV2Stack/TensorListStack/element_shape?
6sequential_3/lstm_7/TensorArrayV2Stack/TensorListStackTensorListStack"sequential_3/lstm_7/while:output:3Msequential_3/lstm_7/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype028
6sequential_3/lstm_7/TensorArrayV2Stack/TensorListStack?
)sequential_3/lstm_7/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2+
)sequential_3/lstm_7/strided_slice_2/stack?
+sequential_3/lstm_7/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2-
+sequential_3/lstm_7/strided_slice_2/stack_1?
+sequential_3/lstm_7/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_3/lstm_7/strided_slice_2/stack_2?
#sequential_3/lstm_7/strided_slice_2StridedSlice?sequential_3/lstm_7/TensorArrayV2Stack/TensorListStack:tensor:02sequential_3/lstm_7/strided_slice_2/stack:output:04sequential_3/lstm_7/strided_slice_2/stack_1:output:04sequential_3/lstm_7/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2%
#sequential_3/lstm_7/strided_slice_2?
$sequential_3/lstm_7/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2&
$sequential_3/lstm_7/transpose_1/perm?
sequential_3/lstm_7/transpose_1	Transpose?sequential_3/lstm_7/TensorArrayV2Stack/TensorListStack:tensor:0-sequential_3/lstm_7/transpose_1/perm:output:0*
T0*"
_output_shapes
:
22!
sequential_3/lstm_7/transpose_1?
sequential_3/lstm_7/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential_3/lstm_7/runtime?
$sequential_3/lstm_7/AssignVariableOpAssignVariableOp@sequential_3_lstm_7_lstm_cell_9_matmul_1_readvariableop_resource"sequential_3/lstm_7/while:output:4#^sequential_3/lstm_7/ReadVariableOp8^sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02&
$sequential_3/lstm_7/AssignVariableOp?
&sequential_3/lstm_7/AssignVariableOp_1AssignVariableOp=sequential_3_lstm_7_lstm_cell_9_mul_2_readvariableop_resource"sequential_3/lstm_7/while:output:5%^sequential_3/lstm_7/ReadVariableOp_15^sequential_3/lstm_7/lstm_cell_9/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02(
&sequential_3/lstm_7/AssignVariableOp_1?
"sequential_3/lstm_6/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2$
"sequential_3/lstm_6/transpose/perm?
sequential_3/lstm_6/transpose	Transpose#sequential_3/lstm_7/transpose_1:y:0+sequential_3/lstm_6/transpose/perm:output:0*
T0*"
_output_shapes
:
22
sequential_3/lstm_6/transpose?
sequential_3/lstm_6/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
      2   2
sequential_3/lstm_6/Shape?
'sequential_3/lstm_6/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2)
'sequential_3/lstm_6/strided_slice/stack?
)sequential_3/lstm_6/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2+
)sequential_3/lstm_6/strided_slice/stack_1?
)sequential_3/lstm_6/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2+
)sequential_3/lstm_6/strided_slice/stack_2?
!sequential_3/lstm_6/strided_sliceStridedSlice"sequential_3/lstm_6/Shape:output:00sequential_3/lstm_6/strided_slice/stack:output:02sequential_3/lstm_6/strided_slice/stack_1:output:02sequential_3/lstm_6/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2#
!sequential_3/lstm_6/strided_slice?
/sequential_3/lstm_6/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????21
/sequential_3/lstm_6/TensorArrayV2/element_shape?
!sequential_3/lstm_6/TensorArrayV2TensorListReserve8sequential_3/lstm_6/TensorArrayV2/element_shape:output:0*sequential_3/lstm_6/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02#
!sequential_3/lstm_6/TensorArrayV2?
Isequential_3/lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2K
Isequential_3/lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shape?
;sequential_3/lstm_6/TensorArrayUnstack/TensorListFromTensorTensorListFromTensor!sequential_3/lstm_6/transpose:y:0Rsequential_3/lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02=
;sequential_3/lstm_6/TensorArrayUnstack/TensorListFromTensor?
)sequential_3/lstm_6/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2+
)sequential_3/lstm_6/strided_slice_1/stack?
+sequential_3/lstm_6/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_3/lstm_6/strided_slice_1/stack_1?
+sequential_3/lstm_6/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_3/lstm_6/strided_slice_1/stack_2?
#sequential_3/lstm_6/strided_slice_1StridedSlice!sequential_3/lstm_6/transpose:y:02sequential_3/lstm_6/strided_slice_1/stack:output:04sequential_3/lstm_6/strided_slice_1/stack_1:output:04sequential_3/lstm_6/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2%
#sequential_3/lstm_6/strided_slice_1?
5sequential_3/lstm_6/lstm_cell_8/MatMul/ReadVariableOpReadVariableOp>sequential_3_lstm_6_lstm_cell_8_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype027
5sequential_3/lstm_6/lstm_cell_8/MatMul/ReadVariableOp?
&sequential_3/lstm_6/lstm_cell_8/MatMulMatMul,sequential_3/lstm_6/strided_slice_1:output:0=sequential_3/lstm_6/lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2(
&sequential_3/lstm_6/lstm_cell_8/MatMul?
7sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp@sequential_3_lstm_6_lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype029
7sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp?
9sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1ReadVariableOpBsequential_3_lstm_6_lstm_cell_8_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02;
9sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1?
(sequential_3/lstm_6/lstm_cell_8/MatMul_1MatMul?sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp:value:0Asequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2*
(sequential_3/lstm_6/lstm_cell_8/MatMul_1?
#sequential_3/lstm_6/lstm_cell_8/addAddV20sequential_3/lstm_6/lstm_cell_8/MatMul:product:02sequential_3/lstm_6/lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2%
#sequential_3/lstm_6/lstm_cell_8/add?
6sequential_3/lstm_6/lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp?sequential_3_lstm_6_lstm_cell_8_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype028
6sequential_3/lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp?
'sequential_3/lstm_6/lstm_cell_8/BiasAddBiasAdd'sequential_3/lstm_6/lstm_cell_8/add:z:0>sequential_3/lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2)
'sequential_3/lstm_6/lstm_cell_8/BiasAdd?
/sequential_3/lstm_6/lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :21
/sequential_3/lstm_6/lstm_cell_8/split/split_dim?
%sequential_3/lstm_6/lstm_cell_8/splitSplit8sequential_3/lstm_6/lstm_cell_8/split/split_dim:output:00sequential_3/lstm_6/lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2'
%sequential_3/lstm_6/lstm_cell_8/split?
%sequential_3/lstm_6/lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2'
%sequential_3/lstm_6/lstm_cell_8/Const?
'sequential_3/lstm_6/lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2)
'sequential_3/lstm_6/lstm_cell_8/Const_1?
#sequential_3/lstm_6/lstm_cell_8/MulMul.sequential_3/lstm_6/lstm_cell_8/split:output:0.sequential_3/lstm_6/lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22%
#sequential_3/lstm_6/lstm_cell_8/Mul?
%sequential_3/lstm_6/lstm_cell_8/Add_1AddV2'sequential_3/lstm_6/lstm_cell_8/Mul:z:00sequential_3/lstm_6/lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_6/lstm_cell_8/Add_1?
7sequential_3/lstm_6/lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??29
7sequential_3/lstm_6/lstm_cell_8/clip_by_value/Minimum/y?
5sequential_3/lstm_6/lstm_cell_8/clip_by_value/MinimumMinimum)sequential_3/lstm_6/lstm_cell_8/Add_1:z:0@sequential_3/lstm_6/lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:227
5sequential_3/lstm_6/lstm_cell_8/clip_by_value/Minimum?
/sequential_3/lstm_6/lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    21
/sequential_3/lstm_6/lstm_cell_8/clip_by_value/y?
-sequential_3/lstm_6/lstm_cell_8/clip_by_valueMaximum9sequential_3/lstm_6/lstm_cell_8/clip_by_value/Minimum:z:08sequential_3/lstm_6/lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22/
-sequential_3/lstm_6/lstm_cell_8/clip_by_value?
'sequential_3/lstm_6/lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2)
'sequential_3/lstm_6/lstm_cell_8/Const_2?
'sequential_3/lstm_6/lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2)
'sequential_3/lstm_6/lstm_cell_8/Const_3?
%sequential_3/lstm_6/lstm_cell_8/Mul_1Mul.sequential_3/lstm_6/lstm_cell_8/split:output:10sequential_3/lstm_6/lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_6/lstm_cell_8/Mul_1?
%sequential_3/lstm_6/lstm_cell_8/Add_2AddV2)sequential_3/lstm_6/lstm_cell_8/Mul_1:z:00sequential_3/lstm_6/lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_6/lstm_cell_8/Add_2?
9sequential_3/lstm_6/lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2;
9sequential_3/lstm_6/lstm_cell_8/clip_by_value_1/Minimum/y?
7sequential_3/lstm_6/lstm_cell_8/clip_by_value_1/MinimumMinimum)sequential_3/lstm_6/lstm_cell_8/Add_2:z:0Bsequential_3/lstm_6/lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:229
7sequential_3/lstm_6/lstm_cell_8/clip_by_value_1/Minimum?
1sequential_3/lstm_6/lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    23
1sequential_3/lstm_6/lstm_cell_8/clip_by_value_1/y?
/sequential_3/lstm_6/lstm_cell_8/clip_by_value_1Maximum;sequential_3/lstm_6/lstm_cell_8/clip_by_value_1/Minimum:z:0:sequential_3/lstm_6/lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:221
/sequential_3/lstm_6/lstm_cell_8/clip_by_value_1?
4sequential_3/lstm_6/lstm_cell_8/mul_2/ReadVariableOpReadVariableOp=sequential_3_lstm_6_lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype026
4sequential_3/lstm_6/lstm_cell_8/mul_2/ReadVariableOp?
%sequential_3/lstm_6/lstm_cell_8/mul_2Mul3sequential_3/lstm_6/lstm_cell_8/clip_by_value_1:z:0<sequential_3/lstm_6/lstm_cell_8/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_6/lstm_cell_8/mul_2?
$sequential_3/lstm_6/lstm_cell_8/TanhTanh.sequential_3/lstm_6/lstm_cell_8/split:output:2*
T0*
_output_shapes

:22&
$sequential_3/lstm_6/lstm_cell_8/Tanh?
%sequential_3/lstm_6/lstm_cell_8/mul_3Mul1sequential_3/lstm_6/lstm_cell_8/clip_by_value:z:0(sequential_3/lstm_6/lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_6/lstm_cell_8/mul_3?
%sequential_3/lstm_6/lstm_cell_8/add_3AddV2)sequential_3/lstm_6/lstm_cell_8/mul_2:z:0)sequential_3/lstm_6/lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_6/lstm_cell_8/add_3?
'sequential_3/lstm_6/lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2)
'sequential_3/lstm_6/lstm_cell_8/Const_4?
'sequential_3/lstm_6/lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2)
'sequential_3/lstm_6/lstm_cell_8/Const_5?
%sequential_3/lstm_6/lstm_cell_8/Mul_4Mul.sequential_3/lstm_6/lstm_cell_8/split:output:30sequential_3/lstm_6/lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_6/lstm_cell_8/Mul_4?
%sequential_3/lstm_6/lstm_cell_8/Add_4AddV2)sequential_3/lstm_6/lstm_cell_8/Mul_4:z:00sequential_3/lstm_6/lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_6/lstm_cell_8/Add_4?
9sequential_3/lstm_6/lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2;
9sequential_3/lstm_6/lstm_cell_8/clip_by_value_2/Minimum/y?
7sequential_3/lstm_6/lstm_cell_8/clip_by_value_2/MinimumMinimum)sequential_3/lstm_6/lstm_cell_8/Add_4:z:0Bsequential_3/lstm_6/lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:229
7sequential_3/lstm_6/lstm_cell_8/clip_by_value_2/Minimum?
1sequential_3/lstm_6/lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    23
1sequential_3/lstm_6/lstm_cell_8/clip_by_value_2/y?
/sequential_3/lstm_6/lstm_cell_8/clip_by_value_2Maximum;sequential_3/lstm_6/lstm_cell_8/clip_by_value_2/Minimum:z:0:sequential_3/lstm_6/lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:221
/sequential_3/lstm_6/lstm_cell_8/clip_by_value_2?
&sequential_3/lstm_6/lstm_cell_8/Tanh_1Tanh)sequential_3/lstm_6/lstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22(
&sequential_3/lstm_6/lstm_cell_8/Tanh_1?
%sequential_3/lstm_6/lstm_cell_8/mul_5Mul3sequential_3/lstm_6/lstm_cell_8/clip_by_value_2:z:0*sequential_3/lstm_6/lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22'
%sequential_3/lstm_6/lstm_cell_8/mul_5?
1sequential_3/lstm_6/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   23
1sequential_3/lstm_6/TensorArrayV2_1/element_shape?
#sequential_3/lstm_6/TensorArrayV2_1TensorListReserve:sequential_3/lstm_6/TensorArrayV2_1/element_shape:output:0*sequential_3/lstm_6/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02%
#sequential_3/lstm_6/TensorArrayV2_1v
sequential_3/lstm_6/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential_3/lstm_6/time?
"sequential_3/lstm_6/ReadVariableOpReadVariableOp@sequential_3_lstm_6_lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02$
"sequential_3/lstm_6/ReadVariableOp?
$sequential_3/lstm_6/ReadVariableOp_1ReadVariableOp=sequential_3_lstm_6_lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02&
$sequential_3/lstm_6/ReadVariableOp_1?
,sequential_3/lstm_6/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2.
,sequential_3/lstm_6/while/maximum_iterations?
&sequential_3/lstm_6/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2(
&sequential_3/lstm_6/while/loop_counter?
sequential_3/lstm_6/whileWhile/sequential_3/lstm_6/while/loop_counter:output:05sequential_3/lstm_6/while/maximum_iterations:output:0!sequential_3/lstm_6/time:output:0,sequential_3/lstm_6/TensorArrayV2_1:handle:0*sequential_3/lstm_6/ReadVariableOp:value:0,sequential_3/lstm_6/ReadVariableOp_1:value:0*sequential_3/lstm_6/strided_slice:output:0Ksequential_3/lstm_6/TensorArrayUnstack/TensorListFromTensor:output_handle:0>sequential_3_lstm_6_lstm_cell_8_matmul_readvariableop_resourceBsequential_3_lstm_6_lstm_cell_8_matmul_1_readvariableop_1_resource?sequential_3_lstm_6_lstm_cell_8_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *0
body(R&
$sequential_3_lstm_6_while_body_43610*0
cond(R&
$sequential_3_lstm_6_while_cond_43609*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
sequential_3/lstm_6/while?
Dsequential_3/lstm_6/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2F
Dsequential_3/lstm_6/TensorArrayV2Stack/TensorListStack/element_shape?
6sequential_3/lstm_6/TensorArrayV2Stack/TensorListStackTensorListStack"sequential_3/lstm_6/while:output:3Msequential_3/lstm_6/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype028
6sequential_3/lstm_6/TensorArrayV2Stack/TensorListStack?
)sequential_3/lstm_6/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2+
)sequential_3/lstm_6/strided_slice_2/stack?
+sequential_3/lstm_6/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2-
+sequential_3/lstm_6/strided_slice_2/stack_1?
+sequential_3/lstm_6/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_3/lstm_6/strided_slice_2/stack_2?
#sequential_3/lstm_6/strided_slice_2StridedSlice?sequential_3/lstm_6/TensorArrayV2Stack/TensorListStack:tensor:02sequential_3/lstm_6/strided_slice_2/stack:output:04sequential_3/lstm_6/strided_slice_2/stack_1:output:04sequential_3/lstm_6/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2%
#sequential_3/lstm_6/strided_slice_2?
$sequential_3/lstm_6/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2&
$sequential_3/lstm_6/transpose_1/perm?
sequential_3/lstm_6/transpose_1	Transpose?sequential_3/lstm_6/TensorArrayV2Stack/TensorListStack:tensor:0-sequential_3/lstm_6/transpose_1/perm:output:0*
T0*"
_output_shapes
:
22!
sequential_3/lstm_6/transpose_1?
sequential_3/lstm_6/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential_3/lstm_6/runtime?
$sequential_3/lstm_6/AssignVariableOpAssignVariableOp@sequential_3_lstm_6_lstm_cell_8_matmul_1_readvariableop_resource"sequential_3/lstm_6/while:output:4#^sequential_3/lstm_6/ReadVariableOp8^sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02&
$sequential_3/lstm_6/AssignVariableOp?
&sequential_3/lstm_6/AssignVariableOp_1AssignVariableOp=sequential_3_lstm_6_lstm_cell_8_mul_2_readvariableop_resource"sequential_3/lstm_6/while:output:5%^sequential_3/lstm_6/ReadVariableOp_15^sequential_3/lstm_6/lstm_cell_8/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02(
&sequential_3/lstm_6/AssignVariableOp_1?
-sequential_3/time_distributed_3/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2/
-sequential_3/time_distributed_3/Reshape/shape?
'sequential_3/time_distributed_3/ReshapeReshape#sequential_3/lstm_6/transpose_1:y:06sequential_3/time_distributed_3/Reshape/shape:output:0*
T0*
_output_shapes

:
22)
'sequential_3/time_distributed_3/Reshape?
=sequential_3/time_distributed_3/dense_3/MatMul/ReadVariableOpReadVariableOpFsequential_3_time_distributed_3_dense_3_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02?
=sequential_3/time_distributed_3/dense_3/MatMul/ReadVariableOp?
.sequential_3/time_distributed_3/dense_3/MatMulMatMul0sequential_3/time_distributed_3/Reshape:output:0Esequential_3/time_distributed_3/dense_3/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
20
.sequential_3/time_distributed_3/dense_3/MatMul?
>sequential_3/time_distributed_3/dense_3/BiasAdd/ReadVariableOpReadVariableOpGsequential_3_time_distributed_3_dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02@
>sequential_3/time_distributed_3/dense_3/BiasAdd/ReadVariableOp?
/sequential_3/time_distributed_3/dense_3/BiasAddBiasAdd8sequential_3/time_distributed_3/dense_3/MatMul:product:0Fsequential_3/time_distributed_3/dense_3/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
21
/sequential_3/time_distributed_3/dense_3/BiasAdd?
/sequential_3/time_distributed_3/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      21
/sequential_3/time_distributed_3/Reshape_1/shape?
)sequential_3/time_distributed_3/Reshape_1Reshape8sequential_3/time_distributed_3/dense_3/BiasAdd:output:08sequential_3/time_distributed_3/Reshape_1/shape:output:0*
T0*"
_output_shapes
:
2+
)sequential_3/time_distributed_3/Reshape_1?
/sequential_3/time_distributed_3/Reshape_2/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   21
/sequential_3/time_distributed_3/Reshape_2/shape?
)sequential_3/time_distributed_3/Reshape_2Reshape#sequential_3/lstm_6/transpose_1:y:08sequential_3/time_distributed_3/Reshape_2/shape:output:0*
T0*
_output_shapes

:
22+
)sequential_3/time_distributed_3/Reshape_2?
IdentityIdentity2sequential_3/time_distributed_3/Reshape_1:output:0^NoOp*
T0*"
_output_shapes
:
2

Identity?
NoOpNoOp%^sequential_3/lstm_6/AssignVariableOp'^sequential_3/lstm_6/AssignVariableOp_1#^sequential_3/lstm_6/ReadVariableOp%^sequential_3/lstm_6/ReadVariableOp_17^sequential_3/lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp6^sequential_3/lstm_6/lstm_cell_8/MatMul/ReadVariableOp8^sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp:^sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_15^sequential_3/lstm_6/lstm_cell_8/mul_2/ReadVariableOp^sequential_3/lstm_6/while%^sequential_3/lstm_7/AssignVariableOp'^sequential_3/lstm_7/AssignVariableOp_1#^sequential_3/lstm_7/ReadVariableOp%^sequential_3/lstm_7/ReadVariableOp_17^sequential_3/lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp6^sequential_3/lstm_7/lstm_cell_9/MatMul/ReadVariableOp8^sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp:^sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_15^sequential_3/lstm_7/lstm_cell_9/mul_2/ReadVariableOp^sequential_3/lstm_7/while?^sequential_3/time_distributed_3/dense_3/BiasAdd/ReadVariableOp>^sequential_3/time_distributed_3/dense_3/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 2L
$sequential_3/lstm_6/AssignVariableOp$sequential_3/lstm_6/AssignVariableOp2P
&sequential_3/lstm_6/AssignVariableOp_1&sequential_3/lstm_6/AssignVariableOp_12H
"sequential_3/lstm_6/ReadVariableOp"sequential_3/lstm_6/ReadVariableOp2L
$sequential_3/lstm_6/ReadVariableOp_1$sequential_3/lstm_6/ReadVariableOp_12p
6sequential_3/lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp6sequential_3/lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp2n
5sequential_3/lstm_6/lstm_cell_8/MatMul/ReadVariableOp5sequential_3/lstm_6/lstm_cell_8/MatMul/ReadVariableOp2r
7sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp7sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp2v
9sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_19sequential_3/lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_12l
4sequential_3/lstm_6/lstm_cell_8/mul_2/ReadVariableOp4sequential_3/lstm_6/lstm_cell_8/mul_2/ReadVariableOp26
sequential_3/lstm_6/whilesequential_3/lstm_6/while2L
$sequential_3/lstm_7/AssignVariableOp$sequential_3/lstm_7/AssignVariableOp2P
&sequential_3/lstm_7/AssignVariableOp_1&sequential_3/lstm_7/AssignVariableOp_12H
"sequential_3/lstm_7/ReadVariableOp"sequential_3/lstm_7/ReadVariableOp2L
$sequential_3/lstm_7/ReadVariableOp_1$sequential_3/lstm_7/ReadVariableOp_12p
6sequential_3/lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp6sequential_3/lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp2n
5sequential_3/lstm_7/lstm_cell_9/MatMul/ReadVariableOp5sequential_3/lstm_7/lstm_cell_9/MatMul/ReadVariableOp2r
7sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp7sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp2v
9sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_19sequential_3/lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_12l
4sequential_3/lstm_7/lstm_cell_9/mul_2/ReadVariableOp4sequential_3/lstm_7/lstm_cell_9/mul_2/ReadVariableOp26
sequential_3/lstm_7/whilesequential_3/lstm_7/while2?
>sequential_3/time_distributed_3/dense_3/BiasAdd/ReadVariableOp>sequential_3/time_distributed_3/dense_3/BiasAdd/ReadVariableOp2~
=sequential_3/time_distributed_3/dense_3/MatMul/ReadVariableOp=sequential_3/time_distributed_3/dense_3/MatMul/ReadVariableOp:P L
"
_output_shapes
:

&
_user_specified_namelstm_7_input
?	
?
&__inference_lstm_6_layer_call_fn_48817
inputs_0
unknown:2
	unknown_0:2
	unknown_1:	2?
	unknown_2:	2?
	unknown_3:	?
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_6_layer_call_and_return_conditional_losses_450392
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:?????????22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:?????????2: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:?????????2
"
_user_specified_name
inputs/0
?
?
while_cond_43823
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_43823___redundant_placeholder03
/while_while_cond_43823___redundant_placeholder13
/while_while_cond_43823___redundant_placeholder23
/while_while_cond_43823___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?
?
while_cond_47731
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_47731___redundant_placeholder03
/while_while_cond_47731___redundant_placeholder13
/while_while_cond_47731___redundant_placeholder23
/while_while_cond_47731___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?0
?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_43804

inputs
states:2
states_1:21
matmul_readvariableop_resource:	?5
"matmul_1_readvariableop_1_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?MatMul_1/ReadVariableOp_1?mul_2/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:2*
dtype02
MatMul_1/ReadVariableOp?
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOp_1?
MatMul_1MatMulMatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1Z
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes

:22
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:22
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes

:22
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3`
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes

:22
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:22
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1u
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes

:2*
dtype02
mul_2/ReadVariableOpq
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:22
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:22
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:22
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5`
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes

:22
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:22
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:22
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:22
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:22

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^MatMul_1/ReadVariableOp_1^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:: : : : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
?m
?
A__inference_lstm_7_layer_call_and_return_conditional_losses_47837

inputs=
*lstm_cell_9_matmul_readvariableop_resource:	?>
,lstm_cell_9_matmul_1_readvariableop_resource:2A
.lstm_cell_9_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_9_biasadd_readvariableop_resource:	?;
)lstm_cell_9_mul_2_readvariableop_resource:2
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_9/BiasAdd/ReadVariableOp?!lstm_cell_9/MatMul/ReadVariableOp?#lstm_cell_9/MatMul_1/ReadVariableOp?%lstm_cell_9/MatMul_1/ReadVariableOp_1? lstm_cell_9/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permq
	transpose	Transposeinputstranspose/perm:output:0*
T0*"
_output_shapes
:
2
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
         2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_9/MatMul/ReadVariableOpReadVariableOp*lstm_cell_9_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02#
!lstm_cell_9/MatMul/ReadVariableOp?
lstm_cell_9/MatMulMatMulstrided_slice_1:output:0)lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/MatMul?
#lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02%
#lstm_cell_9/MatMul_1/ReadVariableOp?
%lstm_cell_9/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_9_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_9/MatMul_1/ReadVariableOp_1?
lstm_cell_9/MatMul_1MatMul+lstm_cell_9/MatMul_1/ReadVariableOp:value:0-lstm_cell_9/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/MatMul_1?
lstm_cell_9/addAddV2lstm_cell_9/MatMul:product:0lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_cell_9/add?
"lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_9_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_9/BiasAdd/ReadVariableOp?
lstm_cell_9/BiasAddBiasAddlstm_cell_9/add:z:0*lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/BiasAdd|
lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_9/split/split_dim?
lstm_cell_9/splitSplit$lstm_cell_9/split/split_dim:output:0lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_9/splitk
lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Consto
lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_1?
lstm_cell_9/MulMullstm_cell_9/split:output:0lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul?
lstm_cell_9/Add_1AddV2lstm_cell_9/Mul:z:0lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_1?
#lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_9/clip_by_value/Minimum/y?
!lstm_cell_9/clip_by_value/MinimumMinimumlstm_cell_9/Add_1:z:0,lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22#
!lstm_cell_9/clip_by_value/Minimum
lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value/y?
lstm_cell_9/clip_by_valueMaximum%lstm_cell_9/clip_by_value/Minimum:z:0$lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_valueo
lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Const_2o
lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_3?
lstm_cell_9/Mul_1Mullstm_cell_9/split:output:1lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul_1?
lstm_cell_9/Add_2AddV2lstm_cell_9/Mul_1:z:0lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_2?
%lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_9/clip_by_value_1/Minimum/y?
#lstm_cell_9/clip_by_value_1/MinimumMinimumlstm_cell_9/Add_2:z:0.lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_9/clip_by_value_1/Minimum?
lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value_1/y?
lstm_cell_9/clip_by_value_1Maximum'lstm_cell_9/clip_by_value_1/Minimum:z:0&lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_value_1?
 lstm_cell_9/mul_2/ReadVariableOpReadVariableOp)lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02"
 lstm_cell_9/mul_2/ReadVariableOp?
lstm_cell_9/mul_2Mullstm_cell_9/clip_by_value_1:z:0(lstm_cell_9/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_2q
lstm_cell_9/TanhTanhlstm_cell_9/split:output:2*
T0*
_output_shapes

:22
lstm_cell_9/Tanh?
lstm_cell_9/mul_3Mullstm_cell_9/clip_by_value:z:0lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_3?
lstm_cell_9/add_3AddV2lstm_cell_9/mul_2:z:0lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_9/add_3o
lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Const_4o
lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_5?
lstm_cell_9/Mul_4Mullstm_cell_9/split:output:3lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul_4?
lstm_cell_9/Add_4AddV2lstm_cell_9/Mul_4:z:0lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_4?
%lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_9/clip_by_value_2/Minimum/y?
#lstm_cell_9/clip_by_value_2/MinimumMinimumlstm_cell_9/Add_4:z:0.lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_9/clip_by_value_2/Minimum?
lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value_2/y?
lstm_cell_9/clip_by_value_2Maximum'lstm_cell_9/clip_by_value_2/Minimum:z:0&lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_value_2p
lstm_cell_9/Tanh_1Tanhlstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_9/Tanh_1?
lstm_cell_9/mul_5Mullstm_cell_9/clip_by_value_2:z:0lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time?
ReadVariableOpReadVariableOp,lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_9_matmul_readvariableop_resource.lstm_cell_9_matmul_1_readvariableop_1_resource+lstm_cell_9_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_47732*
condR
while_cond_47731*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_9_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_9/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_9_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_9/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_9/BiasAdd/ReadVariableOp"^lstm_cell_9/MatMul/ReadVariableOp$^lstm_cell_9/MatMul_1/ReadVariableOp&^lstm_cell_9/MatMul_1/ReadVariableOp_1!^lstm_cell_9/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:
: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_9/BiasAdd/ReadVariableOp"lstm_cell_9/BiasAdd/ReadVariableOp2F
!lstm_cell_9/MatMul/ReadVariableOp!lstm_cell_9/MatMul/ReadVariableOp2J
#lstm_cell_9/MatMul_1/ReadVariableOp#lstm_cell_9/MatMul_1/ReadVariableOp2N
%lstm_cell_9/MatMul_1/ReadVariableOp_1%lstm_cell_9/MatMul_1/ReadVariableOp_12D
 lstm_cell_9/mul_2/ReadVariableOp lstm_cell_9/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:

 
_user_specified_nameinputs
?,
?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_43900

inputs

states
states_11
matmul_readvariableop_resource:	?3
 matmul_1_readvariableop_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1Z
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes

:22
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:22
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes

:22
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3`
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes

:22
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:22
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:22
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:22
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:22
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:22
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5`
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes

:22
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:22
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:22
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:22
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:22

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$::2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:
 
_user_specified_nameinputs:FB

_output_shapes

:2
 
_user_specified_namestates:FB

_output_shapes

:2
 
_user_specified_namestates
?$
?
while_body_44600
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0,
while_lstm_cell_8_44677_0:	2?,
while_lstm_cell_8_44679_0:	2?(
while_lstm_cell_8_44681_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor*
while_lstm_cell_8_44677:	2?*
while_lstm_cell_8_44679:	2?&
while_lstm_cell_8_44681:	???)while/lstm_cell_8/StatefulPartitionedCall?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
)while/lstm_cell_8/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_8_44677_0while_lstm_cell_8_44679_0while_lstm_cell_8_44681_0*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:2:2:2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_446762+
)while/lstm_cell_8/StatefulPartitionedCall?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder2while/lstm_cell_8/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identity2while/lstm_cell_8/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identity2while/lstm_cell_8/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp*^while/lstm_cell_8/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"4
while_lstm_cell_8_44677while_lstm_cell_8_44677_0"4
while_lstm_cell_8_44679while_lstm_cell_8_44679_0"4
while_lstm_cell_8_44681while_lstm_cell_8_44681_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_8/StatefulPartitionedCall)while/lstm_cell_8/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?,
?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49518

inputs
states_0
states_11
matmul_readvariableop_resource:	2?3
 matmul_1_readvariableop_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1Z
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes

:22
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:22
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes

:22
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3`
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes

:22
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:22
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:22
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:22
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:22
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:22
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5`
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes

:22
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:22
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:22
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:22
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:22

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:2:2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:HD

_output_shapes

:2
"
_user_specified_name
states/0:HD

_output_shapes

:2
"
_user_specified_name
states/1
?	
?
&__inference_lstm_6_layer_call_fn_48802
inputs_0
unknown:2
	unknown_0:2
	unknown_1:	2?
	unknown_2:	2?
	unknown_3:	?
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_6_layer_call_and_return_conditional_losses_447222
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:?????????22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:?????????2: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:?????????2
"
_user_specified_name
inputs/0
?.
?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49342

inputs

states
states_11
matmul_readvariableop_resource:	?5
"matmul_1_readvariableop_1_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?MatMul_1/ReadVariableOp_1?mul_2/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMuls
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp?
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOp_1?
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpi
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*$
_output_shapes
::::*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1T
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes
:2
MulU
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes
:2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes
:2
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3Z
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes
:2
Mul_1W
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes
:2
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1o
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes
:*
dtype02
mul_2/ReadVariableOpk
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes
:2
mul_2G
TanhTanhsplit:output:2*
T0*
_output_shapes
:2
TanhU
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes
:2
mul_3P
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes
:2
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5Z
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes
:2
Mul_4W
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes
:2
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2F
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes
:2
Tanh_1Y
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes
:2
mul_5U
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

IdentityY

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

Identity_1Y

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes
:2

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^MatMul_1/ReadVariableOp_1^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$::2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
?	
?
&__inference_lstm_7_layer_call_fn_48045
inputs_0
unknown:2
	unknown_0:2
	unknown_1:	?
	unknown_2:	2?
	unknown_3:	?
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_7_layer_call_and_return_conditional_losses_442632
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:?????????22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:?????????: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:?????????
"
_user_specified_name
inputs/0
?
?
$sequential_3_lstm_6_while_cond_43609D
@sequential_3_lstm_6_while_sequential_3_lstm_6_while_loop_counterJ
Fsequential_3_lstm_6_while_sequential_3_lstm_6_while_maximum_iterations)
%sequential_3_lstm_6_while_placeholder+
'sequential_3_lstm_6_while_placeholder_1+
'sequential_3_lstm_6_while_placeholder_2+
'sequential_3_lstm_6_while_placeholder_3D
@sequential_3_lstm_6_while_less_sequential_3_lstm_6_strided_slice[
Wsequential_3_lstm_6_while_sequential_3_lstm_6_while_cond_43609___redundant_placeholder0[
Wsequential_3_lstm_6_while_sequential_3_lstm_6_while_cond_43609___redundant_placeholder1[
Wsequential_3_lstm_6_while_sequential_3_lstm_6_while_cond_43609___redundant_placeholder2[
Wsequential_3_lstm_6_while_sequential_3_lstm_6_while_cond_43609___redundant_placeholder3&
"sequential_3_lstm_6_while_identity
?
sequential_3/lstm_6/while/LessLess%sequential_3_lstm_6_while_placeholder@sequential_3_lstm_6_while_less_sequential_3_lstm_6_strided_slice*
T0*
_output_shapes
: 2 
sequential_3/lstm_6/while/Less?
"sequential_3/lstm_6/while/IdentityIdentity"sequential_3/lstm_6/while/Less:z:0*
T0
*
_output_shapes
: 2$
"sequential_3/lstm_6/while/Identity"Q
"sequential_3_lstm_6_while_identity+sequential_3/lstm_6/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?
?
+__inference_lstm_cell_9_layer_call_fn_49190

inputs
states_0
states_1
unknown:	?
	unknown_0:	2?
	unknown_1:	?
identity

identity_1

identity_2??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0states_1unknown	unknown_0	unknown_1*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:2:2:2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_439002
StatefulPartitionedCallr
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes

:22

Identityv

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*
_output_shapes

:22

Identity_1v

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*
_output_shapes

:22

Identity_2h
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$::2:2: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:
 
_user_specified_nameinputs:HD

_output_shapes

:2
"
_user_specified_name
states/0:HD

_output_shapes

:2
"
_user_specified_name
states/1
?.
?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49412

inputs
states_0
states_11
matmul_readvariableop_resource:	2?5
"matmul_1_readvariableop_1_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?MatMul_1/ReadVariableOp_1?mul_2/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMulu
MatMul_1/ReadVariableOpReadVariableOpstates_0*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp?
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOp_1?
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpi
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*$
_output_shapes
::::*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1T
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes
:2
MulU
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes
:2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes
:2
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3Z
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes
:2
Mul_1W
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes
:2
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1o
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes
:*
dtype02
mul_2/ReadVariableOpk
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes
:2
mul_2G
TanhTanhsplit:output:2*
T0*
_output_shapes
:2
TanhU
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes
:2
mul_3P
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes
:2
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5Z
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes
:2
Mul_4W
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes
:2
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2F
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes
:2
Tanh_1Y
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes
:2
mul_5U
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

IdentityY

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

Identity_1Y

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes
:2

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^MatMul_1/ReadVariableOp_1^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:2:2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
?m
?
A__inference_lstm_6_layer_call_and_return_conditional_losses_45791

inputs=
*lstm_cell_8_matmul_readvariableop_resource:	2?>
,lstm_cell_8_matmul_1_readvariableop_resource:2A
.lstm_cell_8_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_8_biasadd_readvariableop_resource:	?;
)lstm_cell_8_mul_2_readvariableop_resource:2
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_8/BiasAdd/ReadVariableOp?!lstm_cell_8/MatMul/ReadVariableOp?#lstm_cell_8/MatMul_1/ReadVariableOp?%lstm_cell_8/MatMul_1/ReadVariableOp_1? lstm_cell_8/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permq
	transpose	Transposeinputstranspose/perm:output:0*
T0*"
_output_shapes
:
22
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
      2   2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_8/MatMul/ReadVariableOpReadVariableOp*lstm_cell_8_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02#
!lstm_cell_8/MatMul/ReadVariableOp?
lstm_cell_8/MatMulMatMulstrided_slice_1:output:0)lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/MatMul?
#lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02%
#lstm_cell_8/MatMul_1/ReadVariableOp?
%lstm_cell_8/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_8_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_8/MatMul_1/ReadVariableOp_1?
lstm_cell_8/MatMul_1MatMul+lstm_cell_8/MatMul_1/ReadVariableOp:value:0-lstm_cell_8/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/MatMul_1?
lstm_cell_8/addAddV2lstm_cell_8/MatMul:product:0lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_cell_8/add?
"lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_8_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_8/BiasAdd/ReadVariableOp?
lstm_cell_8/BiasAddBiasAddlstm_cell_8/add:z:0*lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/BiasAdd|
lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_8/split/split_dim?
lstm_cell_8/splitSplit$lstm_cell_8/split/split_dim:output:0lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_8/splitk
lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Consto
lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_1?
lstm_cell_8/MulMullstm_cell_8/split:output:0lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul?
lstm_cell_8/Add_1AddV2lstm_cell_8/Mul:z:0lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_1?
#lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_8/clip_by_value/Minimum/y?
!lstm_cell_8/clip_by_value/MinimumMinimumlstm_cell_8/Add_1:z:0,lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22#
!lstm_cell_8/clip_by_value/Minimum
lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value/y?
lstm_cell_8/clip_by_valueMaximum%lstm_cell_8/clip_by_value/Minimum:z:0$lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_valueo
lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Const_2o
lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_3?
lstm_cell_8/Mul_1Mullstm_cell_8/split:output:1lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul_1?
lstm_cell_8/Add_2AddV2lstm_cell_8/Mul_1:z:0lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_2?
%lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_8/clip_by_value_1/Minimum/y?
#lstm_cell_8/clip_by_value_1/MinimumMinimumlstm_cell_8/Add_2:z:0.lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_8/clip_by_value_1/Minimum?
lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value_1/y?
lstm_cell_8/clip_by_value_1Maximum'lstm_cell_8/clip_by_value_1/Minimum:z:0&lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_value_1?
 lstm_cell_8/mul_2/ReadVariableOpReadVariableOp)lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02"
 lstm_cell_8/mul_2/ReadVariableOp?
lstm_cell_8/mul_2Mullstm_cell_8/clip_by_value_1:z:0(lstm_cell_8/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_2q
lstm_cell_8/TanhTanhlstm_cell_8/split:output:2*
T0*
_output_shapes

:22
lstm_cell_8/Tanh?
lstm_cell_8/mul_3Mullstm_cell_8/clip_by_value:z:0lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_3?
lstm_cell_8/add_3AddV2lstm_cell_8/mul_2:z:0lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_8/add_3o
lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Const_4o
lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_5?
lstm_cell_8/Mul_4Mullstm_cell_8/split:output:3lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul_4?
lstm_cell_8/Add_4AddV2lstm_cell_8/Mul_4:z:0lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_4?
%lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_8/clip_by_value_2/Minimum/y?
#lstm_cell_8/clip_by_value_2/MinimumMinimumlstm_cell_8/Add_4:z:0.lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_8/clip_by_value_2/Minimum?
lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value_2/y?
lstm_cell_8/clip_by_value_2Maximum'lstm_cell_8/clip_by_value_2/Minimum:z:0&lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_value_2p
lstm_cell_8/Tanh_1Tanhlstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_8/Tanh_1?
lstm_cell_8/mul_5Mullstm_cell_8/clip_by_value_2:z:0lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time?
ReadVariableOpReadVariableOp,lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_8_matmul_readvariableop_resource.lstm_cell_8_matmul_1_readvariableop_1_resource+lstm_cell_8_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_45686*
condR
while_cond_45685*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_8_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_8/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_8_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_8/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_8/BiasAdd/ReadVariableOp"^lstm_cell_8/MatMul/ReadVariableOp$^lstm_cell_8/MatMul_1/ReadVariableOp&^lstm_cell_8/MatMul_1/ReadVariableOp_1!^lstm_cell_8/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:
2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_8/BiasAdd/ReadVariableOp"lstm_cell_8/BiasAdd/ReadVariableOp2F
!lstm_cell_8/MatMul/ReadVariableOp!lstm_cell_8/MatMul/ReadVariableOp2J
#lstm_cell_8/MatMul_1/ReadVariableOp#lstm_cell_8/MatMul_1/ReadVariableOp2N
%lstm_cell_8/MatMul_1/ReadVariableOp_1%lstm_cell_8/MatMul_1/ReadVariableOp_12D
 lstm_cell_8/mul_2/ReadVariableOp lstm_cell_8/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
?
?
while_cond_47375
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_47375___redundant_placeholder03
/while_while_cond_47375___redundant_placeholder13
/while_while_cond_47375___redundant_placeholder23
/while_while_cond_47375___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?	
?
lstm_7_while_cond_46953*
&lstm_7_while_lstm_7_while_loop_counter0
,lstm_7_while_lstm_7_while_maximum_iterations
lstm_7_while_placeholder
lstm_7_while_placeholder_1
lstm_7_while_placeholder_2
lstm_7_while_placeholder_3*
&lstm_7_while_less_lstm_7_strided_sliceA
=lstm_7_while_lstm_7_while_cond_46953___redundant_placeholder0A
=lstm_7_while_lstm_7_while_cond_46953___redundant_placeholder1A
=lstm_7_while_lstm_7_while_cond_46953___redundant_placeholder2A
=lstm_7_while_lstm_7_while_cond_46953___redundant_placeholder3
lstm_7_while_identity
?
lstm_7/while/LessLesslstm_7_while_placeholder&lstm_7_while_less_lstm_7_strided_slice*
T0*
_output_shapes
: 2
lstm_7/while/Lessr
lstm_7/while/IdentityIdentitylstm_7/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_7/while/Identity"7
lstm_7_while_identitylstm_7/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?9
?
A__inference_lstm_7_layer_call_and_return_conditional_losses_43946

inputs#
lstm_cell_9_43805:2#
lstm_cell_9_43807:2$
lstm_cell_9_43809:	?$
lstm_cell_9_43811:	2? 
lstm_cell_9_43813:	?
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?#lstm_cell_9/StatefulPartitionedCall?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:?????????2
	transposeK
ShapeShapetranspose:y:0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1?
#lstm_cell_9/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_9_43805lstm_cell_9_43807lstm_cell_9_43809lstm_cell_9_43811lstm_cell_9_43813*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:2:2:2*'
_read_only_resource_inputs	
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_438042%
#lstm_cell_9/StatefulPartitionedCall?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
timer
ReadVariableOpReadVariableOplstm_cell_9_43805*
_output_shapes

:2*
dtype02
ReadVariableOpv
ReadVariableOp_1ReadVariableOplstm_cell_9_43807*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_9_43809lstm_cell_9_43811lstm_cell_9_43813*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_43824*
condR
while_cond_43823*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOplstm_cell_9_43805while:output:4^ReadVariableOp$^lstm_cell_9/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOplstm_cell_9_43807while:output:5^ReadVariableOp_1$^lstm_cell_9/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_9/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:?????????: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_9/StatefulPartitionedCall#lstm_cell_9/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_45362

inputs
dense_3_45352:2
dense_3_45354:
identity??dense_3/StatefulPartitionedCallD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_sliceo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2
Reshape/shapeo
ReshapeReshapeinputsReshape/shape:output:0*
T0*'
_output_shapes
:?????????22	
Reshape?
dense_3/StatefulPartitionedCallStatefulPartitionedCallReshape:output:0dense_3_45352dense_3_45354*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_3_layer_call_and_return_conditional_losses_453032!
dense_3/StatefulPartitionedCallq
Reshape_1/shape/0Const*
_output_shapes
: *
dtype0*
valueB :
?????????2
Reshape_1/shape/0h
Reshape_1/shape/2Const*
_output_shapes
: *
dtype0*
value	B :2
Reshape_1/shape/2?
Reshape_1/shapePackReshape_1/shape/0:output:0strided_slice:output:0Reshape_1/shape/2:output:0*
N*
T0*
_output_shapes
:2
Reshape_1/shape?
	Reshape_1Reshape(dense_3/StatefulPartitionedCall:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :??????????????????2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :??????????????????2

Identityp
NoOpNoOp ^dense_3/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:??????????????????2: : 2B
dense_3/StatefulPartitionedCalldense_3/StatefulPartitionedCall:\ X
4
_output_shapes"
 :??????????????????2
 
_user_specified_nameinputs
?
?
while_cond_44193
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_44193___redundant_placeholder03
/while_while_cond_44193___redundant_placeholder13
/while_while_cond_44193___redundant_placeholder23
/while_while_cond_44193___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?.
?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49173

inputs
states_0
states_11
matmul_readvariableop_resource:	?5
"matmul_1_readvariableop_1_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?MatMul_1/ReadVariableOp_1?mul_2/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMulu
MatMul_1/ReadVariableOpReadVariableOpstates_0*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp?
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOp_1?
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpi
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*$
_output_shapes
::::*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1T
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes
:2
MulU
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes
:2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes
:2
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3Z
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes
:2
Mul_1W
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes
:2
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1o
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes
:*
dtype02
mul_2/ReadVariableOpk
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes
:2
mul_2G
TanhTanhsplit:output:2*
T0*
_output_shapes
:2
TanhU
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes
:2
mul_3P
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes
:2
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5Z
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes
:2
Mul_4W
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes
:2
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2F
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes
:2
Tanh_1Y
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes
:2
mul_5U
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

IdentityY

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

Identity_1Y

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes
:2

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^MatMul_1/ReadVariableOp_1^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$::2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
?.
?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49670

inputs

states
states_11
matmul_readvariableop_resource:	2?5
"matmul_1_readvariableop_1_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?MatMul_1/ReadVariableOp_1?mul_2/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMuls
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp?
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOp_1?
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpi
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*$
_output_shapes
::::*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1T
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes
:2
MulU
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes
:2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes
:2
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3Z
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes
:2
Mul_1W
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes
:2
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1o
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes
:*
dtype02
mul_2/ReadVariableOpk
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes
:2
mul_2G
TanhTanhsplit:output:2*
T0*
_output_shapes
:2
TanhU
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes
:2
mul_3P
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes
:2
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5Z
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes
:2
Mul_4W
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes
:2
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2F
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes
:2
Tanh_1Y
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes
:2
mul_5U
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

IdentityY

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

Identity_1Y

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes
:2

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^MatMul_1/ReadVariableOp_1^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:2:2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
?	
?
lstm_6_while_cond_47127*
&lstm_6_while_lstm_6_while_loop_counter0
,lstm_6_while_lstm_6_while_maximum_iterations
lstm_6_while_placeholder
lstm_6_while_placeholder_1
lstm_6_while_placeholder_2
lstm_6_while_placeholder_3*
&lstm_6_while_less_lstm_6_strided_sliceA
=lstm_6_while_lstm_6_while_cond_47127___redundant_placeholder0A
=lstm_6_while_lstm_6_while_cond_47127___redundant_placeholder1A
=lstm_6_while_lstm_6_while_cond_47127___redundant_placeholder2A
=lstm_6_while_lstm_6_while_cond_47127___redundant_placeholder3
lstm_6_while_identity
?
lstm_6/while/LessLesslstm_6_while_placeholder&lstm_6_while_less_lstm_6_strided_slice*
T0*
_output_shapes
: 2
lstm_6/while/Lessr
lstm_6/while/IdentityIdentitylstm_6/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_6/while/Identity"7
lstm_6_while_identitylstm_6/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?
?
+__inference_lstm_cell_8_layer_call_fn_49609

inputs
states_0
states_1
unknown:	2?
	unknown_0:	2?
	unknown_1:	?
identity

identity_1

identity_2??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0states_1unknown	unknown_0	unknown_1*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:2:2:2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_448102
StatefulPartitionedCallr
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes

:22

Identityv

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*
_output_shapes

:22

Identity_1v

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*
_output_shapes

:22

Identity_2h
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:2:2:2: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:2
 
_user_specified_nameinputs:HD

_output_shapes

:2
"
_user_specified_name
states/0:HD

_output_shapes

:2
"
_user_specified_name
states/1
?X
?
while_body_45974
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_8_matmul_readvariableop_resource_0:	2?G
4while_lstm_cell_8_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_8_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_8_matmul_readvariableop_resource:	2?E
2while_lstm_cell_8_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_8_biasadd_readvariableop_resource:	???(while/lstm_cell_8/BiasAdd/ReadVariableOp?'while/lstm_cell_8/MatMul/ReadVariableOp?)while/lstm_cell_8/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_8/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_8_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02)
'while/lstm_cell_8/MatMul/ReadVariableOp?
while/lstm_cell_8/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/MatMul?
)while/lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_8_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_8/MatMul_1/ReadVariableOp?
while/lstm_cell_8/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_8/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/MatMul_1?
while/lstm_cell_8/addAddV2"while/lstm_cell_8/MatMul:product:0$while/lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/add?
(while/lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_8_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_8/BiasAdd/ReadVariableOp?
while/lstm_cell_8/BiasAddBiasAddwhile/lstm_cell_8/add:z:00while/lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/BiasAdd?
!while/lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_8/split/split_dim?
while/lstm_cell_8/splitSplit*while/lstm_cell_8/split/split_dim:output:0"while/lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_8/splitw
while/lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const{
while/lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_1?
while/lstm_cell_8/MulMul while/lstm_cell_8/split:output:0 while/lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul?
while/lstm_cell_8/Add_1AddV2while/lstm_cell_8/Mul:z:0"while/lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_1?
)while/lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_8/clip_by_value/Minimum/y?
'while/lstm_cell_8/clip_by_value/MinimumMinimumwhile/lstm_cell_8/Add_1:z:02while/lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22)
'while/lstm_cell_8/clip_by_value/Minimum?
!while/lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_8/clip_by_value/y?
while/lstm_cell_8/clip_by_valueMaximum+while/lstm_cell_8/clip_by_value/Minimum:z:0*while/lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22!
while/lstm_cell_8/clip_by_value{
while/lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const_2{
while/lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_3?
while/lstm_cell_8/Mul_1Mul while/lstm_cell_8/split:output:1"while/lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul_1?
while/lstm_cell_8/Add_2AddV2while/lstm_cell_8/Mul_1:z:0"while/lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_2?
+while/lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_8/clip_by_value_1/Minimum/y?
)while/lstm_cell_8/clip_by_value_1/MinimumMinimumwhile/lstm_cell_8/Add_2:z:04while/lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_8/clip_by_value_1/Minimum?
#while/lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_8/clip_by_value_1/y?
!while/lstm_cell_8/clip_by_value_1Maximum-while/lstm_cell_8/clip_by_value_1/Minimum:z:0,while/lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_8/clip_by_value_1?
while/lstm_cell_8/mul_2Mul%while/lstm_cell_8/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_2?
while/lstm_cell_8/TanhTanh while/lstm_cell_8/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_8/Tanh?
while/lstm_cell_8/mul_3Mul#while/lstm_cell_8/clip_by_value:z:0while/lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_3?
while/lstm_cell_8/add_3AddV2while/lstm_cell_8/mul_2:z:0while/lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_8/add_3{
while/lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const_4{
while/lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_5?
while/lstm_cell_8/Mul_4Mul while/lstm_cell_8/split:output:3"while/lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul_4?
while/lstm_cell_8/Add_4AddV2while/lstm_cell_8/Mul_4:z:0"while/lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_4?
+while/lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_8/clip_by_value_2/Minimum/y?
)while/lstm_cell_8/clip_by_value_2/MinimumMinimumwhile/lstm_cell_8/Add_4:z:04while/lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_8/clip_by_value_2/Minimum?
#while/lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_8/clip_by_value_2/y?
!while/lstm_cell_8/clip_by_value_2Maximum-while/lstm_cell_8/clip_by_value_2/Minimum:z:0,while/lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_8/clip_by_value_2?
while/lstm_cell_8/Tanh_1Tanhwhile/lstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Tanh_1?
while/lstm_cell_8/mul_5Mul%while/lstm_cell_8/clip_by_value_2:z:0while/lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_8/mul_5:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_8/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_8/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_8/BiasAdd/ReadVariableOp(^while/lstm_cell_8/MatMul/ReadVariableOp*^while/lstm_cell_8/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"h
1while_lstm_cell_8_biasadd_readvariableop_resource3while_lstm_cell_8_biasadd_readvariableop_resource_0"j
2while_lstm_cell_8_matmul_1_readvariableop_resource4while_lstm_cell_8_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_8_matmul_readvariableop_resource2while_lstm_cell_8_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2T
(while/lstm_cell_8/BiasAdd/ReadVariableOp(while/lstm_cell_8/BiasAdd/ReadVariableOp2R
'while/lstm_cell_8/MatMul/ReadVariableOp'while/lstm_cell_8/MatMul/ReadVariableOp2V
)while/lstm_cell_8/MatMul_1/ReadVariableOp)while/lstm_cell_8/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?
?
while_cond_48325
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_48325___redundant_placeholder03
/while_while_cond_48325___redundant_placeholder13
/while_while_cond_48325___redundant_placeholder23
/while_while_cond_48325___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?
?
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_48889

inputs8
&dense_3_matmul_readvariableop_resource:25
'dense_3_biasadd_readvariableop_resource:
identity??dense_3/BiasAdd/ReadVariableOp?dense_3/MatMul/ReadVariableOpD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_sliceo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2
Reshape/shapeo
ReshapeReshapeinputsReshape/shape:output:0*
T0*'
_output_shapes
:?????????22	
Reshape?
dense_3/MatMul/ReadVariableOpReadVariableOp&dense_3_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_3/MatMul/ReadVariableOp?
dense_3/MatMulMatMulReshape:output:0%dense_3/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_3/MatMul?
dense_3/BiasAdd/ReadVariableOpReadVariableOp'dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_3/BiasAdd/ReadVariableOp?
dense_3/BiasAddBiasAdddense_3/MatMul:product:0&dense_3/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_3/BiasAddq
Reshape_1/shape/0Const*
_output_shapes
: *
dtype0*
valueB :
?????????2
Reshape_1/shape/0h
Reshape_1/shape/2Const*
_output_shapes
: *
dtype0*
value	B :2
Reshape_1/shape/2?
Reshape_1/shapePackReshape_1/shape/0:output:0strided_slice:output:0Reshape_1/shape/2:output:0*
N*
T0*
_output_shapes
:2
Reshape_1/shape?
	Reshape_1Reshapedense_3/BiasAdd:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :??????????????????2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :??????????????????2

Identity?
NoOpNoOp^dense_3/BiasAdd/ReadVariableOp^dense_3/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:??????????????????2: : 2@
dense_3/BiasAdd/ReadVariableOpdense_3/BiasAdd/ReadVariableOp2>
dense_3/MatMul/ReadVariableOpdense_3/MatMul/ReadVariableOp:\ X
4
_output_shapes"
 :??????????????????2
 
_user_specified_nameinputs
?
?
2__inference_time_distributed_3_layer_call_fn_48935

inputs
unknown:2
	unknown_0:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *4
_output_shapes"
 :??????????????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_453622
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*4
_output_shapes"
 :??????????????????2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:??????????????????2: : 22
StatefulPartitionedCallStatefulPartitionedCall:\ X
4
_output_shapes"
 :??????????????????2
 
_user_specified_nameinputs
?
?
,__inference_sequential_3_layer_call_fn_46420
lstm_7_input
unknown:	?
	unknown_0:2
	unknown_1:	2?
	unknown_2:	?
	unknown_3:2
	unknown_4:	2?
	unknown_5:2
	unknown_6:	2?
	unknown_7:	?
	unknown_8:2
	unknown_9:2

unknown_10:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCalllstm_7_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_sequential_3_layer_call_and_return_conditional_losses_463642
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
"
_output_shapes
:

&
_user_specified_namelstm_7_input
?
?
while_cond_44969
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_44969___redundant_placeholder03
/while_while_cond_44969___redundant_placeholder13
/while_while_cond_44969___redundant_placeholder23
/while_while_cond_44969___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?m
?
A__inference_lstm_7_layer_call_and_return_conditional_losses_45602

inputs=
*lstm_cell_9_matmul_readvariableop_resource:	?>
,lstm_cell_9_matmul_1_readvariableop_resource:2A
.lstm_cell_9_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_9_biasadd_readvariableop_resource:	?;
)lstm_cell_9_mul_2_readvariableop_resource:2
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_9/BiasAdd/ReadVariableOp?!lstm_cell_9/MatMul/ReadVariableOp?#lstm_cell_9/MatMul_1/ReadVariableOp?%lstm_cell_9/MatMul_1/ReadVariableOp_1? lstm_cell_9/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permq
	transpose	Transposeinputstranspose/perm:output:0*
T0*"
_output_shapes
:
2
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
         2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_9/MatMul/ReadVariableOpReadVariableOp*lstm_cell_9_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02#
!lstm_cell_9/MatMul/ReadVariableOp?
lstm_cell_9/MatMulMatMulstrided_slice_1:output:0)lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/MatMul?
#lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02%
#lstm_cell_9/MatMul_1/ReadVariableOp?
%lstm_cell_9/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_9_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_9/MatMul_1/ReadVariableOp_1?
lstm_cell_9/MatMul_1MatMul+lstm_cell_9/MatMul_1/ReadVariableOp:value:0-lstm_cell_9/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/MatMul_1?
lstm_cell_9/addAddV2lstm_cell_9/MatMul:product:0lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_cell_9/add?
"lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_9_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_9/BiasAdd/ReadVariableOp?
lstm_cell_9/BiasAddBiasAddlstm_cell_9/add:z:0*lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/BiasAdd|
lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_9/split/split_dim?
lstm_cell_9/splitSplit$lstm_cell_9/split/split_dim:output:0lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_9/splitk
lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Consto
lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_1?
lstm_cell_9/MulMullstm_cell_9/split:output:0lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul?
lstm_cell_9/Add_1AddV2lstm_cell_9/Mul:z:0lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_1?
#lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_9/clip_by_value/Minimum/y?
!lstm_cell_9/clip_by_value/MinimumMinimumlstm_cell_9/Add_1:z:0,lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22#
!lstm_cell_9/clip_by_value/Minimum
lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value/y?
lstm_cell_9/clip_by_valueMaximum%lstm_cell_9/clip_by_value/Minimum:z:0$lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_valueo
lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Const_2o
lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_3?
lstm_cell_9/Mul_1Mullstm_cell_9/split:output:1lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul_1?
lstm_cell_9/Add_2AddV2lstm_cell_9/Mul_1:z:0lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_2?
%lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_9/clip_by_value_1/Minimum/y?
#lstm_cell_9/clip_by_value_1/MinimumMinimumlstm_cell_9/Add_2:z:0.lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_9/clip_by_value_1/Minimum?
lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value_1/y?
lstm_cell_9/clip_by_value_1Maximum'lstm_cell_9/clip_by_value_1/Minimum:z:0&lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_value_1?
 lstm_cell_9/mul_2/ReadVariableOpReadVariableOp)lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02"
 lstm_cell_9/mul_2/ReadVariableOp?
lstm_cell_9/mul_2Mullstm_cell_9/clip_by_value_1:z:0(lstm_cell_9/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_2q
lstm_cell_9/TanhTanhlstm_cell_9/split:output:2*
T0*
_output_shapes

:22
lstm_cell_9/Tanh?
lstm_cell_9/mul_3Mullstm_cell_9/clip_by_value:z:0lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_3?
lstm_cell_9/add_3AddV2lstm_cell_9/mul_2:z:0lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_9/add_3o
lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Const_4o
lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_5?
lstm_cell_9/Mul_4Mullstm_cell_9/split:output:3lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul_4?
lstm_cell_9/Add_4AddV2lstm_cell_9/Mul_4:z:0lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_4?
%lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_9/clip_by_value_2/Minimum/y?
#lstm_cell_9/clip_by_value_2/MinimumMinimumlstm_cell_9/Add_4:z:0.lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_9/clip_by_value_2/Minimum?
lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value_2/y?
lstm_cell_9/clip_by_value_2Maximum'lstm_cell_9/clip_by_value_2/Minimum:z:0&lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_value_2p
lstm_cell_9/Tanh_1Tanhlstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_9/Tanh_1?
lstm_cell_9/mul_5Mullstm_cell_9/clip_by_value_2:z:0lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time?
ReadVariableOpReadVariableOp,lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_9_matmul_readvariableop_resource.lstm_cell_9_matmul_1_readvariableop_1_resource+lstm_cell_9_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_45497*
condR
while_cond_45496*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_9_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_9/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_9_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_9/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_9/BiasAdd/ReadVariableOp"^lstm_cell_9/MatMul/ReadVariableOp$^lstm_cell_9/MatMul_1/ReadVariableOp&^lstm_cell_9/MatMul_1/ReadVariableOp_1!^lstm_cell_9/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:
: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_9/BiasAdd/ReadVariableOp"lstm_cell_9/BiasAdd/ReadVariableOp2F
!lstm_cell_9/MatMul/ReadVariableOp!lstm_cell_9/MatMul/ReadVariableOp2J
#lstm_cell_9/MatMul_1/ReadVariableOp#lstm_cell_9/MatMul_1/ReadVariableOp2N
%lstm_cell_9/MatMul_1/ReadVariableOp_1%lstm_cell_9/MatMul_1/ReadVariableOp_12D
 lstm_cell_9/mul_2/ReadVariableOp lstm_cell_9/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:

 
_user_specified_nameinputs
?X
?
while_body_48326
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_8_matmul_readvariableop_resource_0:	2?G
4while_lstm_cell_8_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_8_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_8_matmul_readvariableop_resource:	2?E
2while_lstm_cell_8_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_8_biasadd_readvariableop_resource:	???(while/lstm_cell_8/BiasAdd/ReadVariableOp?'while/lstm_cell_8/MatMul/ReadVariableOp?)while/lstm_cell_8/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_8/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_8_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02)
'while/lstm_cell_8/MatMul/ReadVariableOp?
while/lstm_cell_8/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/MatMul?
)while/lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_8_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_8/MatMul_1/ReadVariableOp?
while/lstm_cell_8/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_8/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/MatMul_1?
while/lstm_cell_8/addAddV2"while/lstm_cell_8/MatMul:product:0$while/lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/add?
(while/lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_8_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_8/BiasAdd/ReadVariableOp?
while/lstm_cell_8/BiasAddBiasAddwhile/lstm_cell_8/add:z:00while/lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/BiasAdd?
!while/lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_8/split/split_dim?
while/lstm_cell_8/splitSplit*while/lstm_cell_8/split/split_dim:output:0"while/lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_8/splitw
while/lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const{
while/lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_1?
while/lstm_cell_8/MulMul while/lstm_cell_8/split:output:0 while/lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul?
while/lstm_cell_8/Add_1AddV2while/lstm_cell_8/Mul:z:0"while/lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_1?
)while/lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_8/clip_by_value/Minimum/y?
'while/lstm_cell_8/clip_by_value/MinimumMinimumwhile/lstm_cell_8/Add_1:z:02while/lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22)
'while/lstm_cell_8/clip_by_value/Minimum?
!while/lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_8/clip_by_value/y?
while/lstm_cell_8/clip_by_valueMaximum+while/lstm_cell_8/clip_by_value/Minimum:z:0*while/lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22!
while/lstm_cell_8/clip_by_value{
while/lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const_2{
while/lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_3?
while/lstm_cell_8/Mul_1Mul while/lstm_cell_8/split:output:1"while/lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul_1?
while/lstm_cell_8/Add_2AddV2while/lstm_cell_8/Mul_1:z:0"while/lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_2?
+while/lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_8/clip_by_value_1/Minimum/y?
)while/lstm_cell_8/clip_by_value_1/MinimumMinimumwhile/lstm_cell_8/Add_2:z:04while/lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_8/clip_by_value_1/Minimum?
#while/lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_8/clip_by_value_1/y?
!while/lstm_cell_8/clip_by_value_1Maximum-while/lstm_cell_8/clip_by_value_1/Minimum:z:0,while/lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_8/clip_by_value_1?
while/lstm_cell_8/mul_2Mul%while/lstm_cell_8/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_2?
while/lstm_cell_8/TanhTanh while/lstm_cell_8/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_8/Tanh?
while/lstm_cell_8/mul_3Mul#while/lstm_cell_8/clip_by_value:z:0while/lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_3?
while/lstm_cell_8/add_3AddV2while/lstm_cell_8/mul_2:z:0while/lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_8/add_3{
while/lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const_4{
while/lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_5?
while/lstm_cell_8/Mul_4Mul while/lstm_cell_8/split:output:3"while/lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul_4?
while/lstm_cell_8/Add_4AddV2while/lstm_cell_8/Mul_4:z:0"while/lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_4?
+while/lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_8/clip_by_value_2/Minimum/y?
)while/lstm_cell_8/clip_by_value_2/MinimumMinimumwhile/lstm_cell_8/Add_4:z:04while/lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_8/clip_by_value_2/Minimum?
#while/lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_8/clip_by_value_2/y?
!while/lstm_cell_8/clip_by_value_2Maximum-while/lstm_cell_8/clip_by_value_2/Minimum:z:0,while/lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_8/clip_by_value_2?
while/lstm_cell_8/Tanh_1Tanhwhile/lstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Tanh_1?
while/lstm_cell_8/mul_5Mul%while/lstm_cell_8/clip_by_value_2:z:0while/lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_8/mul_5:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_8/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_8/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_8/BiasAdd/ReadVariableOp(^while/lstm_cell_8/MatMul/ReadVariableOp*^while/lstm_cell_8/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"h
1while_lstm_cell_8_biasadd_readvariableop_resource3while_lstm_cell_8_biasadd_readvariableop_resource_0"j
2while_lstm_cell_8_matmul_1_readvariableop_resource4while_lstm_cell_8_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_8_matmul_readvariableop_resource2while_lstm_cell_8_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2T
(while/lstm_cell_8/BiasAdd/ReadVariableOp(while/lstm_cell_8/BiasAdd/ReadVariableOp2R
'while/lstm_cell_8/MatMul/ReadVariableOp'while/lstm_cell_8/MatMul/ReadVariableOp2V
)while/lstm_cell_8/MatMul_1/ReadVariableOp)while/lstm_cell_8/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?e
?

lstm_7_while_body_46590*
&lstm_7_while_lstm_7_while_loop_counter0
,lstm_7_while_lstm_7_while_maximum_iterations
lstm_7_while_placeholder
lstm_7_while_placeholder_1
lstm_7_while_placeholder_2
lstm_7_while_placeholder_3'
#lstm_7_while_lstm_7_strided_slice_0e
alstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor_0L
9lstm_7_while_lstm_cell_9_matmul_readvariableop_resource_0:	?N
;lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource_0:	2?I
:lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource_0:	?
lstm_7_while_identity
lstm_7_while_identity_1
lstm_7_while_identity_2
lstm_7_while_identity_3
lstm_7_while_identity_4
lstm_7_while_identity_5%
!lstm_7_while_lstm_7_strided_slicec
_lstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensorJ
7lstm_7_while_lstm_cell_9_matmul_readvariableop_resource:	?L
9lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource:	2?G
8lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource:	???/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp?.lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp?0lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp?
>lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      2@
>lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shape?
0lstm_7/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemalstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor_0lstm_7_while_placeholderGlstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype022
0lstm_7/while/TensorArrayV2Read/TensorListGetItem?
.lstm_7/while/lstm_cell_9/MatMul/ReadVariableOpReadVariableOp9lstm_7_while_lstm_cell_9_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype020
.lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp?
lstm_7/while/lstm_cell_9/MatMulMatMul7lstm_7/while/TensorArrayV2Read/TensorListGetItem:item:06lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2!
lstm_7/while/lstm_cell_9/MatMul?
0lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp;lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype022
0lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp?
!lstm_7/while/lstm_cell_9/MatMul_1MatMullstm_7_while_placeholder_28lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2#
!lstm_7/while/lstm_cell_9/MatMul_1?
lstm_7/while/lstm_cell_9/addAddV2)lstm_7/while/lstm_cell_9/MatMul:product:0+lstm_7/while/lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_7/while/lstm_cell_9/add?
/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp:lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype021
/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp?
 lstm_7/while/lstm_cell_9/BiasAddBiasAdd lstm_7/while/lstm_cell_9/add:z:07lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2"
 lstm_7/while/lstm_cell_9/BiasAdd?
(lstm_7/while/lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2*
(lstm_7/while/lstm_cell_9/split/split_dim?
lstm_7/while/lstm_cell_9/splitSplit1lstm_7/while/lstm_cell_9/split/split_dim:output:0)lstm_7/while/lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2 
lstm_7/while/lstm_cell_9/split?
lstm_7/while/lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2 
lstm_7/while/lstm_cell_9/Const?
 lstm_7/while/lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_7/while/lstm_cell_9/Const_1?
lstm_7/while/lstm_cell_9/MulMul'lstm_7/while/lstm_cell_9/split:output:0'lstm_7/while/lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
lstm_7/while/lstm_cell_9/Mul?
lstm_7/while/lstm_cell_9/Add_1AddV2 lstm_7/while/lstm_cell_9/Mul:z:0)lstm_7/while/lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/Add_1?
0lstm_7/while/lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??22
0lstm_7/while/lstm_cell_9/clip_by_value/Minimum/y?
.lstm_7/while/lstm_cell_9/clip_by_value/MinimumMinimum"lstm_7/while/lstm_cell_9/Add_1:z:09lstm_7/while/lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:220
.lstm_7/while/lstm_cell_9/clip_by_value/Minimum?
(lstm_7/while/lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2*
(lstm_7/while/lstm_cell_9/clip_by_value/y?
&lstm_7/while/lstm_cell_9/clip_by_valueMaximum2lstm_7/while/lstm_cell_9/clip_by_value/Minimum:z:01lstm_7/while/lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22(
&lstm_7/while/lstm_cell_9/clip_by_value?
 lstm_7/while/lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_7/while/lstm_cell_9/Const_2?
 lstm_7/while/lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_7/while/lstm_cell_9/Const_3?
lstm_7/while/lstm_cell_9/Mul_1Mul'lstm_7/while/lstm_cell_9/split:output:1)lstm_7/while/lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/Mul_1?
lstm_7/while/lstm_cell_9/Add_2AddV2"lstm_7/while/lstm_cell_9/Mul_1:z:0)lstm_7/while/lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/Add_2?
2lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum/y?
0lstm_7/while/lstm_cell_9/clip_by_value_1/MinimumMinimum"lstm_7/while/lstm_cell_9/Add_2:z:0;lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222
0lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum?
*lstm_7/while/lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_7/while/lstm_cell_9/clip_by_value_1/y?
(lstm_7/while/lstm_cell_9/clip_by_value_1Maximum4lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum:z:03lstm_7/while/lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22*
(lstm_7/while/lstm_cell_9/clip_by_value_1?
lstm_7/while/lstm_cell_9/mul_2Mul,lstm_7/while/lstm_cell_9/clip_by_value_1:z:0lstm_7_while_placeholder_3*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/mul_2?
lstm_7/while/lstm_cell_9/TanhTanh'lstm_7/while/lstm_cell_9/split:output:2*
T0*
_output_shapes

:22
lstm_7/while/lstm_cell_9/Tanh?
lstm_7/while/lstm_cell_9/mul_3Mul*lstm_7/while/lstm_cell_9/clip_by_value:z:0!lstm_7/while/lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/mul_3?
lstm_7/while/lstm_cell_9/add_3AddV2"lstm_7/while/lstm_cell_9/mul_2:z:0"lstm_7/while/lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/add_3?
 lstm_7/while/lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_7/while/lstm_cell_9/Const_4?
 lstm_7/while/lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_7/while/lstm_cell_9/Const_5?
lstm_7/while/lstm_cell_9/Mul_4Mul'lstm_7/while/lstm_cell_9/split:output:3)lstm_7/while/lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/Mul_4?
lstm_7/while/lstm_cell_9/Add_4AddV2"lstm_7/while/lstm_cell_9/Mul_4:z:0)lstm_7/while/lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/Add_4?
2lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum/y?
0lstm_7/while/lstm_cell_9/clip_by_value_2/MinimumMinimum"lstm_7/while/lstm_cell_9/Add_4:z:0;lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222
0lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum?
*lstm_7/while/lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_7/while/lstm_cell_9/clip_by_value_2/y?
(lstm_7/while/lstm_cell_9/clip_by_value_2Maximum4lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum:z:03lstm_7/while/lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22*
(lstm_7/while/lstm_cell_9/clip_by_value_2?
lstm_7/while/lstm_cell_9/Tanh_1Tanh"lstm_7/while/lstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22!
lstm_7/while/lstm_cell_9/Tanh_1?
lstm_7/while/lstm_cell_9/mul_5Mul,lstm_7/while/lstm_cell_9/clip_by_value_2:z:0#lstm_7/while/lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/mul_5?
1lstm_7/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_7_while_placeholder_1lstm_7_while_placeholder"lstm_7/while/lstm_cell_9/mul_5:z:0*
_output_shapes
: *
element_dtype023
1lstm_7/while/TensorArrayV2Write/TensorListSetItemj
lstm_7/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/while/add/y?
lstm_7/while/addAddV2lstm_7_while_placeholderlstm_7/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_7/while/addn
lstm_7/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/while/add_1/y?
lstm_7/while/add_1AddV2&lstm_7_while_lstm_7_while_loop_counterlstm_7/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_7/while/add_1?
lstm_7/while/IdentityIdentitylstm_7/while/add_1:z:0^lstm_7/while/NoOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity?
lstm_7/while/Identity_1Identity,lstm_7_while_lstm_7_while_maximum_iterations^lstm_7/while/NoOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity_1?
lstm_7/while/Identity_2Identitylstm_7/while/add:z:0^lstm_7/while/NoOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity_2?
lstm_7/while/Identity_3IdentityAlstm_7/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_7/while/NoOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity_3?
lstm_7/while/Identity_4Identity"lstm_7/while/lstm_cell_9/mul_5:z:0^lstm_7/while/NoOp*
T0*
_output_shapes

:22
lstm_7/while/Identity_4?
lstm_7/while/Identity_5Identity"lstm_7/while/lstm_cell_9/add_3:z:0^lstm_7/while/NoOp*
T0*
_output_shapes

:22
lstm_7/while/Identity_5?
lstm_7/while/NoOpNoOp0^lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp/^lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp1^lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_7/while/NoOp"7
lstm_7_while_identitylstm_7/while/Identity:output:0";
lstm_7_while_identity_1 lstm_7/while/Identity_1:output:0";
lstm_7_while_identity_2 lstm_7/while/Identity_2:output:0";
lstm_7_while_identity_3 lstm_7/while/Identity_3:output:0";
lstm_7_while_identity_4 lstm_7/while/Identity_4:output:0";
lstm_7_while_identity_5 lstm_7/while/Identity_5:output:0"H
!lstm_7_while_lstm_7_strided_slice#lstm_7_while_lstm_7_strided_slice_0"v
8lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource:lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource_0"x
9lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource;lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource_0"t
7lstm_7_while_lstm_cell_9_matmul_readvariableop_resource9lstm_7_while_lstm_cell_9_matmul_readvariableop_resource_0"?
_lstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensoralstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2b
/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp2`
.lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp.lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp2d
0lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp0lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?
?
&__inference_lstm_6_layer_call_fn_48832

inputs
unknown:	2?
	unknown_0:2
	unknown_1:	2?
	unknown_2:	?
	unknown_3:2
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_6_layer_call_and_return_conditional_losses_457912
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:
2: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
?e
?

lstm_6_while_body_46764*
&lstm_6_while_lstm_6_while_loop_counter0
,lstm_6_while_lstm_6_while_maximum_iterations
lstm_6_while_placeholder
lstm_6_while_placeholder_1
lstm_6_while_placeholder_2
lstm_6_while_placeholder_3'
#lstm_6_while_lstm_6_strided_slice_0e
alstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor_0L
9lstm_6_while_lstm_cell_8_matmul_readvariableop_resource_0:	2?N
;lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource_0:	2?I
:lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource_0:	?
lstm_6_while_identity
lstm_6_while_identity_1
lstm_6_while_identity_2
lstm_6_while_identity_3
lstm_6_while_identity_4
lstm_6_while_identity_5%
!lstm_6_while_lstm_6_strided_slicec
_lstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensorJ
7lstm_6_while_lstm_cell_8_matmul_readvariableop_resource:	2?L
9lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource:	2?G
8lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource:	???/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp?.lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp?0lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp?
>lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2@
>lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shape?
0lstm_6/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemalstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor_0lstm_6_while_placeholderGlstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype022
0lstm_6/while/TensorArrayV2Read/TensorListGetItem?
.lstm_6/while/lstm_cell_8/MatMul/ReadVariableOpReadVariableOp9lstm_6_while_lstm_cell_8_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype020
.lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp?
lstm_6/while/lstm_cell_8/MatMulMatMul7lstm_6/while/TensorArrayV2Read/TensorListGetItem:item:06lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2!
lstm_6/while/lstm_cell_8/MatMul?
0lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp;lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype022
0lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp?
!lstm_6/while/lstm_cell_8/MatMul_1MatMullstm_6_while_placeholder_28lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2#
!lstm_6/while/lstm_cell_8/MatMul_1?
lstm_6/while/lstm_cell_8/addAddV2)lstm_6/while/lstm_cell_8/MatMul:product:0+lstm_6/while/lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_6/while/lstm_cell_8/add?
/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp:lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype021
/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp?
 lstm_6/while/lstm_cell_8/BiasAddBiasAdd lstm_6/while/lstm_cell_8/add:z:07lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2"
 lstm_6/while/lstm_cell_8/BiasAdd?
(lstm_6/while/lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2*
(lstm_6/while/lstm_cell_8/split/split_dim?
lstm_6/while/lstm_cell_8/splitSplit1lstm_6/while/lstm_cell_8/split/split_dim:output:0)lstm_6/while/lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2 
lstm_6/while/lstm_cell_8/split?
lstm_6/while/lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2 
lstm_6/while/lstm_cell_8/Const?
 lstm_6/while/lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_6/while/lstm_cell_8/Const_1?
lstm_6/while/lstm_cell_8/MulMul'lstm_6/while/lstm_cell_8/split:output:0'lstm_6/while/lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
lstm_6/while/lstm_cell_8/Mul?
lstm_6/while/lstm_cell_8/Add_1AddV2 lstm_6/while/lstm_cell_8/Mul:z:0)lstm_6/while/lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/Add_1?
0lstm_6/while/lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??22
0lstm_6/while/lstm_cell_8/clip_by_value/Minimum/y?
.lstm_6/while/lstm_cell_8/clip_by_value/MinimumMinimum"lstm_6/while/lstm_cell_8/Add_1:z:09lstm_6/while/lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:220
.lstm_6/while/lstm_cell_8/clip_by_value/Minimum?
(lstm_6/while/lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2*
(lstm_6/while/lstm_cell_8/clip_by_value/y?
&lstm_6/while/lstm_cell_8/clip_by_valueMaximum2lstm_6/while/lstm_cell_8/clip_by_value/Minimum:z:01lstm_6/while/lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22(
&lstm_6/while/lstm_cell_8/clip_by_value?
 lstm_6/while/lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_6/while/lstm_cell_8/Const_2?
 lstm_6/while/lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_6/while/lstm_cell_8/Const_3?
lstm_6/while/lstm_cell_8/Mul_1Mul'lstm_6/while/lstm_cell_8/split:output:1)lstm_6/while/lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/Mul_1?
lstm_6/while/lstm_cell_8/Add_2AddV2"lstm_6/while/lstm_cell_8/Mul_1:z:0)lstm_6/while/lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/Add_2?
2lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum/y?
0lstm_6/while/lstm_cell_8/clip_by_value_1/MinimumMinimum"lstm_6/while/lstm_cell_8/Add_2:z:0;lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222
0lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum?
*lstm_6/while/lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_6/while/lstm_cell_8/clip_by_value_1/y?
(lstm_6/while/lstm_cell_8/clip_by_value_1Maximum4lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum:z:03lstm_6/while/lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22*
(lstm_6/while/lstm_cell_8/clip_by_value_1?
lstm_6/while/lstm_cell_8/mul_2Mul,lstm_6/while/lstm_cell_8/clip_by_value_1:z:0lstm_6_while_placeholder_3*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/mul_2?
lstm_6/while/lstm_cell_8/TanhTanh'lstm_6/while/lstm_cell_8/split:output:2*
T0*
_output_shapes

:22
lstm_6/while/lstm_cell_8/Tanh?
lstm_6/while/lstm_cell_8/mul_3Mul*lstm_6/while/lstm_cell_8/clip_by_value:z:0!lstm_6/while/lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/mul_3?
lstm_6/while/lstm_cell_8/add_3AddV2"lstm_6/while/lstm_cell_8/mul_2:z:0"lstm_6/while/lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/add_3?
 lstm_6/while/lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_6/while/lstm_cell_8/Const_4?
 lstm_6/while/lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_6/while/lstm_cell_8/Const_5?
lstm_6/while/lstm_cell_8/Mul_4Mul'lstm_6/while/lstm_cell_8/split:output:3)lstm_6/while/lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/Mul_4?
lstm_6/while/lstm_cell_8/Add_4AddV2"lstm_6/while/lstm_cell_8/Mul_4:z:0)lstm_6/while/lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/Add_4?
2lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum/y?
0lstm_6/while/lstm_cell_8/clip_by_value_2/MinimumMinimum"lstm_6/while/lstm_cell_8/Add_4:z:0;lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222
0lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum?
*lstm_6/while/lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_6/while/lstm_cell_8/clip_by_value_2/y?
(lstm_6/while/lstm_cell_8/clip_by_value_2Maximum4lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum:z:03lstm_6/while/lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22*
(lstm_6/while/lstm_cell_8/clip_by_value_2?
lstm_6/while/lstm_cell_8/Tanh_1Tanh"lstm_6/while/lstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22!
lstm_6/while/lstm_cell_8/Tanh_1?
lstm_6/while/lstm_cell_8/mul_5Mul,lstm_6/while/lstm_cell_8/clip_by_value_2:z:0#lstm_6/while/lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22 
lstm_6/while/lstm_cell_8/mul_5?
1lstm_6/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_6_while_placeholder_1lstm_6_while_placeholder"lstm_6/while/lstm_cell_8/mul_5:z:0*
_output_shapes
: *
element_dtype023
1lstm_6/while/TensorArrayV2Write/TensorListSetItemj
lstm_6/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/while/add/y?
lstm_6/while/addAddV2lstm_6_while_placeholderlstm_6/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_6/while/addn
lstm_6/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/while/add_1/y?
lstm_6/while/add_1AddV2&lstm_6_while_lstm_6_while_loop_counterlstm_6/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_6/while/add_1?
lstm_6/while/IdentityIdentitylstm_6/while/add_1:z:0^lstm_6/while/NoOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity?
lstm_6/while/Identity_1Identity,lstm_6_while_lstm_6_while_maximum_iterations^lstm_6/while/NoOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity_1?
lstm_6/while/Identity_2Identitylstm_6/while/add:z:0^lstm_6/while/NoOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity_2?
lstm_6/while/Identity_3IdentityAlstm_6/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_6/while/NoOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity_3?
lstm_6/while/Identity_4Identity"lstm_6/while/lstm_cell_8/mul_5:z:0^lstm_6/while/NoOp*
T0*
_output_shapes

:22
lstm_6/while/Identity_4?
lstm_6/while/Identity_5Identity"lstm_6/while/lstm_cell_8/add_3:z:0^lstm_6/while/NoOp*
T0*
_output_shapes

:22
lstm_6/while/Identity_5?
lstm_6/while/NoOpNoOp0^lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp/^lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp1^lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_6/while/NoOp"7
lstm_6_while_identitylstm_6/while/Identity:output:0";
lstm_6_while_identity_1 lstm_6/while/Identity_1:output:0";
lstm_6_while_identity_2 lstm_6/while/Identity_2:output:0";
lstm_6_while_identity_3 lstm_6/while/Identity_3:output:0";
lstm_6_while_identity_4 lstm_6/while/Identity_4:output:0";
lstm_6_while_identity_5 lstm_6/while/Identity_5:output:0"H
!lstm_6_while_lstm_6_strided_slice#lstm_6_while_lstm_6_strided_slice_0"v
8lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource:lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource_0"x
9lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource;lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource_0"t
7lstm_6_while_lstm_cell_8_matmul_readvariableop_resource9lstm_6_while_lstm_cell_8_matmul_readvariableop_resource_0"?
_lstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensoralstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2b
/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp2`
.lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp.lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp2d
0lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp0lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?m
?
A__inference_lstm_6_layer_call_and_return_conditional_losses_48787

inputs=
*lstm_cell_8_matmul_readvariableop_resource:	2?>
,lstm_cell_8_matmul_1_readvariableop_resource:2A
.lstm_cell_8_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_8_biasadd_readvariableop_resource:	?;
)lstm_cell_8_mul_2_readvariableop_resource:2
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_8/BiasAdd/ReadVariableOp?!lstm_cell_8/MatMul/ReadVariableOp?#lstm_cell_8/MatMul_1/ReadVariableOp?%lstm_cell_8/MatMul_1/ReadVariableOp_1? lstm_cell_8/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permq
	transpose	Transposeinputstranspose/perm:output:0*
T0*"
_output_shapes
:
22
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
      2   2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_8/MatMul/ReadVariableOpReadVariableOp*lstm_cell_8_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02#
!lstm_cell_8/MatMul/ReadVariableOp?
lstm_cell_8/MatMulMatMulstrided_slice_1:output:0)lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/MatMul?
#lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02%
#lstm_cell_8/MatMul_1/ReadVariableOp?
%lstm_cell_8/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_8_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_8/MatMul_1/ReadVariableOp_1?
lstm_cell_8/MatMul_1MatMul+lstm_cell_8/MatMul_1/ReadVariableOp:value:0-lstm_cell_8/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/MatMul_1?
lstm_cell_8/addAddV2lstm_cell_8/MatMul:product:0lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_cell_8/add?
"lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_8_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_8/BiasAdd/ReadVariableOp?
lstm_cell_8/BiasAddBiasAddlstm_cell_8/add:z:0*lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/BiasAdd|
lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_8/split/split_dim?
lstm_cell_8/splitSplit$lstm_cell_8/split/split_dim:output:0lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_8/splitk
lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Consto
lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_1?
lstm_cell_8/MulMullstm_cell_8/split:output:0lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul?
lstm_cell_8/Add_1AddV2lstm_cell_8/Mul:z:0lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_1?
#lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_8/clip_by_value/Minimum/y?
!lstm_cell_8/clip_by_value/MinimumMinimumlstm_cell_8/Add_1:z:0,lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22#
!lstm_cell_8/clip_by_value/Minimum
lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value/y?
lstm_cell_8/clip_by_valueMaximum%lstm_cell_8/clip_by_value/Minimum:z:0$lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_valueo
lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Const_2o
lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_3?
lstm_cell_8/Mul_1Mullstm_cell_8/split:output:1lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul_1?
lstm_cell_8/Add_2AddV2lstm_cell_8/Mul_1:z:0lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_2?
%lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_8/clip_by_value_1/Minimum/y?
#lstm_cell_8/clip_by_value_1/MinimumMinimumlstm_cell_8/Add_2:z:0.lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_8/clip_by_value_1/Minimum?
lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value_1/y?
lstm_cell_8/clip_by_value_1Maximum'lstm_cell_8/clip_by_value_1/Minimum:z:0&lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_value_1?
 lstm_cell_8/mul_2/ReadVariableOpReadVariableOp)lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02"
 lstm_cell_8/mul_2/ReadVariableOp?
lstm_cell_8/mul_2Mullstm_cell_8/clip_by_value_1:z:0(lstm_cell_8/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_2q
lstm_cell_8/TanhTanhlstm_cell_8/split:output:2*
T0*
_output_shapes

:22
lstm_cell_8/Tanh?
lstm_cell_8/mul_3Mullstm_cell_8/clip_by_value:z:0lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_3?
lstm_cell_8/add_3AddV2lstm_cell_8/mul_2:z:0lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_8/add_3o
lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Const_4o
lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_5?
lstm_cell_8/Mul_4Mullstm_cell_8/split:output:3lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul_4?
lstm_cell_8/Add_4AddV2lstm_cell_8/Mul_4:z:0lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_4?
%lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_8/clip_by_value_2/Minimum/y?
#lstm_cell_8/clip_by_value_2/MinimumMinimumlstm_cell_8/Add_4:z:0.lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_8/clip_by_value_2/Minimum?
lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value_2/y?
lstm_cell_8/clip_by_value_2Maximum'lstm_cell_8/clip_by_value_2/Minimum:z:0&lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_value_2p
lstm_cell_8/Tanh_1Tanhlstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_8/Tanh_1?
lstm_cell_8/mul_5Mullstm_cell_8/clip_by_value_2:z:0lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time?
ReadVariableOpReadVariableOp,lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_8_matmul_readvariableop_resource.lstm_cell_8_matmul_1_readvariableop_1_resource+lstm_cell_8_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_48682*
condR
while_cond_48681*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_8_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_8/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_8_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_8/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_8/BiasAdd/ReadVariableOp"^lstm_cell_8/MatMul/ReadVariableOp$^lstm_cell_8/MatMul_1/ReadVariableOp&^lstm_cell_8/MatMul_1/ReadVariableOp_1!^lstm_cell_8/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:
2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_8/BiasAdd/ReadVariableOp"lstm_cell_8/BiasAdd/ReadVariableOp2F
!lstm_cell_8/MatMul/ReadVariableOp!lstm_cell_8/MatMul/ReadVariableOp2J
#lstm_cell_8/MatMul_1/ReadVariableOp#lstm_cell_8/MatMul_1/ReadVariableOp2N
%lstm_cell_8/MatMul_1/ReadVariableOp_1%lstm_cell_8/MatMul_1/ReadVariableOp_12D
 lstm_cell_8/mul_2/ReadVariableOp lstm_cell_8/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
?,
?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_44676

inputs

states
states_11
matmul_readvariableop_resource:	2?3
 matmul_1_readvariableop_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1Z
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes

:22
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:22
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes

:22
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3`
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes

:22
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:22
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:22
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:22
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:22
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:22
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5`
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes

:22
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:22
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:22
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:22
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:22

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:2:2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:FB

_output_shapes

:2
 
_user_specified_namestates:FB

_output_shapes

:2
 
_user_specified_namestates
?$
?
while_body_44970
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0,
while_lstm_cell_8_44994_0:	2?,
while_lstm_cell_8_44996_0:	2?(
while_lstm_cell_8_44998_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor*
while_lstm_cell_8_44994:	2?*
while_lstm_cell_8_44996:	2?&
while_lstm_cell_8_44998:	???)while/lstm_cell_8/StatefulPartitionedCall?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
)while/lstm_cell_8/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_8_44994_0while_lstm_cell_8_44996_0while_lstm_cell_8_44998_0*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:2:2:2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_448102+
)while/lstm_cell_8/StatefulPartitionedCall?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder2while/lstm_cell_8/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identity2while/lstm_cell_8/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identity2while/lstm_cell_8/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp*^while/lstm_cell_8/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"4
while_lstm_cell_8_44994while_lstm_cell_8_44994_0"4
while_lstm_cell_8_44996while_lstm_cell_8_44996_0"4
while_lstm_cell_8_44998while_lstm_cell_8_44998_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_8/StatefulPartitionedCall)while/lstm_cell_8/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?9
?
A__inference_lstm_6_layer_call_and_return_conditional_losses_44722

inputs#
lstm_cell_8_44581:2#
lstm_cell_8_44583:2$
lstm_cell_8_44585:	2?$
lstm_cell_8_44587:	2? 
lstm_cell_8_44589:	?
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?#lstm_cell_8/StatefulPartitionedCall?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:?????????22
	transposeK
ShapeShapetranspose:y:0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1?
#lstm_cell_8/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_8_44581lstm_cell_8_44583lstm_cell_8_44585lstm_cell_8_44587lstm_cell_8_44589*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:2:2:2*'
_read_only_resource_inputs	
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_445802%
#lstm_cell_8/StatefulPartitionedCall?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
timer
ReadVariableOpReadVariableOplstm_cell_8_44581*
_output_shapes

:2*
dtype02
ReadVariableOpv
ReadVariableOp_1ReadVariableOplstm_cell_8_44583*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_8_44585lstm_cell_8_44587lstm_cell_8_44589*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_44600*
condR
while_cond_44599*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOplstm_cell_8_44581while:output:4^ReadVariableOp$^lstm_cell_8/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOplstm_cell_8_44583while:output:5^ReadVariableOp_1$^lstm_cell_8/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_8/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:?????????2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_8/StatefulPartitionedCall#lstm_cell_8/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:?????????2
 
_user_specified_nameinputs
?
?
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_45816

inputs8
&dense_3_matmul_readvariableop_resource:25
'dense_3_biasadd_readvariableop_resource:
identity??dense_3/BiasAdd/ReadVariableOp?dense_3/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2
Reshape/shapef
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes

:
22	
Reshape?
dense_3/MatMul/ReadVariableOpReadVariableOp&dense_3_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_3/MatMul/ReadVariableOp?
dense_3/MatMulMatMulReshape:output:0%dense_3/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_3/MatMul?
dense_3/BiasAdd/ReadVariableOpReadVariableOp'dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_3/BiasAdd/ReadVariableOp?
dense_3/BiasAddBiasAdddense_3/MatMul:product:0&dense_3/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_3/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      2
Reshape_1/shape?
	Reshape_1Reshapedense_3/BiasAdd:output:0Reshape_1/shape:output:0*
T0*"
_output_shapes
:
2
	Reshape_1h
IdentityIdentityReshape_1:output:0^NoOp*
T0*"
_output_shapes
:
2

Identity?
NoOpNoOp^dense_3/BiasAdd/ReadVariableOp^dense_3/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:
2: : 2@
dense_3/BiasAdd/ReadVariableOpdense_3/BiasAdd/ReadVariableOp2>
dense_3/MatMul/ReadVariableOpdense_3/MatMul/ReadVariableOp:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
?
?
while_cond_45973
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_45973___redundant_placeholder03
/while_while_cond_45973___redundant_placeholder13
/while_while_cond_45973___redundant_placeholder23
/while_while_cond_45973___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?
?
G__inference_sequential_3_layer_call_and_return_conditional_losses_46364

inputs
lstm_7_46334:	?
lstm_7_46336:2
lstm_7_46338:	2?
lstm_7_46340:	?
lstm_7_46342:2
lstm_6_46345:	2?
lstm_6_46347:2
lstm_6_46349:	2?
lstm_6_46351:	?
lstm_6_46353:2*
time_distributed_3_46356:2&
time_distributed_3_46358:
identity??lstm_6/StatefulPartitionedCall?lstm_7/StatefulPartitionedCall?*time_distributed_3/StatefulPartitionedCall?
lstm_7/StatefulPartitionedCallStatefulPartitionedCallinputslstm_7_46334lstm_7_46336lstm_7_46338lstm_7_46340lstm_7_46342*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_7_layer_call_and_return_conditional_losses_462872 
lstm_7/StatefulPartitionedCall?
lstm_6/StatefulPartitionedCallStatefulPartitionedCall'lstm_7/StatefulPartitionedCall:output:0lstm_6_46345lstm_6_46347lstm_6_46349lstm_6_46351lstm_6_46353*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_6_layer_call_and_return_conditional_losses_460792 
lstm_6/StatefulPartitionedCall?
*time_distributed_3/StatefulPartitionedCallStatefulPartitionedCall'lstm_6/StatefulPartitionedCall:output:0time_distributed_3_46356time_distributed_3_46358*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_458772,
*time_distributed_3/StatefulPartitionedCall?
 time_distributed_3/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2"
 time_distributed_3/Reshape/shape?
time_distributed_3/ReshapeReshape'lstm_6/StatefulPartitionedCall:output:0)time_distributed_3/Reshape/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_3/Reshape?
IdentityIdentity3time_distributed_3/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

Identity?
NoOpNoOp^lstm_6/StatefulPartitionedCall^lstm_7/StatefulPartitionedCall+^time_distributed_3/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 2@
lstm_6/StatefulPartitionedCalllstm_6/StatefulPartitionedCall2@
lstm_7/StatefulPartitionedCalllstm_7/StatefulPartitionedCall2X
*time_distributed_3/StatefulPartitionedCall*time_distributed_3/StatefulPartitionedCall:J F
"
_output_shapes
:

 
_user_specified_nameinputs
?$
?
while_body_44194
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0,
while_lstm_cell_9_44218_0:	?,
while_lstm_cell_9_44220_0:	2?(
while_lstm_cell_9_44222_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor*
while_lstm_cell_9_44218:	?*
while_lstm_cell_9_44220:	2?&
while_lstm_cell_9_44222:	???)while/lstm_cell_9/StatefulPartitionedCall?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
)while/lstm_cell_9/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_9_44218_0while_lstm_cell_9_44220_0while_lstm_cell_9_44222_0*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:2:2:2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_440342+
)while/lstm_cell_9/StatefulPartitionedCall?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder2while/lstm_cell_9/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identity2while/lstm_cell_9/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identity2while/lstm_cell_9/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp*^while/lstm_cell_9/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"4
while_lstm_cell_9_44218while_lstm_cell_9_44218_0"4
while_lstm_cell_9_44220while_lstm_cell_9_44220_0"4
while_lstm_cell_9_44222while_lstm_cell_9_44222_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_9/StatefulPartitionedCall)while/lstm_cell_9/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?X
?
while_body_47732
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_9_matmul_readvariableop_resource_0:	?G
4while_lstm_cell_9_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_9_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_9_matmul_readvariableop_resource:	?E
2while_lstm_cell_9_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_9_biasadd_readvariableop_resource:	???(while/lstm_cell_9/BiasAdd/ReadVariableOp?'while/lstm_cell_9/MatMul/ReadVariableOp?)while/lstm_cell_9/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_9/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_9_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02)
'while/lstm_cell_9/MatMul/ReadVariableOp?
while/lstm_cell_9/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/MatMul?
)while/lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_9_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_9/MatMul_1/ReadVariableOp?
while/lstm_cell_9/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_9/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/MatMul_1?
while/lstm_cell_9/addAddV2"while/lstm_cell_9/MatMul:product:0$while/lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/add?
(while/lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_9_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_9/BiasAdd/ReadVariableOp?
while/lstm_cell_9/BiasAddBiasAddwhile/lstm_cell_9/add:z:00while/lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/BiasAdd?
!while/lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_9/split/split_dim?
while/lstm_cell_9/splitSplit*while/lstm_cell_9/split/split_dim:output:0"while/lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_9/splitw
while/lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const{
while/lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_1?
while/lstm_cell_9/MulMul while/lstm_cell_9/split:output:0 while/lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul?
while/lstm_cell_9/Add_1AddV2while/lstm_cell_9/Mul:z:0"while/lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_1?
)while/lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_9/clip_by_value/Minimum/y?
'while/lstm_cell_9/clip_by_value/MinimumMinimumwhile/lstm_cell_9/Add_1:z:02while/lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22)
'while/lstm_cell_9/clip_by_value/Minimum?
!while/lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_9/clip_by_value/y?
while/lstm_cell_9/clip_by_valueMaximum+while/lstm_cell_9/clip_by_value/Minimum:z:0*while/lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22!
while/lstm_cell_9/clip_by_value{
while/lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const_2{
while/lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_3?
while/lstm_cell_9/Mul_1Mul while/lstm_cell_9/split:output:1"while/lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul_1?
while/lstm_cell_9/Add_2AddV2while/lstm_cell_9/Mul_1:z:0"while/lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_2?
+while/lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_9/clip_by_value_1/Minimum/y?
)while/lstm_cell_9/clip_by_value_1/MinimumMinimumwhile/lstm_cell_9/Add_2:z:04while/lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_9/clip_by_value_1/Minimum?
#while/lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_9/clip_by_value_1/y?
!while/lstm_cell_9/clip_by_value_1Maximum-while/lstm_cell_9/clip_by_value_1/Minimum:z:0,while/lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_9/clip_by_value_1?
while/lstm_cell_9/mul_2Mul%while/lstm_cell_9/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_2?
while/lstm_cell_9/TanhTanh while/lstm_cell_9/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_9/Tanh?
while/lstm_cell_9/mul_3Mul#while/lstm_cell_9/clip_by_value:z:0while/lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_3?
while/lstm_cell_9/add_3AddV2while/lstm_cell_9/mul_2:z:0while/lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_9/add_3{
while/lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const_4{
while/lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_5?
while/lstm_cell_9/Mul_4Mul while/lstm_cell_9/split:output:3"while/lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul_4?
while/lstm_cell_9/Add_4AddV2while/lstm_cell_9/Mul_4:z:0"while/lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_4?
+while/lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_9/clip_by_value_2/Minimum/y?
)while/lstm_cell_9/clip_by_value_2/MinimumMinimumwhile/lstm_cell_9/Add_4:z:04while/lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_9/clip_by_value_2/Minimum?
#while/lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_9/clip_by_value_2/y?
!while/lstm_cell_9/clip_by_value_2Maximum-while/lstm_cell_9/clip_by_value_2/Minimum:z:0,while/lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_9/clip_by_value_2?
while/lstm_cell_9/Tanh_1Tanhwhile/lstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Tanh_1?
while/lstm_cell_9/mul_5Mul%while/lstm_cell_9/clip_by_value_2:z:0while/lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_9/mul_5:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_9/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_9/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_9/BiasAdd/ReadVariableOp(^while/lstm_cell_9/MatMul/ReadVariableOp*^while/lstm_cell_9/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"h
1while_lstm_cell_9_biasadd_readvariableop_resource3while_lstm_cell_9_biasadd_readvariableop_resource_0"j
2while_lstm_cell_9_matmul_1_readvariableop_resource4while_lstm_cell_9_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_9_matmul_readvariableop_resource2while_lstm_cell_9_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2T
(while/lstm_cell_9/BiasAdd/ReadVariableOp(while/lstm_cell_9/BiasAdd/ReadVariableOp2R
'while/lstm_cell_9/MatMul/ReadVariableOp'while/lstm_cell_9/MatMul/ReadVariableOp2V
)while/lstm_cell_9/MatMul_1/ReadVariableOp)while/lstm_cell_9/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?X
?
while_body_47376
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_9_matmul_readvariableop_resource_0:	?G
4while_lstm_cell_9_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_9_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_9_matmul_readvariableop_resource:	?E
2while_lstm_cell_9_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_9_biasadd_readvariableop_resource:	???(while/lstm_cell_9/BiasAdd/ReadVariableOp?'while/lstm_cell_9/MatMul/ReadVariableOp?)while/lstm_cell_9/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_9/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_9_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02)
'while/lstm_cell_9/MatMul/ReadVariableOp?
while/lstm_cell_9/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/MatMul?
)while/lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_9_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_9/MatMul_1/ReadVariableOp?
while/lstm_cell_9/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_9/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/MatMul_1?
while/lstm_cell_9/addAddV2"while/lstm_cell_9/MatMul:product:0$while/lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/add?
(while/lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_9_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_9/BiasAdd/ReadVariableOp?
while/lstm_cell_9/BiasAddBiasAddwhile/lstm_cell_9/add:z:00while/lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/BiasAdd?
!while/lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_9/split/split_dim?
while/lstm_cell_9/splitSplit*while/lstm_cell_9/split/split_dim:output:0"while/lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_9/splitw
while/lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const{
while/lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_1?
while/lstm_cell_9/MulMul while/lstm_cell_9/split:output:0 while/lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul?
while/lstm_cell_9/Add_1AddV2while/lstm_cell_9/Mul:z:0"while/lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_1?
)while/lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_9/clip_by_value/Minimum/y?
'while/lstm_cell_9/clip_by_value/MinimumMinimumwhile/lstm_cell_9/Add_1:z:02while/lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22)
'while/lstm_cell_9/clip_by_value/Minimum?
!while/lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_9/clip_by_value/y?
while/lstm_cell_9/clip_by_valueMaximum+while/lstm_cell_9/clip_by_value/Minimum:z:0*while/lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22!
while/lstm_cell_9/clip_by_value{
while/lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const_2{
while/lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_3?
while/lstm_cell_9/Mul_1Mul while/lstm_cell_9/split:output:1"while/lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul_1?
while/lstm_cell_9/Add_2AddV2while/lstm_cell_9/Mul_1:z:0"while/lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_2?
+while/lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_9/clip_by_value_1/Minimum/y?
)while/lstm_cell_9/clip_by_value_1/MinimumMinimumwhile/lstm_cell_9/Add_2:z:04while/lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_9/clip_by_value_1/Minimum?
#while/lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_9/clip_by_value_1/y?
!while/lstm_cell_9/clip_by_value_1Maximum-while/lstm_cell_9/clip_by_value_1/Minimum:z:0,while/lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_9/clip_by_value_1?
while/lstm_cell_9/mul_2Mul%while/lstm_cell_9/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_2?
while/lstm_cell_9/TanhTanh while/lstm_cell_9/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_9/Tanh?
while/lstm_cell_9/mul_3Mul#while/lstm_cell_9/clip_by_value:z:0while/lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_3?
while/lstm_cell_9/add_3AddV2while/lstm_cell_9/mul_2:z:0while/lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_9/add_3{
while/lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const_4{
while/lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_5?
while/lstm_cell_9/Mul_4Mul while/lstm_cell_9/split:output:3"while/lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul_4?
while/lstm_cell_9/Add_4AddV2while/lstm_cell_9/Mul_4:z:0"while/lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_4?
+while/lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_9/clip_by_value_2/Minimum/y?
)while/lstm_cell_9/clip_by_value_2/MinimumMinimumwhile/lstm_cell_9/Add_4:z:04while/lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_9/clip_by_value_2/Minimum?
#while/lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_9/clip_by_value_2/y?
!while/lstm_cell_9/clip_by_value_2Maximum-while/lstm_cell_9/clip_by_value_2/Minimum:z:0,while/lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_9/clip_by_value_2?
while/lstm_cell_9/Tanh_1Tanhwhile/lstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Tanh_1?
while/lstm_cell_9/mul_5Mul%while/lstm_cell_9/clip_by_value_2:z:0while/lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_9/mul_5:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_9/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_9/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_9/BiasAdd/ReadVariableOp(^while/lstm_cell_9/MatMul/ReadVariableOp*^while/lstm_cell_9/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"h
1while_lstm_cell_9_biasadd_readvariableop_resource3while_lstm_cell_9_biasadd_readvariableop_resource_0"j
2while_lstm_cell_9_matmul_1_readvariableop_resource4while_lstm_cell_9_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_9_matmul_readvariableop_resource2while_lstm_cell_9_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2T
(while/lstm_cell_9/BiasAdd/ReadVariableOp(while/lstm_cell_9/BiasAdd/ReadVariableOp2R
'while/lstm_cell_9/MatMul/ReadVariableOp'while/lstm_cell_9/MatMul/ReadVariableOp2V
)while/lstm_cell_9/MatMul_1/ReadVariableOp)while/lstm_cell_9/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?9
?
A__inference_lstm_6_layer_call_and_return_conditional_losses_45039

inputs#
lstm_cell_8_44951:2#
lstm_cell_8_44953:2$
lstm_cell_8_44955:	2?$
lstm_cell_8_44957:	2? 
lstm_cell_8_44959:	?
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?#lstm_cell_8/StatefulPartitionedCall?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:?????????22
	transposeK
ShapeShapetranspose:y:0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1?
#lstm_cell_8/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_8_44951lstm_cell_8_44953lstm_cell_8_44955lstm_cell_8_44957lstm_cell_8_44959*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:2:2:2*'
_read_only_resource_inputs	
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_449012%
#lstm_cell_8/StatefulPartitionedCall?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
timer
ReadVariableOpReadVariableOplstm_cell_8_44951*
_output_shapes

:2*
dtype02
ReadVariableOpv
ReadVariableOp_1ReadVariableOplstm_cell_8_44953*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_8_44955lstm_cell_8_44957lstm_cell_8_44959*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_44970*
condR
while_cond_44969*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOplstm_cell_8_44951while:output:4^ReadVariableOp$^lstm_cell_8/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOplstm_cell_8_44953while:output:5^ReadVariableOp_1$^lstm_cell_8/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_8/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:?????????2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_8/StatefulPartitionedCall#lstm_cell_8/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:?????????2
 
_user_specified_nameinputs
?
?
while_cond_48147
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_48147___redundant_placeholder03
/while_while_cond_48147___redundant_placeholder13
/while_while_cond_48147___redundant_placeholder23
/while_while_cond_48147___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?
?
+__inference_lstm_cell_8_layer_call_fn_49592

inputs
states_0
states_1
unknown:	2?
	unknown_0:	2?
	unknown_1:	?
identity

identity_1

identity_2??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0states_1unknown	unknown_0	unknown_1*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:2:2:2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_446762
StatefulPartitionedCallr
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes

:22

Identityv

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*
_output_shapes

:22

Identity_1v

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*
_output_shapes

:22

Identity_2h
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:2:2:2: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:2
 
_user_specified_nameinputs:HD

_output_shapes

:2
"
_user_specified_name
states/0:HD

_output_shapes

:2
"
_user_specified_name
states/1
?
?
,__inference_sequential_3_layer_call_fn_45852
lstm_7_input
unknown:	?
	unknown_0:2
	unknown_1:	2?
	unknown_2:	?
	unknown_3:2
	unknown_4:	2?
	unknown_5:2
	unknown_6:	2?
	unknown_7:	?
	unknown_8:2
	unknown_9:2

unknown_10:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCalllstm_7_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_sequential_3_layer_call_and_return_conditional_losses_458252
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
"
_output_shapes
:

&
_user_specified_namelstm_7_input
?X
?
while_body_46182
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_9_matmul_readvariableop_resource_0:	?G
4while_lstm_cell_9_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_9_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_9_matmul_readvariableop_resource:	?E
2while_lstm_cell_9_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_9_biasadd_readvariableop_resource:	???(while/lstm_cell_9/BiasAdd/ReadVariableOp?'while/lstm_cell_9/MatMul/ReadVariableOp?)while/lstm_cell_9/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_9/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_9_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02)
'while/lstm_cell_9/MatMul/ReadVariableOp?
while/lstm_cell_9/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/MatMul?
)while/lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_9_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_9/MatMul_1/ReadVariableOp?
while/lstm_cell_9/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_9/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/MatMul_1?
while/lstm_cell_9/addAddV2"while/lstm_cell_9/MatMul:product:0$while/lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/add?
(while/lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_9_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_9/BiasAdd/ReadVariableOp?
while/lstm_cell_9/BiasAddBiasAddwhile/lstm_cell_9/add:z:00while/lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/BiasAdd?
!while/lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_9/split/split_dim?
while/lstm_cell_9/splitSplit*while/lstm_cell_9/split/split_dim:output:0"while/lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_9/splitw
while/lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const{
while/lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_1?
while/lstm_cell_9/MulMul while/lstm_cell_9/split:output:0 while/lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul?
while/lstm_cell_9/Add_1AddV2while/lstm_cell_9/Mul:z:0"while/lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_1?
)while/lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_9/clip_by_value/Minimum/y?
'while/lstm_cell_9/clip_by_value/MinimumMinimumwhile/lstm_cell_9/Add_1:z:02while/lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22)
'while/lstm_cell_9/clip_by_value/Minimum?
!while/lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_9/clip_by_value/y?
while/lstm_cell_9/clip_by_valueMaximum+while/lstm_cell_9/clip_by_value/Minimum:z:0*while/lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22!
while/lstm_cell_9/clip_by_value{
while/lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const_2{
while/lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_3?
while/lstm_cell_9/Mul_1Mul while/lstm_cell_9/split:output:1"while/lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul_1?
while/lstm_cell_9/Add_2AddV2while/lstm_cell_9/Mul_1:z:0"while/lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_2?
+while/lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_9/clip_by_value_1/Minimum/y?
)while/lstm_cell_9/clip_by_value_1/MinimumMinimumwhile/lstm_cell_9/Add_2:z:04while/lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_9/clip_by_value_1/Minimum?
#while/lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_9/clip_by_value_1/y?
!while/lstm_cell_9/clip_by_value_1Maximum-while/lstm_cell_9/clip_by_value_1/Minimum:z:0,while/lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_9/clip_by_value_1?
while/lstm_cell_9/mul_2Mul%while/lstm_cell_9/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_2?
while/lstm_cell_9/TanhTanh while/lstm_cell_9/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_9/Tanh?
while/lstm_cell_9/mul_3Mul#while/lstm_cell_9/clip_by_value:z:0while/lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_3?
while/lstm_cell_9/add_3AddV2while/lstm_cell_9/mul_2:z:0while/lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_9/add_3{
while/lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const_4{
while/lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_5?
while/lstm_cell_9/Mul_4Mul while/lstm_cell_9/split:output:3"while/lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul_4?
while/lstm_cell_9/Add_4AddV2while/lstm_cell_9/Mul_4:z:0"while/lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_4?
+while/lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_9/clip_by_value_2/Minimum/y?
)while/lstm_cell_9/clip_by_value_2/MinimumMinimumwhile/lstm_cell_9/Add_4:z:04while/lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_9/clip_by_value_2/Minimum?
#while/lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_9/clip_by_value_2/y?
!while/lstm_cell_9/clip_by_value_2Maximum-while/lstm_cell_9/clip_by_value_2/Minimum:z:0,while/lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_9/clip_by_value_2?
while/lstm_cell_9/Tanh_1Tanhwhile/lstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Tanh_1?
while/lstm_cell_9/mul_5Mul%while/lstm_cell_9/clip_by_value_2:z:0while/lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_9/mul_5:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_9/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_9/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_9/BiasAdd/ReadVariableOp(^while/lstm_cell_9/MatMul/ReadVariableOp*^while/lstm_cell_9/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"h
1while_lstm_cell_9_biasadd_readvariableop_resource3while_lstm_cell_9_biasadd_readvariableop_resource_0"j
2while_lstm_cell_9_matmul_1_readvariableop_resource4while_lstm_cell_9_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_9_matmul_readvariableop_resource2while_lstm_cell_9_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2T
(while/lstm_cell_9/BiasAdd/ReadVariableOp(while/lstm_cell_9/BiasAdd/ReadVariableOp2R
'while/lstm_cell_9/MatMul/ReadVariableOp'while/lstm_cell_9/MatMul/ReadVariableOp2V
)while/lstm_cell_9/MatMul_1/ReadVariableOp)while/lstm_cell_9/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?9
?
A__inference_lstm_7_layer_call_and_return_conditional_losses_44263

inputs#
lstm_cell_9_44175:2#
lstm_cell_9_44177:2$
lstm_cell_9_44179:	?$
lstm_cell_9_44181:	2? 
lstm_cell_9_44183:	?
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?#lstm_cell_9/StatefulPartitionedCall?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:?????????2
	transposeK
ShapeShapetranspose:y:0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1?
#lstm_cell_9/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_9_44175lstm_cell_9_44177lstm_cell_9_44179lstm_cell_9_44181lstm_cell_9_44183*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:2:2:2*'
_read_only_resource_inputs	
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_441252%
#lstm_cell_9/StatefulPartitionedCall?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
timer
ReadVariableOpReadVariableOplstm_cell_9_44175*
_output_shapes

:2*
dtype02
ReadVariableOpv
ReadVariableOp_1ReadVariableOplstm_cell_9_44177*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_9_44179lstm_cell_9_44181lstm_cell_9_44183*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_44194*
condR
while_cond_44193*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOplstm_cell_9_44175while:output:4^ReadVariableOp$^lstm_cell_9/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOplstm_cell_9_44177while:output:5^ReadVariableOp_1$^lstm_cell_9/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_9/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:?????????: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_9/StatefulPartitionedCall#lstm_cell_9/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:?????????
 
_user_specified_nameinputs
?,
?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_44810

inputs

states
states_11
matmul_readvariableop_resource:	2?3
 matmul_1_readvariableop_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1Z
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes

:22
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:22
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes

:22
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3`
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes

:22
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:22
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:22
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:22
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:22
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:22
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5`
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes

:22
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:22
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:22
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:22
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:22

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:2:2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:FB

_output_shapes

:2
 
_user_specified_namestates:FB

_output_shapes

:2
 
_user_specified_namestates
?m
?
A__inference_lstm_6_layer_call_and_return_conditional_losses_46079

inputs=
*lstm_cell_8_matmul_readvariableop_resource:	2?>
,lstm_cell_8_matmul_1_readvariableop_resource:2A
.lstm_cell_8_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_8_biasadd_readvariableop_resource:	?;
)lstm_cell_8_mul_2_readvariableop_resource:2
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_8/BiasAdd/ReadVariableOp?!lstm_cell_8/MatMul/ReadVariableOp?#lstm_cell_8/MatMul_1/ReadVariableOp?%lstm_cell_8/MatMul_1/ReadVariableOp_1? lstm_cell_8/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permq
	transpose	Transposeinputstranspose/perm:output:0*
T0*"
_output_shapes
:
22
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
      2   2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_8/MatMul/ReadVariableOpReadVariableOp*lstm_cell_8_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02#
!lstm_cell_8/MatMul/ReadVariableOp?
lstm_cell_8/MatMulMatMulstrided_slice_1:output:0)lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/MatMul?
#lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02%
#lstm_cell_8/MatMul_1/ReadVariableOp?
%lstm_cell_8/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_8_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_8/MatMul_1/ReadVariableOp_1?
lstm_cell_8/MatMul_1MatMul+lstm_cell_8/MatMul_1/ReadVariableOp:value:0-lstm_cell_8/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/MatMul_1?
lstm_cell_8/addAddV2lstm_cell_8/MatMul:product:0lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_cell_8/add?
"lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_8_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_8/BiasAdd/ReadVariableOp?
lstm_cell_8/BiasAddBiasAddlstm_cell_8/add:z:0*lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/BiasAdd|
lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_8/split/split_dim?
lstm_cell_8/splitSplit$lstm_cell_8/split/split_dim:output:0lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_8/splitk
lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Consto
lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_1?
lstm_cell_8/MulMullstm_cell_8/split:output:0lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul?
lstm_cell_8/Add_1AddV2lstm_cell_8/Mul:z:0lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_1?
#lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_8/clip_by_value/Minimum/y?
!lstm_cell_8/clip_by_value/MinimumMinimumlstm_cell_8/Add_1:z:0,lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22#
!lstm_cell_8/clip_by_value/Minimum
lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value/y?
lstm_cell_8/clip_by_valueMaximum%lstm_cell_8/clip_by_value/Minimum:z:0$lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_valueo
lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Const_2o
lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_3?
lstm_cell_8/Mul_1Mullstm_cell_8/split:output:1lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul_1?
lstm_cell_8/Add_2AddV2lstm_cell_8/Mul_1:z:0lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_2?
%lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_8/clip_by_value_1/Minimum/y?
#lstm_cell_8/clip_by_value_1/MinimumMinimumlstm_cell_8/Add_2:z:0.lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_8/clip_by_value_1/Minimum?
lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value_1/y?
lstm_cell_8/clip_by_value_1Maximum'lstm_cell_8/clip_by_value_1/Minimum:z:0&lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_value_1?
 lstm_cell_8/mul_2/ReadVariableOpReadVariableOp)lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02"
 lstm_cell_8/mul_2/ReadVariableOp?
lstm_cell_8/mul_2Mullstm_cell_8/clip_by_value_1:z:0(lstm_cell_8/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_2q
lstm_cell_8/TanhTanhlstm_cell_8/split:output:2*
T0*
_output_shapes

:22
lstm_cell_8/Tanh?
lstm_cell_8/mul_3Mullstm_cell_8/clip_by_value:z:0lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_3?
lstm_cell_8/add_3AddV2lstm_cell_8/mul_2:z:0lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_8/add_3o
lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Const_4o
lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_5?
lstm_cell_8/Mul_4Mullstm_cell_8/split:output:3lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul_4?
lstm_cell_8/Add_4AddV2lstm_cell_8/Mul_4:z:0lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_4?
%lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_8/clip_by_value_2/Minimum/y?
#lstm_cell_8/clip_by_value_2/MinimumMinimumlstm_cell_8/Add_4:z:0.lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_8/clip_by_value_2/Minimum?
lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value_2/y?
lstm_cell_8/clip_by_value_2Maximum'lstm_cell_8/clip_by_value_2/Minimum:z:0&lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_value_2p
lstm_cell_8/Tanh_1Tanhlstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_8/Tanh_1?
lstm_cell_8/mul_5Mullstm_cell_8/clip_by_value_2:z:0lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time?
ReadVariableOpReadVariableOp,lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_8_matmul_readvariableop_resource.lstm_cell_8_matmul_1_readvariableop_1_resource+lstm_cell_8_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_45974*
condR
while_cond_45973*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_8_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_8/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_8_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_8/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_8/BiasAdd/ReadVariableOp"^lstm_cell_8/MatMul/ReadVariableOp$^lstm_cell_8/MatMul_1/ReadVariableOp&^lstm_cell_8/MatMul_1/ReadVariableOp_1!^lstm_cell_8/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:
2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_8/BiasAdd/ReadVariableOp"lstm_cell_8/BiasAdd/ReadVariableOp2F
!lstm_cell_8/MatMul/ReadVariableOp!lstm_cell_8/MatMul/ReadVariableOp2J
#lstm_cell_8/MatMul_1/ReadVariableOp#lstm_cell_8/MatMul_1/ReadVariableOp2N
%lstm_cell_8/MatMul_1/ReadVariableOp_1%lstm_cell_8/MatMul_1/ReadVariableOp_12D
 lstm_cell_8/mul_2/ReadVariableOp lstm_cell_8/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
?
?
while_cond_45685
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_45685___redundant_placeholder03
/while_while_cond_45685___redundant_placeholder13
/while_while_cond_45685___redundant_placeholder23
/while_while_cond_45685___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?,
?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49465

inputs
states_0
states_11
matmul_readvariableop_resource:	2?3
 matmul_1_readvariableop_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1Z
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes

:22
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:22
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes

:22
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3`
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes

:22
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:22
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:22
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:22
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:22
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:22
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5`
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes

:22
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:22
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:22
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:22
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:22

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:2:2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:HD

_output_shapes

:2
"
_user_specified_name
states/0:HD

_output_shapes

:2
"
_user_specified_name
states/1
?
?
while_cond_47553
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_47553___redundant_placeholder03
/while_while_cond_47553___redundant_placeholder13
/while_while_cond_47553___redundant_placeholder23
/while_while_cond_47553___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
ċ
?
G__inference_sequential_3_layer_call_and_return_conditional_losses_46881

inputsD
1lstm_7_lstm_cell_9_matmul_readvariableop_resource:	?E
3lstm_7_lstm_cell_9_matmul_1_readvariableop_resource:2H
5lstm_7_lstm_cell_9_matmul_1_readvariableop_1_resource:	2?A
2lstm_7_lstm_cell_9_biasadd_readvariableop_resource:	?B
0lstm_7_lstm_cell_9_mul_2_readvariableop_resource:2D
1lstm_6_lstm_cell_8_matmul_readvariableop_resource:	2?E
3lstm_6_lstm_cell_8_matmul_1_readvariableop_resource:2H
5lstm_6_lstm_cell_8_matmul_1_readvariableop_1_resource:	2?A
2lstm_6_lstm_cell_8_biasadd_readvariableop_resource:	?B
0lstm_6_lstm_cell_8_mul_2_readvariableop_resource:2K
9time_distributed_3_dense_3_matmul_readvariableop_resource:2H
:time_distributed_3_dense_3_biasadd_readvariableop_resource:
identity??lstm_6/AssignVariableOp?lstm_6/AssignVariableOp_1?lstm_6/ReadVariableOp?lstm_6/ReadVariableOp_1?)lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp?(lstm_6/lstm_cell_8/MatMul/ReadVariableOp?*lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp?,lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1?'lstm_6/lstm_cell_8/mul_2/ReadVariableOp?lstm_6/while?lstm_7/AssignVariableOp?lstm_7/AssignVariableOp_1?lstm_7/ReadVariableOp?lstm_7/ReadVariableOp_1?)lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp?(lstm_7/lstm_cell_9/MatMul/ReadVariableOp?*lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp?,lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1?'lstm_7/lstm_cell_9/mul_2/ReadVariableOp?lstm_7/while?1time_distributed_3/dense_3/BiasAdd/ReadVariableOp?0time_distributed_3/dense_3/MatMul/ReadVariableOp?
lstm_7/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_7/transpose/perm?
lstm_7/transpose	Transposeinputslstm_7/transpose/perm:output:0*
T0*"
_output_shapes
:
2
lstm_7/transposeq
lstm_7/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
         2
lstm_7/Shape?
lstm_7/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_7/strided_slice/stack?
lstm_7/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_7/strided_slice/stack_1?
lstm_7/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_7/strided_slice/stack_2?
lstm_7/strided_sliceStridedSlicelstm_7/Shape:output:0#lstm_7/strided_slice/stack:output:0%lstm_7/strided_slice/stack_1:output:0%lstm_7/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_7/strided_slice?
"lstm_7/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2$
"lstm_7/TensorArrayV2/element_shape?
lstm_7/TensorArrayV2TensorListReserve+lstm_7/TensorArrayV2/element_shape:output:0lstm_7/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_7/TensorArrayV2?
<lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      2>
<lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shape?
.lstm_7/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_7/transpose:y:0Elstm_7/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type020
.lstm_7/TensorArrayUnstack/TensorListFromTensor?
lstm_7/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_7/strided_slice_1/stack?
lstm_7/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_1/stack_1?
lstm_7/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_1/stack_2?
lstm_7/strided_slice_1StridedSlicelstm_7/transpose:y:0%lstm_7/strided_slice_1/stack:output:0'lstm_7/strided_slice_1/stack_1:output:0'lstm_7/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
lstm_7/strided_slice_1?
(lstm_7/lstm_cell_9/MatMul/ReadVariableOpReadVariableOp1lstm_7_lstm_cell_9_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02*
(lstm_7/lstm_cell_9/MatMul/ReadVariableOp?
lstm_7/lstm_cell_9/MatMulMatMullstm_7/strided_slice_1:output:00lstm_7/lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_7/lstm_cell_9/MatMul?
*lstm_7/lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp3lstm_7_lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02,
*lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp?
,lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1ReadVariableOp5lstm_7_lstm_cell_9_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02.
,lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1?
lstm_7/lstm_cell_9/MatMul_1MatMul2lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp:value:04lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_7/lstm_cell_9/MatMul_1?
lstm_7/lstm_cell_9/addAddV2#lstm_7/lstm_cell_9/MatMul:product:0%lstm_7/lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_7/lstm_cell_9/add?
)lstm_7/lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp2lstm_7_lstm_cell_9_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02+
)lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp?
lstm_7/lstm_cell_9/BiasAddBiasAddlstm_7/lstm_cell_9/add:z:01lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_7/lstm_cell_9/BiasAdd?
"lstm_7/lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"lstm_7/lstm_cell_9/split/split_dim?
lstm_7/lstm_cell_9/splitSplit+lstm_7/lstm_cell_9/split/split_dim:output:0#lstm_7/lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_7/lstm_cell_9/splity
lstm_7/lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_7/lstm_cell_9/Const}
lstm_7/lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_7/lstm_cell_9/Const_1?
lstm_7/lstm_cell_9/MulMul!lstm_7/lstm_cell_9/split:output:0!lstm_7/lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Mul?
lstm_7/lstm_cell_9/Add_1AddV2lstm_7/lstm_cell_9/Mul:z:0#lstm_7/lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Add_1?
*lstm_7/lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2,
*lstm_7/lstm_cell_9/clip_by_value/Minimum/y?
(lstm_7/lstm_cell_9/clip_by_value/MinimumMinimumlstm_7/lstm_cell_9/Add_1:z:03lstm_7/lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(lstm_7/lstm_cell_9/clip_by_value/Minimum?
"lstm_7/lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"lstm_7/lstm_cell_9/clip_by_value/y?
 lstm_7/lstm_cell_9/clip_by_valueMaximum,lstm_7/lstm_cell_9/clip_by_value/Minimum:z:0+lstm_7/lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 lstm_7/lstm_cell_9/clip_by_value}
lstm_7/lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_7/lstm_cell_9/Const_2}
lstm_7/lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_7/lstm_cell_9/Const_3?
lstm_7/lstm_cell_9/Mul_1Mul!lstm_7/lstm_cell_9/split:output:1#lstm_7/lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Mul_1?
lstm_7/lstm_cell_9/Add_2AddV2lstm_7/lstm_cell_9/Mul_1:z:0#lstm_7/lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Add_2?
,lstm_7/lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_7/lstm_cell_9/clip_by_value_1/Minimum/y?
*lstm_7/lstm_cell_9/clip_by_value_1/MinimumMinimumlstm_7/lstm_cell_9/Add_2:z:05lstm_7/lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*lstm_7/lstm_cell_9/clip_by_value_1/Minimum?
$lstm_7/lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_7/lstm_cell_9/clip_by_value_1/y?
"lstm_7/lstm_cell_9/clip_by_value_1Maximum.lstm_7/lstm_cell_9/clip_by_value_1/Minimum:z:0-lstm_7/lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"lstm_7/lstm_cell_9/clip_by_value_1?
'lstm_7/lstm_cell_9/mul_2/ReadVariableOpReadVariableOp0lstm_7_lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02)
'lstm_7/lstm_cell_9/mul_2/ReadVariableOp?
lstm_7/lstm_cell_9/mul_2Mul&lstm_7/lstm_cell_9/clip_by_value_1:z:0/lstm_7/lstm_cell_9/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/mul_2?
lstm_7/lstm_cell_9/TanhTanh!lstm_7/lstm_cell_9/split:output:2*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Tanh?
lstm_7/lstm_cell_9/mul_3Mul$lstm_7/lstm_cell_9/clip_by_value:z:0lstm_7/lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/mul_3?
lstm_7/lstm_cell_9/add_3AddV2lstm_7/lstm_cell_9/mul_2:z:0lstm_7/lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/add_3}
lstm_7/lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_7/lstm_cell_9/Const_4}
lstm_7/lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_7/lstm_cell_9/Const_5?
lstm_7/lstm_cell_9/Mul_4Mul!lstm_7/lstm_cell_9/split:output:3#lstm_7/lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Mul_4?
lstm_7/lstm_cell_9/Add_4AddV2lstm_7/lstm_cell_9/Mul_4:z:0#lstm_7/lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Add_4?
,lstm_7/lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_7/lstm_cell_9/clip_by_value_2/Minimum/y?
*lstm_7/lstm_cell_9/clip_by_value_2/MinimumMinimumlstm_7/lstm_cell_9/Add_4:z:05lstm_7/lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*lstm_7/lstm_cell_9/clip_by_value_2/Minimum?
$lstm_7/lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_7/lstm_cell_9/clip_by_value_2/y?
"lstm_7/lstm_cell_9/clip_by_value_2Maximum.lstm_7/lstm_cell_9/clip_by_value_2/Minimum:z:0-lstm_7/lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"lstm_7/lstm_cell_9/clip_by_value_2?
lstm_7/lstm_cell_9/Tanh_1Tanhlstm_7/lstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Tanh_1?
lstm_7/lstm_cell_9/mul_5Mul&lstm_7/lstm_cell_9/clip_by_value_2:z:0lstm_7/lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/mul_5?
$lstm_7/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2&
$lstm_7/TensorArrayV2_1/element_shape?
lstm_7/TensorArrayV2_1TensorListReserve-lstm_7/TensorArrayV2_1/element_shape:output:0lstm_7/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_7/TensorArrayV2_1\
lstm_7/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_7/time?
lstm_7/ReadVariableOpReadVariableOp3lstm_7_lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_7/ReadVariableOp?
lstm_7/ReadVariableOp_1ReadVariableOp0lstm_7_lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_7/ReadVariableOp_1?
lstm_7/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2!
lstm_7/while/maximum_iterationsx
lstm_7/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_7/while/loop_counter?
lstm_7/whileWhile"lstm_7/while/loop_counter:output:0(lstm_7/while/maximum_iterations:output:0lstm_7/time:output:0lstm_7/TensorArrayV2_1:handle:0lstm_7/ReadVariableOp:value:0lstm_7/ReadVariableOp_1:value:0lstm_7/strided_slice:output:0>lstm_7/TensorArrayUnstack/TensorListFromTensor:output_handle:01lstm_7_lstm_cell_9_matmul_readvariableop_resource5lstm_7_lstm_cell_9_matmul_1_readvariableop_1_resource2lstm_7_lstm_cell_9_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *#
bodyR
lstm_7_while_body_46590*#
condR
lstm_7_while_cond_46589*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
lstm_7/while?
7lstm_7/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7lstm_7/TensorArrayV2Stack/TensorListStack/element_shape?
)lstm_7/TensorArrayV2Stack/TensorListStackTensorListStacklstm_7/while:output:3@lstm_7/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02+
)lstm_7/TensorArrayV2Stack/TensorListStack?
lstm_7/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
lstm_7/strided_slice_2/stack?
lstm_7/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2 
lstm_7/strided_slice_2/stack_1?
lstm_7/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_2/stack_2?
lstm_7/strided_slice_2StridedSlice2lstm_7/TensorArrayV2Stack/TensorListStack:tensor:0%lstm_7/strided_slice_2/stack:output:0'lstm_7/strided_slice_2/stack_1:output:0'lstm_7/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_7/strided_slice_2?
lstm_7/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_7/transpose_1/perm?
lstm_7/transpose_1	Transpose2lstm_7/TensorArrayV2Stack/TensorListStack:tensor:0 lstm_7/transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
lstm_7/transpose_1t
lstm_7/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_7/runtime?
lstm_7/AssignVariableOpAssignVariableOp3lstm_7_lstm_cell_9_matmul_1_readvariableop_resourcelstm_7/while:output:4^lstm_7/ReadVariableOp+^lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_7/AssignVariableOp?
lstm_7/AssignVariableOp_1AssignVariableOp0lstm_7_lstm_cell_9_mul_2_readvariableop_resourcelstm_7/while:output:5^lstm_7/ReadVariableOp_1(^lstm_7/lstm_cell_9/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_7/AssignVariableOp_1?
lstm_6/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_6/transpose/perm?
lstm_6/transpose	Transposelstm_7/transpose_1:y:0lstm_6/transpose/perm:output:0*
T0*"
_output_shapes
:
22
lstm_6/transposeq
lstm_6/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
      2   2
lstm_6/Shape?
lstm_6/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_6/strided_slice/stack?
lstm_6/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_6/strided_slice/stack_1?
lstm_6/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_6/strided_slice/stack_2?
lstm_6/strided_sliceStridedSlicelstm_6/Shape:output:0#lstm_6/strided_slice/stack:output:0%lstm_6/strided_slice/stack_1:output:0%lstm_6/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_6/strided_slice?
"lstm_6/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2$
"lstm_6/TensorArrayV2/element_shape?
lstm_6/TensorArrayV2TensorListReserve+lstm_6/TensorArrayV2/element_shape:output:0lstm_6/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_6/TensorArrayV2?
<lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2>
<lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shape?
.lstm_6/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_6/transpose:y:0Elstm_6/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type020
.lstm_6/TensorArrayUnstack/TensorListFromTensor?
lstm_6/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_6/strided_slice_1/stack?
lstm_6/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_1/stack_1?
lstm_6/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_1/stack_2?
lstm_6/strided_slice_1StridedSlicelstm_6/transpose:y:0%lstm_6/strided_slice_1/stack:output:0'lstm_6/strided_slice_1/stack_1:output:0'lstm_6/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_6/strided_slice_1?
(lstm_6/lstm_cell_8/MatMul/ReadVariableOpReadVariableOp1lstm_6_lstm_cell_8_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02*
(lstm_6/lstm_cell_8/MatMul/ReadVariableOp?
lstm_6/lstm_cell_8/MatMulMatMullstm_6/strided_slice_1:output:00lstm_6/lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_6/lstm_cell_8/MatMul?
*lstm_6/lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp3lstm_6_lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02,
*lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp?
,lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1ReadVariableOp5lstm_6_lstm_cell_8_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02.
,lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1?
lstm_6/lstm_cell_8/MatMul_1MatMul2lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp:value:04lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_6/lstm_cell_8/MatMul_1?
lstm_6/lstm_cell_8/addAddV2#lstm_6/lstm_cell_8/MatMul:product:0%lstm_6/lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_6/lstm_cell_8/add?
)lstm_6/lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp2lstm_6_lstm_cell_8_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02+
)lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp?
lstm_6/lstm_cell_8/BiasAddBiasAddlstm_6/lstm_cell_8/add:z:01lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_6/lstm_cell_8/BiasAdd?
"lstm_6/lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"lstm_6/lstm_cell_8/split/split_dim?
lstm_6/lstm_cell_8/splitSplit+lstm_6/lstm_cell_8/split/split_dim:output:0#lstm_6/lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_6/lstm_cell_8/splity
lstm_6/lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_6/lstm_cell_8/Const}
lstm_6/lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_6/lstm_cell_8/Const_1?
lstm_6/lstm_cell_8/MulMul!lstm_6/lstm_cell_8/split:output:0!lstm_6/lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Mul?
lstm_6/lstm_cell_8/Add_1AddV2lstm_6/lstm_cell_8/Mul:z:0#lstm_6/lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Add_1?
*lstm_6/lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2,
*lstm_6/lstm_cell_8/clip_by_value/Minimum/y?
(lstm_6/lstm_cell_8/clip_by_value/MinimumMinimumlstm_6/lstm_cell_8/Add_1:z:03lstm_6/lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(lstm_6/lstm_cell_8/clip_by_value/Minimum?
"lstm_6/lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"lstm_6/lstm_cell_8/clip_by_value/y?
 lstm_6/lstm_cell_8/clip_by_valueMaximum,lstm_6/lstm_cell_8/clip_by_value/Minimum:z:0+lstm_6/lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 lstm_6/lstm_cell_8/clip_by_value}
lstm_6/lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_6/lstm_cell_8/Const_2}
lstm_6/lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_6/lstm_cell_8/Const_3?
lstm_6/lstm_cell_8/Mul_1Mul!lstm_6/lstm_cell_8/split:output:1#lstm_6/lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Mul_1?
lstm_6/lstm_cell_8/Add_2AddV2lstm_6/lstm_cell_8/Mul_1:z:0#lstm_6/lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Add_2?
,lstm_6/lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_6/lstm_cell_8/clip_by_value_1/Minimum/y?
*lstm_6/lstm_cell_8/clip_by_value_1/MinimumMinimumlstm_6/lstm_cell_8/Add_2:z:05lstm_6/lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*lstm_6/lstm_cell_8/clip_by_value_1/Minimum?
$lstm_6/lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_6/lstm_cell_8/clip_by_value_1/y?
"lstm_6/lstm_cell_8/clip_by_value_1Maximum.lstm_6/lstm_cell_8/clip_by_value_1/Minimum:z:0-lstm_6/lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"lstm_6/lstm_cell_8/clip_by_value_1?
'lstm_6/lstm_cell_8/mul_2/ReadVariableOpReadVariableOp0lstm_6_lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02)
'lstm_6/lstm_cell_8/mul_2/ReadVariableOp?
lstm_6/lstm_cell_8/mul_2Mul&lstm_6/lstm_cell_8/clip_by_value_1:z:0/lstm_6/lstm_cell_8/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/mul_2?
lstm_6/lstm_cell_8/TanhTanh!lstm_6/lstm_cell_8/split:output:2*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Tanh?
lstm_6/lstm_cell_8/mul_3Mul$lstm_6/lstm_cell_8/clip_by_value:z:0lstm_6/lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/mul_3?
lstm_6/lstm_cell_8/add_3AddV2lstm_6/lstm_cell_8/mul_2:z:0lstm_6/lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/add_3}
lstm_6/lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_6/lstm_cell_8/Const_4}
lstm_6/lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_6/lstm_cell_8/Const_5?
lstm_6/lstm_cell_8/Mul_4Mul!lstm_6/lstm_cell_8/split:output:3#lstm_6/lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Mul_4?
lstm_6/lstm_cell_8/Add_4AddV2lstm_6/lstm_cell_8/Mul_4:z:0#lstm_6/lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Add_4?
,lstm_6/lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_6/lstm_cell_8/clip_by_value_2/Minimum/y?
*lstm_6/lstm_cell_8/clip_by_value_2/MinimumMinimumlstm_6/lstm_cell_8/Add_4:z:05lstm_6/lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*lstm_6/lstm_cell_8/clip_by_value_2/Minimum?
$lstm_6/lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_6/lstm_cell_8/clip_by_value_2/y?
"lstm_6/lstm_cell_8/clip_by_value_2Maximum.lstm_6/lstm_cell_8/clip_by_value_2/Minimum:z:0-lstm_6/lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"lstm_6/lstm_cell_8/clip_by_value_2?
lstm_6/lstm_cell_8/Tanh_1Tanhlstm_6/lstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Tanh_1?
lstm_6/lstm_cell_8/mul_5Mul&lstm_6/lstm_cell_8/clip_by_value_2:z:0lstm_6/lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/mul_5?
$lstm_6/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2&
$lstm_6/TensorArrayV2_1/element_shape?
lstm_6/TensorArrayV2_1TensorListReserve-lstm_6/TensorArrayV2_1/element_shape:output:0lstm_6/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_6/TensorArrayV2_1\
lstm_6/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_6/time?
lstm_6/ReadVariableOpReadVariableOp3lstm_6_lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_6/ReadVariableOp?
lstm_6/ReadVariableOp_1ReadVariableOp0lstm_6_lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_6/ReadVariableOp_1?
lstm_6/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2!
lstm_6/while/maximum_iterationsx
lstm_6/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_6/while/loop_counter?
lstm_6/whileWhile"lstm_6/while/loop_counter:output:0(lstm_6/while/maximum_iterations:output:0lstm_6/time:output:0lstm_6/TensorArrayV2_1:handle:0lstm_6/ReadVariableOp:value:0lstm_6/ReadVariableOp_1:value:0lstm_6/strided_slice:output:0>lstm_6/TensorArrayUnstack/TensorListFromTensor:output_handle:01lstm_6_lstm_cell_8_matmul_readvariableop_resource5lstm_6_lstm_cell_8_matmul_1_readvariableop_1_resource2lstm_6_lstm_cell_8_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *#
bodyR
lstm_6_while_body_46764*#
condR
lstm_6_while_cond_46763*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
lstm_6/while?
7lstm_6/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7lstm_6/TensorArrayV2Stack/TensorListStack/element_shape?
)lstm_6/TensorArrayV2Stack/TensorListStackTensorListStacklstm_6/while:output:3@lstm_6/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02+
)lstm_6/TensorArrayV2Stack/TensorListStack?
lstm_6/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
lstm_6/strided_slice_2/stack?
lstm_6/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2 
lstm_6/strided_slice_2/stack_1?
lstm_6/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_2/stack_2?
lstm_6/strided_slice_2StridedSlice2lstm_6/TensorArrayV2Stack/TensorListStack:tensor:0%lstm_6/strided_slice_2/stack:output:0'lstm_6/strided_slice_2/stack_1:output:0'lstm_6/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_6/strided_slice_2?
lstm_6/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_6/transpose_1/perm?
lstm_6/transpose_1	Transpose2lstm_6/TensorArrayV2Stack/TensorListStack:tensor:0 lstm_6/transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
lstm_6/transpose_1t
lstm_6/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_6/runtime?
lstm_6/AssignVariableOpAssignVariableOp3lstm_6_lstm_cell_8_matmul_1_readvariableop_resourcelstm_6/while:output:4^lstm_6/ReadVariableOp+^lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_6/AssignVariableOp?
lstm_6/AssignVariableOp_1AssignVariableOp0lstm_6_lstm_cell_8_mul_2_readvariableop_resourcelstm_6/while:output:5^lstm_6/ReadVariableOp_1(^lstm_6/lstm_cell_8/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_6/AssignVariableOp_1?
 time_distributed_3/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2"
 time_distributed_3/Reshape/shape?
time_distributed_3/ReshapeReshapelstm_6/transpose_1:y:0)time_distributed_3/Reshape/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_3/Reshape?
0time_distributed_3/dense_3/MatMul/ReadVariableOpReadVariableOp9time_distributed_3_dense_3_matmul_readvariableop_resource*
_output_shapes

:2*
dtype022
0time_distributed_3/dense_3/MatMul/ReadVariableOp?
!time_distributed_3/dense_3/MatMulMatMul#time_distributed_3/Reshape:output:08time_distributed_3/dense_3/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
2#
!time_distributed_3/dense_3/MatMul?
1time_distributed_3/dense_3/BiasAdd/ReadVariableOpReadVariableOp:time_distributed_3_dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype023
1time_distributed_3/dense_3/BiasAdd/ReadVariableOp?
"time_distributed_3/dense_3/BiasAddBiasAdd+time_distributed_3/dense_3/MatMul:product:09time_distributed_3/dense_3/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
2$
"time_distributed_3/dense_3/BiasAdd?
"time_distributed_3/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      2$
"time_distributed_3/Reshape_1/shape?
time_distributed_3/Reshape_1Reshape+time_distributed_3/dense_3/BiasAdd:output:0+time_distributed_3/Reshape_1/shape:output:0*
T0*"
_output_shapes
:
2
time_distributed_3/Reshape_1?
"time_distributed_3/Reshape_2/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2$
"time_distributed_3/Reshape_2/shape?
time_distributed_3/Reshape_2Reshapelstm_6/transpose_1:y:0+time_distributed_3/Reshape_2/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_3/Reshape_2{
IdentityIdentity%time_distributed_3/Reshape_1:output:0^NoOp*
T0*"
_output_shapes
:
2

Identity?
NoOpNoOp^lstm_6/AssignVariableOp^lstm_6/AssignVariableOp_1^lstm_6/ReadVariableOp^lstm_6/ReadVariableOp_1*^lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp)^lstm_6/lstm_cell_8/MatMul/ReadVariableOp+^lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp-^lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1(^lstm_6/lstm_cell_8/mul_2/ReadVariableOp^lstm_6/while^lstm_7/AssignVariableOp^lstm_7/AssignVariableOp_1^lstm_7/ReadVariableOp^lstm_7/ReadVariableOp_1*^lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp)^lstm_7/lstm_cell_9/MatMul/ReadVariableOp+^lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp-^lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1(^lstm_7/lstm_cell_9/mul_2/ReadVariableOp^lstm_7/while2^time_distributed_3/dense_3/BiasAdd/ReadVariableOp1^time_distributed_3/dense_3/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 22
lstm_6/AssignVariableOplstm_6/AssignVariableOp26
lstm_6/AssignVariableOp_1lstm_6/AssignVariableOp_12.
lstm_6/ReadVariableOplstm_6/ReadVariableOp22
lstm_6/ReadVariableOp_1lstm_6/ReadVariableOp_12V
)lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp)lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp2T
(lstm_6/lstm_cell_8/MatMul/ReadVariableOp(lstm_6/lstm_cell_8/MatMul/ReadVariableOp2X
*lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp*lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp2\
,lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1,lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_12R
'lstm_6/lstm_cell_8/mul_2/ReadVariableOp'lstm_6/lstm_cell_8/mul_2/ReadVariableOp2
lstm_6/whilelstm_6/while22
lstm_7/AssignVariableOplstm_7/AssignVariableOp26
lstm_7/AssignVariableOp_1lstm_7/AssignVariableOp_12.
lstm_7/ReadVariableOplstm_7/ReadVariableOp22
lstm_7/ReadVariableOp_1lstm_7/ReadVariableOp_12V
)lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp)lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp2T
(lstm_7/lstm_cell_9/MatMul/ReadVariableOp(lstm_7/lstm_cell_9/MatMul/ReadVariableOp2X
*lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp*lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp2\
,lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1,lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_12R
'lstm_7/lstm_cell_9/mul_2/ReadVariableOp'lstm_7/lstm_cell_9/mul_2/ReadVariableOp2
lstm_7/whilelstm_7/while2f
1time_distributed_3/dense_3/BiasAdd/ReadVariableOp1time_distributed_3/dense_3/BiasAdd/ReadVariableOp2d
0time_distributed_3/dense_3/MatMul/ReadVariableOp0time_distributed_3/dense_3/MatMul/ReadVariableOp:J F
"
_output_shapes
:

 
_user_specified_nameinputs
?m
?
A__inference_lstm_6_layer_call_and_return_conditional_losses_48253
inputs_0=
*lstm_cell_8_matmul_readvariableop_resource:	2?>
,lstm_cell_8_matmul_1_readvariableop_resource:2A
.lstm_cell_8_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_8_biasadd_readvariableop_resource:	?;
)lstm_cell_8_mul_2_readvariableop_resource:2
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_8/BiasAdd/ReadVariableOp?!lstm_cell_8/MatMul/ReadVariableOp?#lstm_cell_8/MatMul_1/ReadVariableOp?%lstm_cell_8/MatMul_1/ReadVariableOp_1? lstm_cell_8/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:?????????22
	transposeK
ShapeShapetranspose:y:0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_8/MatMul/ReadVariableOpReadVariableOp*lstm_cell_8_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02#
!lstm_cell_8/MatMul/ReadVariableOp?
lstm_cell_8/MatMulMatMulstrided_slice_1:output:0)lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/MatMul?
#lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02%
#lstm_cell_8/MatMul_1/ReadVariableOp?
%lstm_cell_8/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_8_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_8/MatMul_1/ReadVariableOp_1?
lstm_cell_8/MatMul_1MatMul+lstm_cell_8/MatMul_1/ReadVariableOp:value:0-lstm_cell_8/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/MatMul_1?
lstm_cell_8/addAddV2lstm_cell_8/MatMul:product:0lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_cell_8/add?
"lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_8_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_8/BiasAdd/ReadVariableOp?
lstm_cell_8/BiasAddBiasAddlstm_cell_8/add:z:0*lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/BiasAdd|
lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_8/split/split_dim?
lstm_cell_8/splitSplit$lstm_cell_8/split/split_dim:output:0lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_8/splitk
lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Consto
lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_1?
lstm_cell_8/MulMullstm_cell_8/split:output:0lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul?
lstm_cell_8/Add_1AddV2lstm_cell_8/Mul:z:0lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_1?
#lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_8/clip_by_value/Minimum/y?
!lstm_cell_8/clip_by_value/MinimumMinimumlstm_cell_8/Add_1:z:0,lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22#
!lstm_cell_8/clip_by_value/Minimum
lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value/y?
lstm_cell_8/clip_by_valueMaximum%lstm_cell_8/clip_by_value/Minimum:z:0$lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_valueo
lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Const_2o
lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_3?
lstm_cell_8/Mul_1Mullstm_cell_8/split:output:1lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul_1?
lstm_cell_8/Add_2AddV2lstm_cell_8/Mul_1:z:0lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_2?
%lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_8/clip_by_value_1/Minimum/y?
#lstm_cell_8/clip_by_value_1/MinimumMinimumlstm_cell_8/Add_2:z:0.lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_8/clip_by_value_1/Minimum?
lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value_1/y?
lstm_cell_8/clip_by_value_1Maximum'lstm_cell_8/clip_by_value_1/Minimum:z:0&lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_value_1?
 lstm_cell_8/mul_2/ReadVariableOpReadVariableOp)lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02"
 lstm_cell_8/mul_2/ReadVariableOp?
lstm_cell_8/mul_2Mullstm_cell_8/clip_by_value_1:z:0(lstm_cell_8/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_2q
lstm_cell_8/TanhTanhlstm_cell_8/split:output:2*
T0*
_output_shapes

:22
lstm_cell_8/Tanh?
lstm_cell_8/mul_3Mullstm_cell_8/clip_by_value:z:0lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_3?
lstm_cell_8/add_3AddV2lstm_cell_8/mul_2:z:0lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_8/add_3o
lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Const_4o
lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_5?
lstm_cell_8/Mul_4Mullstm_cell_8/split:output:3lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul_4?
lstm_cell_8/Add_4AddV2lstm_cell_8/Mul_4:z:0lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_4?
%lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_8/clip_by_value_2/Minimum/y?
#lstm_cell_8/clip_by_value_2/MinimumMinimumlstm_cell_8/Add_4:z:0.lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_8/clip_by_value_2/Minimum?
lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value_2/y?
lstm_cell_8/clip_by_value_2Maximum'lstm_cell_8/clip_by_value_2/Minimum:z:0&lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_value_2p
lstm_cell_8/Tanh_1Tanhlstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_8/Tanh_1?
lstm_cell_8/mul_5Mullstm_cell_8/clip_by_value_2:z:0lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time?
ReadVariableOpReadVariableOp,lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_8_matmul_readvariableop_resource.lstm_cell_8_matmul_1_readvariableop_1_resource+lstm_cell_8_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_48148*
condR
while_cond_48147*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_8_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_8/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_8_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_8/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_8/BiasAdd/ReadVariableOp"^lstm_cell_8/MatMul/ReadVariableOp$^lstm_cell_8/MatMul_1/ReadVariableOp&^lstm_cell_8/MatMul_1/ReadVariableOp_1!^lstm_cell_8/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:?????????2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_8/BiasAdd/ReadVariableOp"lstm_cell_8/BiasAdd/ReadVariableOp2F
!lstm_cell_8/MatMul/ReadVariableOp!lstm_cell_8/MatMul/ReadVariableOp2J
#lstm_cell_8/MatMul_1/ReadVariableOp#lstm_cell_8/MatMul_1/ReadVariableOp2N
%lstm_cell_8/MatMul_1/ReadVariableOp_1%lstm_cell_8/MatMul_1/ReadVariableOp_12D
 lstm_cell_8/mul_2/ReadVariableOp lstm_cell_8/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:?????????2
"
_user_specified_name
inputs/0
?
?
&__inference_lstm_6_layer_call_fn_48847

inputs
unknown:	2?
	unknown_0:2
	unknown_1:	2?
	unknown_2:	?
	unknown_3:2
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_6_layer_call_and_return_conditional_losses_460792
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:
2: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
?m
?
A__inference_lstm_7_layer_call_and_return_conditional_losses_48015

inputs=
*lstm_cell_9_matmul_readvariableop_resource:	?>
,lstm_cell_9_matmul_1_readvariableop_resource:2A
.lstm_cell_9_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_9_biasadd_readvariableop_resource:	?;
)lstm_cell_9_mul_2_readvariableop_resource:2
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_9/BiasAdd/ReadVariableOp?!lstm_cell_9/MatMul/ReadVariableOp?#lstm_cell_9/MatMul_1/ReadVariableOp?%lstm_cell_9/MatMul_1/ReadVariableOp_1? lstm_cell_9/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permq
	transpose	Transposeinputstranspose/perm:output:0*
T0*"
_output_shapes
:
2
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
         2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_9/MatMul/ReadVariableOpReadVariableOp*lstm_cell_9_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02#
!lstm_cell_9/MatMul/ReadVariableOp?
lstm_cell_9/MatMulMatMulstrided_slice_1:output:0)lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/MatMul?
#lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02%
#lstm_cell_9/MatMul_1/ReadVariableOp?
%lstm_cell_9/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_9_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_9/MatMul_1/ReadVariableOp_1?
lstm_cell_9/MatMul_1MatMul+lstm_cell_9/MatMul_1/ReadVariableOp:value:0-lstm_cell_9/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/MatMul_1?
lstm_cell_9/addAddV2lstm_cell_9/MatMul:product:0lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_cell_9/add?
"lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_9_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_9/BiasAdd/ReadVariableOp?
lstm_cell_9/BiasAddBiasAddlstm_cell_9/add:z:0*lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/BiasAdd|
lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_9/split/split_dim?
lstm_cell_9/splitSplit$lstm_cell_9/split/split_dim:output:0lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_9/splitk
lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Consto
lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_1?
lstm_cell_9/MulMullstm_cell_9/split:output:0lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul?
lstm_cell_9/Add_1AddV2lstm_cell_9/Mul:z:0lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_1?
#lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_9/clip_by_value/Minimum/y?
!lstm_cell_9/clip_by_value/MinimumMinimumlstm_cell_9/Add_1:z:0,lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22#
!lstm_cell_9/clip_by_value/Minimum
lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value/y?
lstm_cell_9/clip_by_valueMaximum%lstm_cell_9/clip_by_value/Minimum:z:0$lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_valueo
lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Const_2o
lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_3?
lstm_cell_9/Mul_1Mullstm_cell_9/split:output:1lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul_1?
lstm_cell_9/Add_2AddV2lstm_cell_9/Mul_1:z:0lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_2?
%lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_9/clip_by_value_1/Minimum/y?
#lstm_cell_9/clip_by_value_1/MinimumMinimumlstm_cell_9/Add_2:z:0.lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_9/clip_by_value_1/Minimum?
lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value_1/y?
lstm_cell_9/clip_by_value_1Maximum'lstm_cell_9/clip_by_value_1/Minimum:z:0&lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_value_1?
 lstm_cell_9/mul_2/ReadVariableOpReadVariableOp)lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02"
 lstm_cell_9/mul_2/ReadVariableOp?
lstm_cell_9/mul_2Mullstm_cell_9/clip_by_value_1:z:0(lstm_cell_9/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_2q
lstm_cell_9/TanhTanhlstm_cell_9/split:output:2*
T0*
_output_shapes

:22
lstm_cell_9/Tanh?
lstm_cell_9/mul_3Mullstm_cell_9/clip_by_value:z:0lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_3?
lstm_cell_9/add_3AddV2lstm_cell_9/mul_2:z:0lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_9/add_3o
lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Const_4o
lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_5?
lstm_cell_9/Mul_4Mullstm_cell_9/split:output:3lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul_4?
lstm_cell_9/Add_4AddV2lstm_cell_9/Mul_4:z:0lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_4?
%lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_9/clip_by_value_2/Minimum/y?
#lstm_cell_9/clip_by_value_2/MinimumMinimumlstm_cell_9/Add_4:z:0.lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_9/clip_by_value_2/Minimum?
lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value_2/y?
lstm_cell_9/clip_by_value_2Maximum'lstm_cell_9/clip_by_value_2/Minimum:z:0&lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_value_2p
lstm_cell_9/Tanh_1Tanhlstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_9/Tanh_1?
lstm_cell_9/mul_5Mullstm_cell_9/clip_by_value_2:z:0lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time?
ReadVariableOpReadVariableOp,lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_9_matmul_readvariableop_resource.lstm_cell_9_matmul_1_readvariableop_1_resource+lstm_cell_9_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_47910*
condR
while_cond_47909*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_9_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_9/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_9_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_9/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_9/BiasAdd/ReadVariableOp"^lstm_cell_9/MatMul/ReadVariableOp$^lstm_cell_9/MatMul_1/ReadVariableOp&^lstm_cell_9/MatMul_1/ReadVariableOp_1!^lstm_cell_9/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:
: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_9/BiasAdd/ReadVariableOp"lstm_cell_9/BiasAdd/ReadVariableOp2F
!lstm_cell_9/MatMul/ReadVariableOp!lstm_cell_9/MatMul/ReadVariableOp2J
#lstm_cell_9/MatMul_1/ReadVariableOp#lstm_cell_9/MatMul_1/ReadVariableOp2N
%lstm_cell_9/MatMul_1/ReadVariableOp_1%lstm_cell_9/MatMul_1/ReadVariableOp_12D
 lstm_cell_9/mul_2/ReadVariableOp lstm_cell_9/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:

 
_user_specified_nameinputs
?+
?
__inference__traced_save_49847
file_prefix8
4savev2_lstm_7_lstm_cell_9_kernel_read_readvariableopB
>savev2_lstm_7_lstm_cell_9_recurrent_kernel_read_readvariableop6
2savev2_lstm_7_lstm_cell_9_bias_read_readvariableop8
4savev2_lstm_6_lstm_cell_8_kernel_read_readvariableopB
>savev2_lstm_6_lstm_cell_8_recurrent_kernel_read_readvariableop6
2savev2_lstm_6_lstm_cell_8_bias_read_readvariableop8
4savev2_time_distributed_3_kernel_read_readvariableop6
2savev2_time_distributed_3_bias_read_readvariableop.
*savev2_lstm_7_variable_read_readvariableop0
,savev2_lstm_7_variable_1_read_readvariableop.
*savev2_lstm_6_variable_read_readvariableop0
,savev2_lstm_6_variable_1_read_readvariableop$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop&
"savev2_total_1_read_readvariableop&
"savev2_count_1_read_readvariableop
savev2_const

identity_1??MergeV2Checkpoints?
StaticRegexFullMatchStaticRegexFullMatchfile_prefix"/device:CPU:**
_output_shapes
: *
pattern
^s3://.*2
StaticRegexFullMatchc
ConstConst"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B.part2
Constl
Const_1Const"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B
_temp/part2	
Const_1?
SelectSelectStaticRegexFullMatch:output:0Const:output:0Const_1:output:0"/device:CPU:**
T0*
_output_shapes
: 2
Selectt

StringJoin
StringJoinfile_prefixSelect:output:0"/device:CPU:**
N*
_output_shapes
: 2

StringJoinZ

num_shardsConst*
_output_shapes
: *
dtype0*
value	B :2

num_shards
ShardedFilename/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B : 2
ShardedFilename/shard?
ShardedFilenameShardedFilenameStringJoin:output:0ShardedFilename/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: 2
ShardedFilename?
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*?
value?B?B&variables/0/.ATTRIBUTES/VARIABLE_VALUEB&variables/1/.ATTRIBUTES/VARIABLE_VALUEB&variables/2/.ATTRIBUTES/VARIABLE_VALUEB&variables/3/.ATTRIBUTES/VARIABLE_VALUEB&variables/4/.ATTRIBUTES/VARIABLE_VALUEB&variables/5/.ATTRIBUTES/VARIABLE_VALUEB&variables/6/.ATTRIBUTES/VARIABLE_VALUEB&variables/7/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
SaveV2/tensor_names?
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*5
value,B*B B B B B B B B B B B B B B B B B 2
SaveV2/shape_and_slices?
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:04savev2_lstm_7_lstm_cell_9_kernel_read_readvariableop>savev2_lstm_7_lstm_cell_9_recurrent_kernel_read_readvariableop2savev2_lstm_7_lstm_cell_9_bias_read_readvariableop4savev2_lstm_6_lstm_cell_8_kernel_read_readvariableop>savev2_lstm_6_lstm_cell_8_recurrent_kernel_read_readvariableop2savev2_lstm_6_lstm_cell_8_bias_read_readvariableop4savev2_time_distributed_3_kernel_read_readvariableop2savev2_time_distributed_3_bias_read_readvariableop*savev2_lstm_7_variable_read_readvariableop,savev2_lstm_7_variable_1_read_readvariableop*savev2_lstm_6_variable_read_readvariableop,savev2_lstm_6_variable_1_read_readvariableop savev2_total_read_readvariableop savev2_count_read_readvariableop"savev2_total_1_read_readvariableop"savev2_count_1_read_readvariableopsavev2_const"/device:CPU:0*
_output_shapes
 *
dtypes
22
SaveV2?
&MergeV2Checkpoints/checkpoint_prefixesPackShardedFilename:filename:0^SaveV2"/device:CPU:0*
N*
T0*
_output_shapes
:2(
&MergeV2Checkpoints/checkpoint_prefixes?
MergeV2CheckpointsMergeV2Checkpoints/MergeV2Checkpoints/checkpoint_prefixes:output:0file_prefix"/device:CPU:0*
_output_shapes
 2
MergeV2Checkpointsr
IdentityIdentityfile_prefix^MergeV2Checkpoints"/device:CPU:0*
T0*
_output_shapes
: 2

Identity_

Identity_1IdentityIdentity:output:0^NoOp*
T0*
_output_shapes
: 2

Identity_1c
NoOpNoOp^MergeV2Checkpoints*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"!

identity_1Identity_1:output:0*?
_input_shapes?
~: :	?:	2?:?:	2?:	2?:?:2::2:2:2:2: : : : : 2(
MergeV2CheckpointsMergeV2Checkpoints:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:%!

_output_shapes
:	?:%!

_output_shapes
:	2?:!

_output_shapes	
:?:%!

_output_shapes
:	2?:%!

_output_shapes
:	2?:!

_output_shapes	
:?:$ 

_output_shapes

:2: 

_output_shapes
::$	 

_output_shapes

:2:$
 

_output_shapes

:2:$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: 
?,
?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49063

inputs
states_0
states_11
matmul_readvariableop_resource:	?3
 matmul_1_readvariableop_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1Z
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes

:22
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:22
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes

:22
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3`
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes

:22
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:22
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:22
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:22
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:22
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:22
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5`
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes

:22
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:22
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:22
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:22
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:22

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$::2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:
 
_user_specified_nameinputs:HD

_output_shapes

:2
"
_user_specified_name
states/0:HD

_output_shapes

:2
"
_user_specified_name
states/1
?
?
while_cond_46181
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_46181___redundant_placeholder03
/while_while_cond_46181___redundant_placeholder13
/while_while_cond_46181___redundant_placeholder23
/while_while_cond_46181___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?0
?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_44580

inputs
states:2
states_1:21
matmul_readvariableop_resource:	2?5
"matmul_1_readvariableop_1_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?MatMul_1/ReadVariableOp_1?mul_2/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:2*
dtype02
MatMul_1/ReadVariableOp?
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOp_1?
MatMul_1MatMulMatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1Z
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes

:22
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:22
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes

:22
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3`
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes

:22
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:22
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1u
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes

:2*
dtype02
mul_2/ReadVariableOpq
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:22
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:22
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:22
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5`
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes

:22
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:22
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:22
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:22
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:22

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^MatMul_1/ReadVariableOp_1^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:2: : : : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
?
?
G__inference_sequential_3_layer_call_and_return_conditional_losses_46486
lstm_7_input
lstm_7_46456:	?
lstm_7_46458:2
lstm_7_46460:	2?
lstm_7_46462:	?
lstm_7_46464:2
lstm_6_46467:	2?
lstm_6_46469:2
lstm_6_46471:	2?
lstm_6_46473:	?
lstm_6_46475:2*
time_distributed_3_46478:2&
time_distributed_3_46480:
identity??lstm_6/StatefulPartitionedCall?lstm_7/StatefulPartitionedCall?*time_distributed_3/StatefulPartitionedCall?
lstm_7/StatefulPartitionedCallStatefulPartitionedCalllstm_7_inputlstm_7_46456lstm_7_46458lstm_7_46460lstm_7_46462lstm_7_46464*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_7_layer_call_and_return_conditional_losses_462872 
lstm_7/StatefulPartitionedCall?
lstm_6/StatefulPartitionedCallStatefulPartitionedCall'lstm_7/StatefulPartitionedCall:output:0lstm_6_46467lstm_6_46469lstm_6_46471lstm_6_46473lstm_6_46475*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_6_layer_call_and_return_conditional_losses_460792 
lstm_6/StatefulPartitionedCall?
*time_distributed_3/StatefulPartitionedCallStatefulPartitionedCall'lstm_6/StatefulPartitionedCall:output:0time_distributed_3_46478time_distributed_3_46480*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_458772,
*time_distributed_3/StatefulPartitionedCall?
 time_distributed_3/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2"
 time_distributed_3/Reshape/shape?
time_distributed_3/ReshapeReshape'lstm_6/StatefulPartitionedCall:output:0)time_distributed_3/Reshape/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_3/Reshape?
IdentityIdentity3time_distributed_3/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

Identity?
NoOpNoOp^lstm_6/StatefulPartitionedCall^lstm_7/StatefulPartitionedCall+^time_distributed_3/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 2@
lstm_6/StatefulPartitionedCalllstm_6/StatefulPartitionedCall2@
lstm_7/StatefulPartitionedCalllstm_7/StatefulPartitionedCall2X
*time_distributed_3/StatefulPartitionedCall*time_distributed_3/StatefulPartitionedCall:P L
"
_output_shapes
:

&
_user_specified_namelstm_7_input
ċ
?
G__inference_sequential_3_layer_call_and_return_conditional_losses_47245

inputsD
1lstm_7_lstm_cell_9_matmul_readvariableop_resource:	?E
3lstm_7_lstm_cell_9_matmul_1_readvariableop_resource:2H
5lstm_7_lstm_cell_9_matmul_1_readvariableop_1_resource:	2?A
2lstm_7_lstm_cell_9_biasadd_readvariableop_resource:	?B
0lstm_7_lstm_cell_9_mul_2_readvariableop_resource:2D
1lstm_6_lstm_cell_8_matmul_readvariableop_resource:	2?E
3lstm_6_lstm_cell_8_matmul_1_readvariableop_resource:2H
5lstm_6_lstm_cell_8_matmul_1_readvariableop_1_resource:	2?A
2lstm_6_lstm_cell_8_biasadd_readvariableop_resource:	?B
0lstm_6_lstm_cell_8_mul_2_readvariableop_resource:2K
9time_distributed_3_dense_3_matmul_readvariableop_resource:2H
:time_distributed_3_dense_3_biasadd_readvariableop_resource:
identity??lstm_6/AssignVariableOp?lstm_6/AssignVariableOp_1?lstm_6/ReadVariableOp?lstm_6/ReadVariableOp_1?)lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp?(lstm_6/lstm_cell_8/MatMul/ReadVariableOp?*lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp?,lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1?'lstm_6/lstm_cell_8/mul_2/ReadVariableOp?lstm_6/while?lstm_7/AssignVariableOp?lstm_7/AssignVariableOp_1?lstm_7/ReadVariableOp?lstm_7/ReadVariableOp_1?)lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp?(lstm_7/lstm_cell_9/MatMul/ReadVariableOp?*lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp?,lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1?'lstm_7/lstm_cell_9/mul_2/ReadVariableOp?lstm_7/while?1time_distributed_3/dense_3/BiasAdd/ReadVariableOp?0time_distributed_3/dense_3/MatMul/ReadVariableOp?
lstm_7/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_7/transpose/perm?
lstm_7/transpose	Transposeinputslstm_7/transpose/perm:output:0*
T0*"
_output_shapes
:
2
lstm_7/transposeq
lstm_7/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
         2
lstm_7/Shape?
lstm_7/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_7/strided_slice/stack?
lstm_7/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_7/strided_slice/stack_1?
lstm_7/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_7/strided_slice/stack_2?
lstm_7/strided_sliceStridedSlicelstm_7/Shape:output:0#lstm_7/strided_slice/stack:output:0%lstm_7/strided_slice/stack_1:output:0%lstm_7/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_7/strided_slice?
"lstm_7/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2$
"lstm_7/TensorArrayV2/element_shape?
lstm_7/TensorArrayV2TensorListReserve+lstm_7/TensorArrayV2/element_shape:output:0lstm_7/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_7/TensorArrayV2?
<lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      2>
<lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shape?
.lstm_7/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_7/transpose:y:0Elstm_7/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type020
.lstm_7/TensorArrayUnstack/TensorListFromTensor?
lstm_7/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_7/strided_slice_1/stack?
lstm_7/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_1/stack_1?
lstm_7/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_1/stack_2?
lstm_7/strided_slice_1StridedSlicelstm_7/transpose:y:0%lstm_7/strided_slice_1/stack:output:0'lstm_7/strided_slice_1/stack_1:output:0'lstm_7/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
lstm_7/strided_slice_1?
(lstm_7/lstm_cell_9/MatMul/ReadVariableOpReadVariableOp1lstm_7_lstm_cell_9_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02*
(lstm_7/lstm_cell_9/MatMul/ReadVariableOp?
lstm_7/lstm_cell_9/MatMulMatMullstm_7/strided_slice_1:output:00lstm_7/lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_7/lstm_cell_9/MatMul?
*lstm_7/lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp3lstm_7_lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02,
*lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp?
,lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1ReadVariableOp5lstm_7_lstm_cell_9_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02.
,lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1?
lstm_7/lstm_cell_9/MatMul_1MatMul2lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp:value:04lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_7/lstm_cell_9/MatMul_1?
lstm_7/lstm_cell_9/addAddV2#lstm_7/lstm_cell_9/MatMul:product:0%lstm_7/lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_7/lstm_cell_9/add?
)lstm_7/lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp2lstm_7_lstm_cell_9_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02+
)lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp?
lstm_7/lstm_cell_9/BiasAddBiasAddlstm_7/lstm_cell_9/add:z:01lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_7/lstm_cell_9/BiasAdd?
"lstm_7/lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"lstm_7/lstm_cell_9/split/split_dim?
lstm_7/lstm_cell_9/splitSplit+lstm_7/lstm_cell_9/split/split_dim:output:0#lstm_7/lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_7/lstm_cell_9/splity
lstm_7/lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_7/lstm_cell_9/Const}
lstm_7/lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_7/lstm_cell_9/Const_1?
lstm_7/lstm_cell_9/MulMul!lstm_7/lstm_cell_9/split:output:0!lstm_7/lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Mul?
lstm_7/lstm_cell_9/Add_1AddV2lstm_7/lstm_cell_9/Mul:z:0#lstm_7/lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Add_1?
*lstm_7/lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2,
*lstm_7/lstm_cell_9/clip_by_value/Minimum/y?
(lstm_7/lstm_cell_9/clip_by_value/MinimumMinimumlstm_7/lstm_cell_9/Add_1:z:03lstm_7/lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(lstm_7/lstm_cell_9/clip_by_value/Minimum?
"lstm_7/lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"lstm_7/lstm_cell_9/clip_by_value/y?
 lstm_7/lstm_cell_9/clip_by_valueMaximum,lstm_7/lstm_cell_9/clip_by_value/Minimum:z:0+lstm_7/lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 lstm_7/lstm_cell_9/clip_by_value}
lstm_7/lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_7/lstm_cell_9/Const_2}
lstm_7/lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_7/lstm_cell_9/Const_3?
lstm_7/lstm_cell_9/Mul_1Mul!lstm_7/lstm_cell_9/split:output:1#lstm_7/lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Mul_1?
lstm_7/lstm_cell_9/Add_2AddV2lstm_7/lstm_cell_9/Mul_1:z:0#lstm_7/lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Add_2?
,lstm_7/lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_7/lstm_cell_9/clip_by_value_1/Minimum/y?
*lstm_7/lstm_cell_9/clip_by_value_1/MinimumMinimumlstm_7/lstm_cell_9/Add_2:z:05lstm_7/lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*lstm_7/lstm_cell_9/clip_by_value_1/Minimum?
$lstm_7/lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_7/lstm_cell_9/clip_by_value_1/y?
"lstm_7/lstm_cell_9/clip_by_value_1Maximum.lstm_7/lstm_cell_9/clip_by_value_1/Minimum:z:0-lstm_7/lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"lstm_7/lstm_cell_9/clip_by_value_1?
'lstm_7/lstm_cell_9/mul_2/ReadVariableOpReadVariableOp0lstm_7_lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02)
'lstm_7/lstm_cell_9/mul_2/ReadVariableOp?
lstm_7/lstm_cell_9/mul_2Mul&lstm_7/lstm_cell_9/clip_by_value_1:z:0/lstm_7/lstm_cell_9/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/mul_2?
lstm_7/lstm_cell_9/TanhTanh!lstm_7/lstm_cell_9/split:output:2*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Tanh?
lstm_7/lstm_cell_9/mul_3Mul$lstm_7/lstm_cell_9/clip_by_value:z:0lstm_7/lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/mul_3?
lstm_7/lstm_cell_9/add_3AddV2lstm_7/lstm_cell_9/mul_2:z:0lstm_7/lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/add_3}
lstm_7/lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_7/lstm_cell_9/Const_4}
lstm_7/lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_7/lstm_cell_9/Const_5?
lstm_7/lstm_cell_9/Mul_4Mul!lstm_7/lstm_cell_9/split:output:3#lstm_7/lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Mul_4?
lstm_7/lstm_cell_9/Add_4AddV2lstm_7/lstm_cell_9/Mul_4:z:0#lstm_7/lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Add_4?
,lstm_7/lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_7/lstm_cell_9/clip_by_value_2/Minimum/y?
*lstm_7/lstm_cell_9/clip_by_value_2/MinimumMinimumlstm_7/lstm_cell_9/Add_4:z:05lstm_7/lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*lstm_7/lstm_cell_9/clip_by_value_2/Minimum?
$lstm_7/lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_7/lstm_cell_9/clip_by_value_2/y?
"lstm_7/lstm_cell_9/clip_by_value_2Maximum.lstm_7/lstm_cell_9/clip_by_value_2/Minimum:z:0-lstm_7/lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"lstm_7/lstm_cell_9/clip_by_value_2?
lstm_7/lstm_cell_9/Tanh_1Tanhlstm_7/lstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/Tanh_1?
lstm_7/lstm_cell_9/mul_5Mul&lstm_7/lstm_cell_9/clip_by_value_2:z:0lstm_7/lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_7/lstm_cell_9/mul_5?
$lstm_7/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2&
$lstm_7/TensorArrayV2_1/element_shape?
lstm_7/TensorArrayV2_1TensorListReserve-lstm_7/TensorArrayV2_1/element_shape:output:0lstm_7/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_7/TensorArrayV2_1\
lstm_7/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_7/time?
lstm_7/ReadVariableOpReadVariableOp3lstm_7_lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_7/ReadVariableOp?
lstm_7/ReadVariableOp_1ReadVariableOp0lstm_7_lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_7/ReadVariableOp_1?
lstm_7/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2!
lstm_7/while/maximum_iterationsx
lstm_7/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_7/while/loop_counter?
lstm_7/whileWhile"lstm_7/while/loop_counter:output:0(lstm_7/while/maximum_iterations:output:0lstm_7/time:output:0lstm_7/TensorArrayV2_1:handle:0lstm_7/ReadVariableOp:value:0lstm_7/ReadVariableOp_1:value:0lstm_7/strided_slice:output:0>lstm_7/TensorArrayUnstack/TensorListFromTensor:output_handle:01lstm_7_lstm_cell_9_matmul_readvariableop_resource5lstm_7_lstm_cell_9_matmul_1_readvariableop_1_resource2lstm_7_lstm_cell_9_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *#
bodyR
lstm_7_while_body_46954*#
condR
lstm_7_while_cond_46953*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
lstm_7/while?
7lstm_7/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7lstm_7/TensorArrayV2Stack/TensorListStack/element_shape?
)lstm_7/TensorArrayV2Stack/TensorListStackTensorListStacklstm_7/while:output:3@lstm_7/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02+
)lstm_7/TensorArrayV2Stack/TensorListStack?
lstm_7/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
lstm_7/strided_slice_2/stack?
lstm_7/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2 
lstm_7/strided_slice_2/stack_1?
lstm_7/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_2/stack_2?
lstm_7/strided_slice_2StridedSlice2lstm_7/TensorArrayV2Stack/TensorListStack:tensor:0%lstm_7/strided_slice_2/stack:output:0'lstm_7/strided_slice_2/stack_1:output:0'lstm_7/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_7/strided_slice_2?
lstm_7/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_7/transpose_1/perm?
lstm_7/transpose_1	Transpose2lstm_7/TensorArrayV2Stack/TensorListStack:tensor:0 lstm_7/transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
lstm_7/transpose_1t
lstm_7/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_7/runtime?
lstm_7/AssignVariableOpAssignVariableOp3lstm_7_lstm_cell_9_matmul_1_readvariableop_resourcelstm_7/while:output:4^lstm_7/ReadVariableOp+^lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_7/AssignVariableOp?
lstm_7/AssignVariableOp_1AssignVariableOp0lstm_7_lstm_cell_9_mul_2_readvariableop_resourcelstm_7/while:output:5^lstm_7/ReadVariableOp_1(^lstm_7/lstm_cell_9/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_7/AssignVariableOp_1?
lstm_6/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_6/transpose/perm?
lstm_6/transpose	Transposelstm_7/transpose_1:y:0lstm_6/transpose/perm:output:0*
T0*"
_output_shapes
:
22
lstm_6/transposeq
lstm_6/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
      2   2
lstm_6/Shape?
lstm_6/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_6/strided_slice/stack?
lstm_6/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_6/strided_slice/stack_1?
lstm_6/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_6/strided_slice/stack_2?
lstm_6/strided_sliceStridedSlicelstm_6/Shape:output:0#lstm_6/strided_slice/stack:output:0%lstm_6/strided_slice/stack_1:output:0%lstm_6/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_6/strided_slice?
"lstm_6/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2$
"lstm_6/TensorArrayV2/element_shape?
lstm_6/TensorArrayV2TensorListReserve+lstm_6/TensorArrayV2/element_shape:output:0lstm_6/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_6/TensorArrayV2?
<lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2>
<lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shape?
.lstm_6/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_6/transpose:y:0Elstm_6/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type020
.lstm_6/TensorArrayUnstack/TensorListFromTensor?
lstm_6/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_6/strided_slice_1/stack?
lstm_6/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_1/stack_1?
lstm_6/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_1/stack_2?
lstm_6/strided_slice_1StridedSlicelstm_6/transpose:y:0%lstm_6/strided_slice_1/stack:output:0'lstm_6/strided_slice_1/stack_1:output:0'lstm_6/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_6/strided_slice_1?
(lstm_6/lstm_cell_8/MatMul/ReadVariableOpReadVariableOp1lstm_6_lstm_cell_8_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02*
(lstm_6/lstm_cell_8/MatMul/ReadVariableOp?
lstm_6/lstm_cell_8/MatMulMatMullstm_6/strided_slice_1:output:00lstm_6/lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_6/lstm_cell_8/MatMul?
*lstm_6/lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp3lstm_6_lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02,
*lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp?
,lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1ReadVariableOp5lstm_6_lstm_cell_8_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02.
,lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1?
lstm_6/lstm_cell_8/MatMul_1MatMul2lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp:value:04lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_6/lstm_cell_8/MatMul_1?
lstm_6/lstm_cell_8/addAddV2#lstm_6/lstm_cell_8/MatMul:product:0%lstm_6/lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_6/lstm_cell_8/add?
)lstm_6/lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp2lstm_6_lstm_cell_8_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02+
)lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp?
lstm_6/lstm_cell_8/BiasAddBiasAddlstm_6/lstm_cell_8/add:z:01lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_6/lstm_cell_8/BiasAdd?
"lstm_6/lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"lstm_6/lstm_cell_8/split/split_dim?
lstm_6/lstm_cell_8/splitSplit+lstm_6/lstm_cell_8/split/split_dim:output:0#lstm_6/lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_6/lstm_cell_8/splity
lstm_6/lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_6/lstm_cell_8/Const}
lstm_6/lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_6/lstm_cell_8/Const_1?
lstm_6/lstm_cell_8/MulMul!lstm_6/lstm_cell_8/split:output:0!lstm_6/lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Mul?
lstm_6/lstm_cell_8/Add_1AddV2lstm_6/lstm_cell_8/Mul:z:0#lstm_6/lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Add_1?
*lstm_6/lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2,
*lstm_6/lstm_cell_8/clip_by_value/Minimum/y?
(lstm_6/lstm_cell_8/clip_by_value/MinimumMinimumlstm_6/lstm_cell_8/Add_1:z:03lstm_6/lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(lstm_6/lstm_cell_8/clip_by_value/Minimum?
"lstm_6/lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"lstm_6/lstm_cell_8/clip_by_value/y?
 lstm_6/lstm_cell_8/clip_by_valueMaximum,lstm_6/lstm_cell_8/clip_by_value/Minimum:z:0+lstm_6/lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 lstm_6/lstm_cell_8/clip_by_value}
lstm_6/lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_6/lstm_cell_8/Const_2}
lstm_6/lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_6/lstm_cell_8/Const_3?
lstm_6/lstm_cell_8/Mul_1Mul!lstm_6/lstm_cell_8/split:output:1#lstm_6/lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Mul_1?
lstm_6/lstm_cell_8/Add_2AddV2lstm_6/lstm_cell_8/Mul_1:z:0#lstm_6/lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Add_2?
,lstm_6/lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_6/lstm_cell_8/clip_by_value_1/Minimum/y?
*lstm_6/lstm_cell_8/clip_by_value_1/MinimumMinimumlstm_6/lstm_cell_8/Add_2:z:05lstm_6/lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*lstm_6/lstm_cell_8/clip_by_value_1/Minimum?
$lstm_6/lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_6/lstm_cell_8/clip_by_value_1/y?
"lstm_6/lstm_cell_8/clip_by_value_1Maximum.lstm_6/lstm_cell_8/clip_by_value_1/Minimum:z:0-lstm_6/lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"lstm_6/lstm_cell_8/clip_by_value_1?
'lstm_6/lstm_cell_8/mul_2/ReadVariableOpReadVariableOp0lstm_6_lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02)
'lstm_6/lstm_cell_8/mul_2/ReadVariableOp?
lstm_6/lstm_cell_8/mul_2Mul&lstm_6/lstm_cell_8/clip_by_value_1:z:0/lstm_6/lstm_cell_8/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/mul_2?
lstm_6/lstm_cell_8/TanhTanh!lstm_6/lstm_cell_8/split:output:2*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Tanh?
lstm_6/lstm_cell_8/mul_3Mul$lstm_6/lstm_cell_8/clip_by_value:z:0lstm_6/lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/mul_3?
lstm_6/lstm_cell_8/add_3AddV2lstm_6/lstm_cell_8/mul_2:z:0lstm_6/lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/add_3}
lstm_6/lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_6/lstm_cell_8/Const_4}
lstm_6/lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_6/lstm_cell_8/Const_5?
lstm_6/lstm_cell_8/Mul_4Mul!lstm_6/lstm_cell_8/split:output:3#lstm_6/lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Mul_4?
lstm_6/lstm_cell_8/Add_4AddV2lstm_6/lstm_cell_8/Mul_4:z:0#lstm_6/lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Add_4?
,lstm_6/lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_6/lstm_cell_8/clip_by_value_2/Minimum/y?
*lstm_6/lstm_cell_8/clip_by_value_2/MinimumMinimumlstm_6/lstm_cell_8/Add_4:z:05lstm_6/lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*lstm_6/lstm_cell_8/clip_by_value_2/Minimum?
$lstm_6/lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_6/lstm_cell_8/clip_by_value_2/y?
"lstm_6/lstm_cell_8/clip_by_value_2Maximum.lstm_6/lstm_cell_8/clip_by_value_2/Minimum:z:0-lstm_6/lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"lstm_6/lstm_cell_8/clip_by_value_2?
lstm_6/lstm_cell_8/Tanh_1Tanhlstm_6/lstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/Tanh_1?
lstm_6/lstm_cell_8/mul_5Mul&lstm_6/lstm_cell_8/clip_by_value_2:z:0lstm_6/lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_6/lstm_cell_8/mul_5?
$lstm_6/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2&
$lstm_6/TensorArrayV2_1/element_shape?
lstm_6/TensorArrayV2_1TensorListReserve-lstm_6/TensorArrayV2_1/element_shape:output:0lstm_6/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_6/TensorArrayV2_1\
lstm_6/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_6/time?
lstm_6/ReadVariableOpReadVariableOp3lstm_6_lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_6/ReadVariableOp?
lstm_6/ReadVariableOp_1ReadVariableOp0lstm_6_lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_6/ReadVariableOp_1?
lstm_6/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2!
lstm_6/while/maximum_iterationsx
lstm_6/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_6/while/loop_counter?
lstm_6/whileWhile"lstm_6/while/loop_counter:output:0(lstm_6/while/maximum_iterations:output:0lstm_6/time:output:0lstm_6/TensorArrayV2_1:handle:0lstm_6/ReadVariableOp:value:0lstm_6/ReadVariableOp_1:value:0lstm_6/strided_slice:output:0>lstm_6/TensorArrayUnstack/TensorListFromTensor:output_handle:01lstm_6_lstm_cell_8_matmul_readvariableop_resource5lstm_6_lstm_cell_8_matmul_1_readvariableop_1_resource2lstm_6_lstm_cell_8_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *#
bodyR
lstm_6_while_body_47128*#
condR
lstm_6_while_cond_47127*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
lstm_6/while?
7lstm_6/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7lstm_6/TensorArrayV2Stack/TensorListStack/element_shape?
)lstm_6/TensorArrayV2Stack/TensorListStackTensorListStacklstm_6/while:output:3@lstm_6/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02+
)lstm_6/TensorArrayV2Stack/TensorListStack?
lstm_6/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
lstm_6/strided_slice_2/stack?
lstm_6/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2 
lstm_6/strided_slice_2/stack_1?
lstm_6/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_2/stack_2?
lstm_6/strided_slice_2StridedSlice2lstm_6/TensorArrayV2Stack/TensorListStack:tensor:0%lstm_6/strided_slice_2/stack:output:0'lstm_6/strided_slice_2/stack_1:output:0'lstm_6/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_6/strided_slice_2?
lstm_6/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_6/transpose_1/perm?
lstm_6/transpose_1	Transpose2lstm_6/TensorArrayV2Stack/TensorListStack:tensor:0 lstm_6/transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
lstm_6/transpose_1t
lstm_6/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_6/runtime?
lstm_6/AssignVariableOpAssignVariableOp3lstm_6_lstm_cell_8_matmul_1_readvariableop_resourcelstm_6/while:output:4^lstm_6/ReadVariableOp+^lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_6/AssignVariableOp?
lstm_6/AssignVariableOp_1AssignVariableOp0lstm_6_lstm_cell_8_mul_2_readvariableop_resourcelstm_6/while:output:5^lstm_6/ReadVariableOp_1(^lstm_6/lstm_cell_8/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_6/AssignVariableOp_1?
 time_distributed_3/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2"
 time_distributed_3/Reshape/shape?
time_distributed_3/ReshapeReshapelstm_6/transpose_1:y:0)time_distributed_3/Reshape/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_3/Reshape?
0time_distributed_3/dense_3/MatMul/ReadVariableOpReadVariableOp9time_distributed_3_dense_3_matmul_readvariableop_resource*
_output_shapes

:2*
dtype022
0time_distributed_3/dense_3/MatMul/ReadVariableOp?
!time_distributed_3/dense_3/MatMulMatMul#time_distributed_3/Reshape:output:08time_distributed_3/dense_3/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
2#
!time_distributed_3/dense_3/MatMul?
1time_distributed_3/dense_3/BiasAdd/ReadVariableOpReadVariableOp:time_distributed_3_dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype023
1time_distributed_3/dense_3/BiasAdd/ReadVariableOp?
"time_distributed_3/dense_3/BiasAddBiasAdd+time_distributed_3/dense_3/MatMul:product:09time_distributed_3/dense_3/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
2$
"time_distributed_3/dense_3/BiasAdd?
"time_distributed_3/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      2$
"time_distributed_3/Reshape_1/shape?
time_distributed_3/Reshape_1Reshape+time_distributed_3/dense_3/BiasAdd:output:0+time_distributed_3/Reshape_1/shape:output:0*
T0*"
_output_shapes
:
2
time_distributed_3/Reshape_1?
"time_distributed_3/Reshape_2/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2$
"time_distributed_3/Reshape_2/shape?
time_distributed_3/Reshape_2Reshapelstm_6/transpose_1:y:0+time_distributed_3/Reshape_2/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_3/Reshape_2{
IdentityIdentity%time_distributed_3/Reshape_1:output:0^NoOp*
T0*"
_output_shapes
:
2

Identity?
NoOpNoOp^lstm_6/AssignVariableOp^lstm_6/AssignVariableOp_1^lstm_6/ReadVariableOp^lstm_6/ReadVariableOp_1*^lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp)^lstm_6/lstm_cell_8/MatMul/ReadVariableOp+^lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp-^lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1(^lstm_6/lstm_cell_8/mul_2/ReadVariableOp^lstm_6/while^lstm_7/AssignVariableOp^lstm_7/AssignVariableOp_1^lstm_7/ReadVariableOp^lstm_7/ReadVariableOp_1*^lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp)^lstm_7/lstm_cell_9/MatMul/ReadVariableOp+^lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp-^lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1(^lstm_7/lstm_cell_9/mul_2/ReadVariableOp^lstm_7/while2^time_distributed_3/dense_3/BiasAdd/ReadVariableOp1^time_distributed_3/dense_3/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 22
lstm_6/AssignVariableOplstm_6/AssignVariableOp26
lstm_6/AssignVariableOp_1lstm_6/AssignVariableOp_12.
lstm_6/ReadVariableOplstm_6/ReadVariableOp22
lstm_6/ReadVariableOp_1lstm_6/ReadVariableOp_12V
)lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp)lstm_6/lstm_cell_8/BiasAdd/ReadVariableOp2T
(lstm_6/lstm_cell_8/MatMul/ReadVariableOp(lstm_6/lstm_cell_8/MatMul/ReadVariableOp2X
*lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp*lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp2\
,lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_1,lstm_6/lstm_cell_8/MatMul_1/ReadVariableOp_12R
'lstm_6/lstm_cell_8/mul_2/ReadVariableOp'lstm_6/lstm_cell_8/mul_2/ReadVariableOp2
lstm_6/whilelstm_6/while22
lstm_7/AssignVariableOplstm_7/AssignVariableOp26
lstm_7/AssignVariableOp_1lstm_7/AssignVariableOp_12.
lstm_7/ReadVariableOplstm_7/ReadVariableOp22
lstm_7/ReadVariableOp_1lstm_7/ReadVariableOp_12V
)lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp)lstm_7/lstm_cell_9/BiasAdd/ReadVariableOp2T
(lstm_7/lstm_cell_9/MatMul/ReadVariableOp(lstm_7/lstm_cell_9/MatMul/ReadVariableOp2X
*lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp*lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp2\
,lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_1,lstm_7/lstm_cell_9/MatMul_1/ReadVariableOp_12R
'lstm_7/lstm_cell_9/mul_2/ReadVariableOp'lstm_7/lstm_cell_9/mul_2/ReadVariableOp2
lstm_7/whilelstm_7/while2f
1time_distributed_3/dense_3/BiasAdd/ReadVariableOp1time_distributed_3/dense_3/BiasAdd/ReadVariableOp2d
0time_distributed_3/dense_3/MatMul/ReadVariableOp0time_distributed_3/dense_3/MatMul/ReadVariableOp:J F
"
_output_shapes
:

 
_user_specified_nameinputs
?m
?
A__inference_lstm_7_layer_call_and_return_conditional_losses_46287

inputs=
*lstm_cell_9_matmul_readvariableop_resource:	?>
,lstm_cell_9_matmul_1_readvariableop_resource:2A
.lstm_cell_9_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_9_biasadd_readvariableop_resource:	?;
)lstm_cell_9_mul_2_readvariableop_resource:2
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_9/BiasAdd/ReadVariableOp?!lstm_cell_9/MatMul/ReadVariableOp?#lstm_cell_9/MatMul_1/ReadVariableOp?%lstm_cell_9/MatMul_1/ReadVariableOp_1? lstm_cell_9/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permq
	transpose	Transposeinputstranspose/perm:output:0*
T0*"
_output_shapes
:
2
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
         2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_9/MatMul/ReadVariableOpReadVariableOp*lstm_cell_9_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02#
!lstm_cell_9/MatMul/ReadVariableOp?
lstm_cell_9/MatMulMatMulstrided_slice_1:output:0)lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/MatMul?
#lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02%
#lstm_cell_9/MatMul_1/ReadVariableOp?
%lstm_cell_9/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_9_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_9/MatMul_1/ReadVariableOp_1?
lstm_cell_9/MatMul_1MatMul+lstm_cell_9/MatMul_1/ReadVariableOp:value:0-lstm_cell_9/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/MatMul_1?
lstm_cell_9/addAddV2lstm_cell_9/MatMul:product:0lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_cell_9/add?
"lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_9_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_9/BiasAdd/ReadVariableOp?
lstm_cell_9/BiasAddBiasAddlstm_cell_9/add:z:0*lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/BiasAdd|
lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_9/split/split_dim?
lstm_cell_9/splitSplit$lstm_cell_9/split/split_dim:output:0lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_9/splitk
lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Consto
lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_1?
lstm_cell_9/MulMullstm_cell_9/split:output:0lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul?
lstm_cell_9/Add_1AddV2lstm_cell_9/Mul:z:0lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_1?
#lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_9/clip_by_value/Minimum/y?
!lstm_cell_9/clip_by_value/MinimumMinimumlstm_cell_9/Add_1:z:0,lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22#
!lstm_cell_9/clip_by_value/Minimum
lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value/y?
lstm_cell_9/clip_by_valueMaximum%lstm_cell_9/clip_by_value/Minimum:z:0$lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_valueo
lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Const_2o
lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_3?
lstm_cell_9/Mul_1Mullstm_cell_9/split:output:1lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul_1?
lstm_cell_9/Add_2AddV2lstm_cell_9/Mul_1:z:0lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_2?
%lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_9/clip_by_value_1/Minimum/y?
#lstm_cell_9/clip_by_value_1/MinimumMinimumlstm_cell_9/Add_2:z:0.lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_9/clip_by_value_1/Minimum?
lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value_1/y?
lstm_cell_9/clip_by_value_1Maximum'lstm_cell_9/clip_by_value_1/Minimum:z:0&lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_value_1?
 lstm_cell_9/mul_2/ReadVariableOpReadVariableOp)lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02"
 lstm_cell_9/mul_2/ReadVariableOp?
lstm_cell_9/mul_2Mullstm_cell_9/clip_by_value_1:z:0(lstm_cell_9/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_2q
lstm_cell_9/TanhTanhlstm_cell_9/split:output:2*
T0*
_output_shapes

:22
lstm_cell_9/Tanh?
lstm_cell_9/mul_3Mullstm_cell_9/clip_by_value:z:0lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_3?
lstm_cell_9/add_3AddV2lstm_cell_9/mul_2:z:0lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_9/add_3o
lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Const_4o
lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_5?
lstm_cell_9/Mul_4Mullstm_cell_9/split:output:3lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul_4?
lstm_cell_9/Add_4AddV2lstm_cell_9/Mul_4:z:0lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_4?
%lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_9/clip_by_value_2/Minimum/y?
#lstm_cell_9/clip_by_value_2/MinimumMinimumlstm_cell_9/Add_4:z:0.lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_9/clip_by_value_2/Minimum?
lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value_2/y?
lstm_cell_9/clip_by_value_2Maximum'lstm_cell_9/clip_by_value_2/Minimum:z:0&lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_value_2p
lstm_cell_9/Tanh_1Tanhlstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_9/Tanh_1?
lstm_cell_9/mul_5Mullstm_cell_9/clip_by_value_2:z:0lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time?
ReadVariableOpReadVariableOp,lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_9_matmul_readvariableop_resource.lstm_cell_9_matmul_1_readvariableop_1_resource+lstm_cell_9_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_46182*
condR
while_cond_46181*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_9_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_9/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_9_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_9/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_9/BiasAdd/ReadVariableOp"^lstm_cell_9/MatMul/ReadVariableOp$^lstm_cell_9/MatMul_1/ReadVariableOp&^lstm_cell_9/MatMul_1/ReadVariableOp_1!^lstm_cell_9/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:
: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_9/BiasAdd/ReadVariableOp"lstm_cell_9/BiasAdd/ReadVariableOp2F
!lstm_cell_9/MatMul/ReadVariableOp!lstm_cell_9/MatMul/ReadVariableOp2J
#lstm_cell_9/MatMul_1/ReadVariableOp#lstm_cell_9/MatMul_1/ReadVariableOp2N
%lstm_cell_9/MatMul_1/ReadVariableOp_1%lstm_cell_9/MatMul_1/ReadVariableOp_12D
 lstm_cell_9/mul_2/ReadVariableOp lstm_cell_9/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:

 
_user_specified_nameinputs
?,
?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_44034

inputs

states
states_11
matmul_readvariableop_resource:	?3
 matmul_1_readvariableop_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1Z
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes

:22
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:22
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes

:22
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3`
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes

:22
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:22
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:22
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:22
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:22
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:22
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5`
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes

:22
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:22
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:22
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:22
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:22

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$::2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:
 
_user_specified_nameinputs:FB

_output_shapes

:2
 
_user_specified_namestates:FB

_output_shapes

:2
 
_user_specified_namestates
?	
?
&__inference_lstm_7_layer_call_fn_48030
inputs_0
unknown:2
	unknown_0:2
	unknown_1:	?
	unknown_2:	2?
	unknown_3:	?
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_7_layer_call_and_return_conditional_losses_439462
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:?????????22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:?????????: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:?????????
"
_user_specified_name
inputs/0
?0
?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_44901

inputs
states:2
states_1:21
matmul_readvariableop_resource:	2?5
"matmul_1_readvariableop_1_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?MatMul_1/ReadVariableOp_1?mul_2/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:2*
dtype02
MatMul_1/ReadVariableOp?
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOp_1?
MatMul_1MatMulMatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1Z
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes

:22
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:22
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes

:22
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3`
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes

:22
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:22
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1u
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes

:2*
dtype02
mul_2/ReadVariableOpq
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:22
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:22
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:22
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5`
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes

:22
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:22
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:22
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:22
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:22

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^MatMul_1/ReadVariableOp_1^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:2: : : : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
?
?
+__inference_lstm_cell_9_layer_call_fn_49207

inputs
states_0
states_1
unknown:	?
	unknown_0:	2?
	unknown_1:	?
identity

identity_1

identity_2??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0states_1unknown	unknown_0	unknown_1*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:2:2:2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_440342
StatefulPartitionedCallr
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes

:22

Identityv

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*
_output_shapes

:22

Identity_1v

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*
_output_shapes

:22

Identity_2h
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$::2:2: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:
 
_user_specified_nameinputs:HD

_output_shapes

:2
"
_user_specified_name
states/0:HD

_output_shapes

:2
"
_user_specified_name
states/1
?,
?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49116

inputs
states_0
states_11
matmul_readvariableop_resource:	?3
 matmul_1_readvariableop_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1Z
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes

:22
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:22
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes

:22
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3`
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes

:22
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:22
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:22
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:22
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:22
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:22
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5`
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes

:22
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:22
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:22
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:22
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:22

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$::2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:
 
_user_specified_nameinputs:HD

_output_shapes

:2
"
_user_specified_name
states/0:HD

_output_shapes

:2
"
_user_specified_name
states/1
?
?
&__inference_lstm_7_layer_call_fn_48075

inputs
unknown:	?
	unknown_0:2
	unknown_1:	2?
	unknown_2:	?
	unknown_3:2
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_7_layer_call_and_return_conditional_losses_462872
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:
: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:

 
_user_specified_nameinputs
?
?
+__inference_lstm_cell_9_layer_call_fn_49281

inputs
states_0
states_1
unknown:	?
	unknown_0:	2?
	unknown_1:	?
identity

identity_1

identity_2??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0states_1unknown	unknown_0	unknown_1*
Tin

2*
Tout
2*
_collective_manager_ids
 * 
_output_shapes
:::*'
_read_only_resource_inputs	
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_492682
StatefulPartitionedCalll
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
:2

Identityp

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*
_output_shapes
:2

Identity_1p

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*
_output_shapes
:2

Identity_2h
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$::2:2: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
?.
?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49744

inputs

states
states_11
matmul_readvariableop_resource:	2?5
"matmul_1_readvariableop_1_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?MatMul_1/ReadVariableOp_1?mul_2/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMuls
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp?
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOp_1?
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpi
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*$
_output_shapes
::::*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1T
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes
:2
MulU
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes
:2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes
:2
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3Z
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes
:2
Mul_1W
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes
:2
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1o
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes
:*
dtype02
mul_2/ReadVariableOpk
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes
:2
mul_2G
TanhTanhsplit:output:2*
T0*
_output_shapes
:2
TanhU
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes
:2
mul_3P
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes
:2
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5Z
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes
:2
Mul_4W
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes
:2
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2F
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes
:2
Tanh_1Y
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes
:2
mul_5U
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

IdentityY

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

Identity_1Y

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes
:2

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^MatMul_1/ReadVariableOp_1^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:2:2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
?m
?
A__inference_lstm_6_layer_call_and_return_conditional_losses_48431
inputs_0=
*lstm_cell_8_matmul_readvariableop_resource:	2?>
,lstm_cell_8_matmul_1_readvariableop_resource:2A
.lstm_cell_8_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_8_biasadd_readvariableop_resource:	?;
)lstm_cell_8_mul_2_readvariableop_resource:2
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_8/BiasAdd/ReadVariableOp?!lstm_cell_8/MatMul/ReadVariableOp?#lstm_cell_8/MatMul_1/ReadVariableOp?%lstm_cell_8/MatMul_1/ReadVariableOp_1? lstm_cell_8/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:?????????22
	transposeK
ShapeShapetranspose:y:0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_8/MatMul/ReadVariableOpReadVariableOp*lstm_cell_8_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02#
!lstm_cell_8/MatMul/ReadVariableOp?
lstm_cell_8/MatMulMatMulstrided_slice_1:output:0)lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/MatMul?
#lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02%
#lstm_cell_8/MatMul_1/ReadVariableOp?
%lstm_cell_8/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_8_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_8/MatMul_1/ReadVariableOp_1?
lstm_cell_8/MatMul_1MatMul+lstm_cell_8/MatMul_1/ReadVariableOp:value:0-lstm_cell_8/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/MatMul_1?
lstm_cell_8/addAddV2lstm_cell_8/MatMul:product:0lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_cell_8/add?
"lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_8_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_8/BiasAdd/ReadVariableOp?
lstm_cell_8/BiasAddBiasAddlstm_cell_8/add:z:0*lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/BiasAdd|
lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_8/split/split_dim?
lstm_cell_8/splitSplit$lstm_cell_8/split/split_dim:output:0lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_8/splitk
lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Consto
lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_1?
lstm_cell_8/MulMullstm_cell_8/split:output:0lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul?
lstm_cell_8/Add_1AddV2lstm_cell_8/Mul:z:0lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_1?
#lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_8/clip_by_value/Minimum/y?
!lstm_cell_8/clip_by_value/MinimumMinimumlstm_cell_8/Add_1:z:0,lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22#
!lstm_cell_8/clip_by_value/Minimum
lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value/y?
lstm_cell_8/clip_by_valueMaximum%lstm_cell_8/clip_by_value/Minimum:z:0$lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_valueo
lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Const_2o
lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_3?
lstm_cell_8/Mul_1Mullstm_cell_8/split:output:1lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul_1?
lstm_cell_8/Add_2AddV2lstm_cell_8/Mul_1:z:0lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_2?
%lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_8/clip_by_value_1/Minimum/y?
#lstm_cell_8/clip_by_value_1/MinimumMinimumlstm_cell_8/Add_2:z:0.lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_8/clip_by_value_1/Minimum?
lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value_1/y?
lstm_cell_8/clip_by_value_1Maximum'lstm_cell_8/clip_by_value_1/Minimum:z:0&lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_value_1?
 lstm_cell_8/mul_2/ReadVariableOpReadVariableOp)lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02"
 lstm_cell_8/mul_2/ReadVariableOp?
lstm_cell_8/mul_2Mullstm_cell_8/clip_by_value_1:z:0(lstm_cell_8/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_2q
lstm_cell_8/TanhTanhlstm_cell_8/split:output:2*
T0*
_output_shapes

:22
lstm_cell_8/Tanh?
lstm_cell_8/mul_3Mullstm_cell_8/clip_by_value:z:0lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_3?
lstm_cell_8/add_3AddV2lstm_cell_8/mul_2:z:0lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_8/add_3o
lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Const_4o
lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_5?
lstm_cell_8/Mul_4Mullstm_cell_8/split:output:3lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul_4?
lstm_cell_8/Add_4AddV2lstm_cell_8/Mul_4:z:0lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_4?
%lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_8/clip_by_value_2/Minimum/y?
#lstm_cell_8/clip_by_value_2/MinimumMinimumlstm_cell_8/Add_4:z:0.lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_8/clip_by_value_2/Minimum?
lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value_2/y?
lstm_cell_8/clip_by_value_2Maximum'lstm_cell_8/clip_by_value_2/Minimum:z:0&lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_value_2p
lstm_cell_8/Tanh_1Tanhlstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_8/Tanh_1?
lstm_cell_8/mul_5Mullstm_cell_8/clip_by_value_2:z:0lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time?
ReadVariableOpReadVariableOp,lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_8_matmul_readvariableop_resource.lstm_cell_8_matmul_1_readvariableop_1_resource+lstm_cell_8_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_48326*
condR
while_cond_48325*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_8_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_8/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_8_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_8/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_8/BiasAdd/ReadVariableOp"^lstm_cell_8/MatMul/ReadVariableOp$^lstm_cell_8/MatMul_1/ReadVariableOp&^lstm_cell_8/MatMul_1/ReadVariableOp_1!^lstm_cell_8/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:?????????2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_8/BiasAdd/ReadVariableOp"lstm_cell_8/BiasAdd/ReadVariableOp2F
!lstm_cell_8/MatMul/ReadVariableOp!lstm_cell_8/MatMul/ReadVariableOp2J
#lstm_cell_8/MatMul_1/ReadVariableOp#lstm_cell_8/MatMul_1/ReadVariableOp2N
%lstm_cell_8/MatMul_1/ReadVariableOp_1%lstm_cell_8/MatMul_1/ReadVariableOp_12D
 lstm_cell_8/mul_2/ReadVariableOp lstm_cell_8/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:?????????2
"
_user_specified_name
inputs/0
?
?
while_cond_47909
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_47909___redundant_placeholder03
/while_while_cond_47909___redundant_placeholder13
/while_while_cond_47909___redundant_placeholder23
/while_while_cond_47909___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?$
?
while_body_43824
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0,
while_lstm_cell_9_43901_0:	?,
while_lstm_cell_9_43903_0:	2?(
while_lstm_cell_9_43905_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor*
while_lstm_cell_9_43901:	?*
while_lstm_cell_9_43903:	2?&
while_lstm_cell_9_43905:	???)while/lstm_cell_9/StatefulPartitionedCall?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
)while/lstm_cell_9/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_9_43901_0while_lstm_cell_9_43903_0while_lstm_cell_9_43905_0*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:2:2:2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_439002+
)while/lstm_cell_9/StatefulPartitionedCall?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder2while/lstm_cell_9/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identity2while/lstm_cell_9/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identity2while/lstm_cell_9/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp*^while/lstm_cell_9/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"4
while_lstm_cell_9_43901while_lstm_cell_9_43901_0"4
while_lstm_cell_9_43903while_lstm_cell_9_43903_0"4
while_lstm_cell_9_43905while_lstm_cell_9_43905_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_9/StatefulPartitionedCall)while/lstm_cell_9/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?~
?
$sequential_3_lstm_7_while_body_43436D
@sequential_3_lstm_7_while_sequential_3_lstm_7_while_loop_counterJ
Fsequential_3_lstm_7_while_sequential_3_lstm_7_while_maximum_iterations)
%sequential_3_lstm_7_while_placeholder+
'sequential_3_lstm_7_while_placeholder_1+
'sequential_3_lstm_7_while_placeholder_2+
'sequential_3_lstm_7_while_placeholder_3A
=sequential_3_lstm_7_while_sequential_3_lstm_7_strided_slice_0
{sequential_3_lstm_7_while_tensorarrayv2read_tensorlistgetitem_sequential_3_lstm_7_tensorarrayunstack_tensorlistfromtensor_0Y
Fsequential_3_lstm_7_while_lstm_cell_9_matmul_readvariableop_resource_0:	?[
Hsequential_3_lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource_0:	2?V
Gsequential_3_lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource_0:	?&
"sequential_3_lstm_7_while_identity(
$sequential_3_lstm_7_while_identity_1(
$sequential_3_lstm_7_while_identity_2(
$sequential_3_lstm_7_while_identity_3(
$sequential_3_lstm_7_while_identity_4(
$sequential_3_lstm_7_while_identity_5?
;sequential_3_lstm_7_while_sequential_3_lstm_7_strided_slice}
ysequential_3_lstm_7_while_tensorarrayv2read_tensorlistgetitem_sequential_3_lstm_7_tensorarrayunstack_tensorlistfromtensorW
Dsequential_3_lstm_7_while_lstm_cell_9_matmul_readvariableop_resource:	?Y
Fsequential_3_lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource:	2?T
Esequential_3_lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource:	???<sequential_3/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp?;sequential_3/lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp?=sequential_3/lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp?
Ksequential_3/lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      2M
Ksequential_3/lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shape?
=sequential_3/lstm_7/while/TensorArrayV2Read/TensorListGetItemTensorListGetItem{sequential_3_lstm_7_while_tensorarrayv2read_tensorlistgetitem_sequential_3_lstm_7_tensorarrayunstack_tensorlistfromtensor_0%sequential_3_lstm_7_while_placeholderTsequential_3/lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02?
=sequential_3/lstm_7/while/TensorArrayV2Read/TensorListGetItem?
;sequential_3/lstm_7/while/lstm_cell_9/MatMul/ReadVariableOpReadVariableOpFsequential_3_lstm_7_while_lstm_cell_9_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02=
;sequential_3/lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp?
,sequential_3/lstm_7/while/lstm_cell_9/MatMulMatMulDsequential_3/lstm_7/while/TensorArrayV2Read/TensorListGetItem:item:0Csequential_3/lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2.
,sequential_3/lstm_7/while/lstm_cell_9/MatMul?
=sequential_3/lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOpHsequential_3_lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02?
=sequential_3/lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp?
.sequential_3/lstm_7/while/lstm_cell_9/MatMul_1MatMul'sequential_3_lstm_7_while_placeholder_2Esequential_3/lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?20
.sequential_3/lstm_7/while/lstm_cell_9/MatMul_1?
)sequential_3/lstm_7/while/lstm_cell_9/addAddV26sequential_3/lstm_7/while/lstm_cell_9/MatMul:product:08sequential_3/lstm_7/while/lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2+
)sequential_3/lstm_7/while/lstm_cell_9/add?
<sequential_3/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOpGsequential_3_lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02>
<sequential_3/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp?
-sequential_3/lstm_7/while/lstm_cell_9/BiasAddBiasAdd-sequential_3/lstm_7/while/lstm_cell_9/add:z:0Dsequential_3/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2/
-sequential_3/lstm_7/while/lstm_cell_9/BiasAdd?
5sequential_3/lstm_7/while/lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :27
5sequential_3/lstm_7/while/lstm_cell_9/split/split_dim?
+sequential_3/lstm_7/while/lstm_cell_9/splitSplit>sequential_3/lstm_7/while/lstm_cell_9/split/split_dim:output:06sequential_3/lstm_7/while/lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2-
+sequential_3/lstm_7/while/lstm_cell_9/split?
+sequential_3/lstm_7/while/lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2-
+sequential_3/lstm_7/while/lstm_cell_9/Const?
-sequential_3/lstm_7/while/lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2/
-sequential_3/lstm_7/while/lstm_cell_9/Const_1?
)sequential_3/lstm_7/while/lstm_cell_9/MulMul4sequential_3/lstm_7/while/lstm_cell_9/split:output:04sequential_3/lstm_7/while/lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22+
)sequential_3/lstm_7/while/lstm_cell_9/Mul?
+sequential_3/lstm_7/while/lstm_cell_9/Add_1AddV2-sequential_3/lstm_7/while/lstm_cell_9/Mul:z:06sequential_3/lstm_7/while/lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_7/while/lstm_cell_9/Add_1?
=sequential_3/lstm_7/while/lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2?
=sequential_3/lstm_7/while/lstm_cell_9/clip_by_value/Minimum/y?
;sequential_3/lstm_7/while/lstm_cell_9/clip_by_value/MinimumMinimum/sequential_3/lstm_7/while/lstm_cell_9/Add_1:z:0Fsequential_3/lstm_7/while/lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22=
;sequential_3/lstm_7/while/lstm_cell_9/clip_by_value/Minimum?
5sequential_3/lstm_7/while/lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    27
5sequential_3/lstm_7/while/lstm_cell_9/clip_by_value/y?
3sequential_3/lstm_7/while/lstm_cell_9/clip_by_valueMaximum?sequential_3/lstm_7/while/lstm_cell_9/clip_by_value/Minimum:z:0>sequential_3/lstm_7/while/lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:225
3sequential_3/lstm_7/while/lstm_cell_9/clip_by_value?
-sequential_3/lstm_7/while/lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2/
-sequential_3/lstm_7/while/lstm_cell_9/Const_2?
-sequential_3/lstm_7/while/lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2/
-sequential_3/lstm_7/while/lstm_cell_9/Const_3?
+sequential_3/lstm_7/while/lstm_cell_9/Mul_1Mul4sequential_3/lstm_7/while/lstm_cell_9/split:output:16sequential_3/lstm_7/while/lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_7/while/lstm_cell_9/Mul_1?
+sequential_3/lstm_7/while/lstm_cell_9/Add_2AddV2/sequential_3/lstm_7/while/lstm_cell_9/Mul_1:z:06sequential_3/lstm_7/while/lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_7/while/lstm_cell_9/Add_2?
?sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2A
?sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum/y?
=sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_1/MinimumMinimum/sequential_3/lstm_7/while/lstm_cell_9/Add_2:z:0Hsequential_3/lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22?
=sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum?
7sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    29
7sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_1/y?
5sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_1MaximumAsequential_3/lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum:z:0@sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:227
5sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_1?
+sequential_3/lstm_7/while/lstm_cell_9/mul_2Mul9sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_1:z:0'sequential_3_lstm_7_while_placeholder_3*
T0*
_output_shapes

:22-
+sequential_3/lstm_7/while/lstm_cell_9/mul_2?
*sequential_3/lstm_7/while/lstm_cell_9/TanhTanh4sequential_3/lstm_7/while/lstm_cell_9/split:output:2*
T0*
_output_shapes

:22,
*sequential_3/lstm_7/while/lstm_cell_9/Tanh?
+sequential_3/lstm_7/while/lstm_cell_9/mul_3Mul7sequential_3/lstm_7/while/lstm_cell_9/clip_by_value:z:0.sequential_3/lstm_7/while/lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_7/while/lstm_cell_9/mul_3?
+sequential_3/lstm_7/while/lstm_cell_9/add_3AddV2/sequential_3/lstm_7/while/lstm_cell_9/mul_2:z:0/sequential_3/lstm_7/while/lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_7/while/lstm_cell_9/add_3?
-sequential_3/lstm_7/while/lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2/
-sequential_3/lstm_7/while/lstm_cell_9/Const_4?
-sequential_3/lstm_7/while/lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2/
-sequential_3/lstm_7/while/lstm_cell_9/Const_5?
+sequential_3/lstm_7/while/lstm_cell_9/Mul_4Mul4sequential_3/lstm_7/while/lstm_cell_9/split:output:36sequential_3/lstm_7/while/lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_7/while/lstm_cell_9/Mul_4?
+sequential_3/lstm_7/while/lstm_cell_9/Add_4AddV2/sequential_3/lstm_7/while/lstm_cell_9/Mul_4:z:06sequential_3/lstm_7/while/lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_7/while/lstm_cell_9/Add_4?
?sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2A
?sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum/y?
=sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_2/MinimumMinimum/sequential_3/lstm_7/while/lstm_cell_9/Add_4:z:0Hsequential_3/lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22?
=sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum?
7sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    29
7sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_2/y?
5sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_2MaximumAsequential_3/lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum:z:0@sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:227
5sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_2?
,sequential_3/lstm_7/while/lstm_cell_9/Tanh_1Tanh/sequential_3/lstm_7/while/lstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22.
,sequential_3/lstm_7/while/lstm_cell_9/Tanh_1?
+sequential_3/lstm_7/while/lstm_cell_9/mul_5Mul9sequential_3/lstm_7/while/lstm_cell_9/clip_by_value_2:z:00sequential_3/lstm_7/while/lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_7/while/lstm_cell_9/mul_5?
>sequential_3/lstm_7/while/TensorArrayV2Write/TensorListSetItemTensorListSetItem'sequential_3_lstm_7_while_placeholder_1%sequential_3_lstm_7_while_placeholder/sequential_3/lstm_7/while/lstm_cell_9/mul_5:z:0*
_output_shapes
: *
element_dtype02@
>sequential_3/lstm_7/while/TensorArrayV2Write/TensorListSetItem?
sequential_3/lstm_7/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2!
sequential_3/lstm_7/while/add/y?
sequential_3/lstm_7/while/addAddV2%sequential_3_lstm_7_while_placeholder(sequential_3/lstm_7/while/add/y:output:0*
T0*
_output_shapes
: 2
sequential_3/lstm_7/while/add?
!sequential_3/lstm_7/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2#
!sequential_3/lstm_7/while/add_1/y?
sequential_3/lstm_7/while/add_1AddV2@sequential_3_lstm_7_while_sequential_3_lstm_7_while_loop_counter*sequential_3/lstm_7/while/add_1/y:output:0*
T0*
_output_shapes
: 2!
sequential_3/lstm_7/while/add_1?
"sequential_3/lstm_7/while/IdentityIdentity#sequential_3/lstm_7/while/add_1:z:0^sequential_3/lstm_7/while/NoOp*
T0*
_output_shapes
: 2$
"sequential_3/lstm_7/while/Identity?
$sequential_3/lstm_7/while/Identity_1IdentityFsequential_3_lstm_7_while_sequential_3_lstm_7_while_maximum_iterations^sequential_3/lstm_7/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_3/lstm_7/while/Identity_1?
$sequential_3/lstm_7/while/Identity_2Identity!sequential_3/lstm_7/while/add:z:0^sequential_3/lstm_7/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_3/lstm_7/while/Identity_2?
$sequential_3/lstm_7/while/Identity_3IdentityNsequential_3/lstm_7/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^sequential_3/lstm_7/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_3/lstm_7/while/Identity_3?
$sequential_3/lstm_7/while/Identity_4Identity/sequential_3/lstm_7/while/lstm_cell_9/mul_5:z:0^sequential_3/lstm_7/while/NoOp*
T0*
_output_shapes

:22&
$sequential_3/lstm_7/while/Identity_4?
$sequential_3/lstm_7/while/Identity_5Identity/sequential_3/lstm_7/while/lstm_cell_9/add_3:z:0^sequential_3/lstm_7/while/NoOp*
T0*
_output_shapes

:22&
$sequential_3/lstm_7/while/Identity_5?
sequential_3/lstm_7/while/NoOpNoOp=^sequential_3/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp<^sequential_3/lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp>^sequential_3/lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2 
sequential_3/lstm_7/while/NoOp"Q
"sequential_3_lstm_7_while_identity+sequential_3/lstm_7/while/Identity:output:0"U
$sequential_3_lstm_7_while_identity_1-sequential_3/lstm_7/while/Identity_1:output:0"U
$sequential_3_lstm_7_while_identity_2-sequential_3/lstm_7/while/Identity_2:output:0"U
$sequential_3_lstm_7_while_identity_3-sequential_3/lstm_7/while/Identity_3:output:0"U
$sequential_3_lstm_7_while_identity_4-sequential_3/lstm_7/while/Identity_4:output:0"U
$sequential_3_lstm_7_while_identity_5-sequential_3/lstm_7/while/Identity_5:output:0"?
Esequential_3_lstm_7_while_lstm_cell_9_biasadd_readvariableop_resourceGsequential_3_lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource_0"?
Fsequential_3_lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resourceHsequential_3_lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource_0"?
Dsequential_3_lstm_7_while_lstm_cell_9_matmul_readvariableop_resourceFsequential_3_lstm_7_while_lstm_cell_9_matmul_readvariableop_resource_0"|
;sequential_3_lstm_7_while_sequential_3_lstm_7_strided_slice=sequential_3_lstm_7_while_sequential_3_lstm_7_strided_slice_0"?
ysequential_3_lstm_7_while_tensorarrayv2read_tensorlistgetitem_sequential_3_lstm_7_tensorarrayunstack_tensorlistfromtensor{sequential_3_lstm_7_while_tensorarrayv2read_tensorlistgetitem_sequential_3_lstm_7_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2|
<sequential_3/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp<sequential_3/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp2z
;sequential_3/lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp;sequential_3/lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp2~
=sequential_3/lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp=sequential_3/lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?m
?
A__inference_lstm_7_layer_call_and_return_conditional_losses_47481
inputs_0=
*lstm_cell_9_matmul_readvariableop_resource:	?>
,lstm_cell_9_matmul_1_readvariableop_resource:2A
.lstm_cell_9_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_9_biasadd_readvariableop_resource:	?;
)lstm_cell_9_mul_2_readvariableop_resource:2
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_9/BiasAdd/ReadVariableOp?!lstm_cell_9/MatMul/ReadVariableOp?#lstm_cell_9/MatMul_1/ReadVariableOp?%lstm_cell_9/MatMul_1/ReadVariableOp_1? lstm_cell_9/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:?????????2
	transposeK
ShapeShapetranspose:y:0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_9/MatMul/ReadVariableOpReadVariableOp*lstm_cell_9_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02#
!lstm_cell_9/MatMul/ReadVariableOp?
lstm_cell_9/MatMulMatMulstrided_slice_1:output:0)lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/MatMul?
#lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02%
#lstm_cell_9/MatMul_1/ReadVariableOp?
%lstm_cell_9/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_9_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_9/MatMul_1/ReadVariableOp_1?
lstm_cell_9/MatMul_1MatMul+lstm_cell_9/MatMul_1/ReadVariableOp:value:0-lstm_cell_9/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/MatMul_1?
lstm_cell_9/addAddV2lstm_cell_9/MatMul:product:0lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_cell_9/add?
"lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_9_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_9/BiasAdd/ReadVariableOp?
lstm_cell_9/BiasAddBiasAddlstm_cell_9/add:z:0*lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_9/BiasAdd|
lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_9/split/split_dim?
lstm_cell_9/splitSplit$lstm_cell_9/split/split_dim:output:0lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_9/splitk
lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Consto
lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_1?
lstm_cell_9/MulMullstm_cell_9/split:output:0lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul?
lstm_cell_9/Add_1AddV2lstm_cell_9/Mul:z:0lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_1?
#lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_9/clip_by_value/Minimum/y?
!lstm_cell_9/clip_by_value/MinimumMinimumlstm_cell_9/Add_1:z:0,lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22#
!lstm_cell_9/clip_by_value/Minimum
lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value/y?
lstm_cell_9/clip_by_valueMaximum%lstm_cell_9/clip_by_value/Minimum:z:0$lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_valueo
lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Const_2o
lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_3?
lstm_cell_9/Mul_1Mullstm_cell_9/split:output:1lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul_1?
lstm_cell_9/Add_2AddV2lstm_cell_9/Mul_1:z:0lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_2?
%lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_9/clip_by_value_1/Minimum/y?
#lstm_cell_9/clip_by_value_1/MinimumMinimumlstm_cell_9/Add_2:z:0.lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_9/clip_by_value_1/Minimum?
lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value_1/y?
lstm_cell_9/clip_by_value_1Maximum'lstm_cell_9/clip_by_value_1/Minimum:z:0&lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_value_1?
 lstm_cell_9/mul_2/ReadVariableOpReadVariableOp)lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02"
 lstm_cell_9/mul_2/ReadVariableOp?
lstm_cell_9/mul_2Mullstm_cell_9/clip_by_value_1:z:0(lstm_cell_9/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_2q
lstm_cell_9/TanhTanhlstm_cell_9/split:output:2*
T0*
_output_shapes

:22
lstm_cell_9/Tanh?
lstm_cell_9/mul_3Mullstm_cell_9/clip_by_value:z:0lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_3?
lstm_cell_9/add_3AddV2lstm_cell_9/mul_2:z:0lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_9/add_3o
lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_9/Const_4o
lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_9/Const_5?
lstm_cell_9/Mul_4Mullstm_cell_9/split:output:3lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Mul_4?
lstm_cell_9/Add_4AddV2lstm_cell_9/Mul_4:z:0lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_9/Add_4?
%lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_9/clip_by_value_2/Minimum/y?
#lstm_cell_9/clip_by_value_2/MinimumMinimumlstm_cell_9/Add_4:z:0.lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_9/clip_by_value_2/Minimum?
lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_9/clip_by_value_2/y?
lstm_cell_9/clip_by_value_2Maximum'lstm_cell_9/clip_by_value_2/Minimum:z:0&lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_9/clip_by_value_2p
lstm_cell_9/Tanh_1Tanhlstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_9/Tanh_1?
lstm_cell_9/mul_5Mullstm_cell_9/clip_by_value_2:z:0lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_9/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time?
ReadVariableOpReadVariableOp,lstm_cell_9_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_9_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_9_matmul_readvariableop_resource.lstm_cell_9_matmul_1_readvariableop_1_resource+lstm_cell_9_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_47376*
condR
while_cond_47375*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_9_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_9/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_9_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_9/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_9/BiasAdd/ReadVariableOp"^lstm_cell_9/MatMul/ReadVariableOp$^lstm_cell_9/MatMul_1/ReadVariableOp&^lstm_cell_9/MatMul_1/ReadVariableOp_1!^lstm_cell_9/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:?????????: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_9/BiasAdd/ReadVariableOp"lstm_cell_9/BiasAdd/ReadVariableOp2F
!lstm_cell_9/MatMul/ReadVariableOp!lstm_cell_9/MatMul/ReadVariableOp2J
#lstm_cell_9/MatMul_1/ReadVariableOp#lstm_cell_9/MatMul_1/ReadVariableOp2N
%lstm_cell_9/MatMul_1/ReadVariableOp_1%lstm_cell_9/MatMul_1/ReadVariableOp_12D
 lstm_cell_9/mul_2/ReadVariableOp lstm_cell_9/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:?????????
"
_user_specified_name
inputs/0
?m
?
A__inference_lstm_6_layer_call_and_return_conditional_losses_48609

inputs=
*lstm_cell_8_matmul_readvariableop_resource:	2?>
,lstm_cell_8_matmul_1_readvariableop_resource:2A
.lstm_cell_8_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_8_biasadd_readvariableop_resource:	?;
)lstm_cell_8_mul_2_readvariableop_resource:2
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_8/BiasAdd/ReadVariableOp?!lstm_cell_8/MatMul/ReadVariableOp?#lstm_cell_8/MatMul_1/ReadVariableOp?%lstm_cell_8/MatMul_1/ReadVariableOp_1? lstm_cell_8/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permq
	transpose	Transposeinputstranspose/perm:output:0*
T0*"
_output_shapes
:
22
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
      2   2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_8/MatMul/ReadVariableOpReadVariableOp*lstm_cell_8_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02#
!lstm_cell_8/MatMul/ReadVariableOp?
lstm_cell_8/MatMulMatMulstrided_slice_1:output:0)lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/MatMul?
#lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02%
#lstm_cell_8/MatMul_1/ReadVariableOp?
%lstm_cell_8/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_8_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_8/MatMul_1/ReadVariableOp_1?
lstm_cell_8/MatMul_1MatMul+lstm_cell_8/MatMul_1/ReadVariableOp:value:0-lstm_cell_8/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/MatMul_1?
lstm_cell_8/addAddV2lstm_cell_8/MatMul:product:0lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_cell_8/add?
"lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_8_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_8/BiasAdd/ReadVariableOp?
lstm_cell_8/BiasAddBiasAddlstm_cell_8/add:z:0*lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
lstm_cell_8/BiasAdd|
lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_8/split/split_dim?
lstm_cell_8/splitSplit$lstm_cell_8/split/split_dim:output:0lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_8/splitk
lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Consto
lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_1?
lstm_cell_8/MulMullstm_cell_8/split:output:0lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul?
lstm_cell_8/Add_1AddV2lstm_cell_8/Mul:z:0lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_1?
#lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_8/clip_by_value/Minimum/y?
!lstm_cell_8/clip_by_value/MinimumMinimumlstm_cell_8/Add_1:z:0,lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22#
!lstm_cell_8/clip_by_value/Minimum
lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value/y?
lstm_cell_8/clip_by_valueMaximum%lstm_cell_8/clip_by_value/Minimum:z:0$lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_valueo
lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Const_2o
lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_3?
lstm_cell_8/Mul_1Mullstm_cell_8/split:output:1lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul_1?
lstm_cell_8/Add_2AddV2lstm_cell_8/Mul_1:z:0lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_2?
%lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_8/clip_by_value_1/Minimum/y?
#lstm_cell_8/clip_by_value_1/MinimumMinimumlstm_cell_8/Add_2:z:0.lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_8/clip_by_value_1/Minimum?
lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value_1/y?
lstm_cell_8/clip_by_value_1Maximum'lstm_cell_8/clip_by_value_1/Minimum:z:0&lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_value_1?
 lstm_cell_8/mul_2/ReadVariableOpReadVariableOp)lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02"
 lstm_cell_8/mul_2/ReadVariableOp?
lstm_cell_8/mul_2Mullstm_cell_8/clip_by_value_1:z:0(lstm_cell_8/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_2q
lstm_cell_8/TanhTanhlstm_cell_8/split:output:2*
T0*
_output_shapes

:22
lstm_cell_8/Tanh?
lstm_cell_8/mul_3Mullstm_cell_8/clip_by_value:z:0lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_3?
lstm_cell_8/add_3AddV2lstm_cell_8/mul_2:z:0lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_8/add_3o
lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_8/Const_4o
lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_8/Const_5?
lstm_cell_8/Mul_4Mullstm_cell_8/split:output:3lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Mul_4?
lstm_cell_8/Add_4AddV2lstm_cell_8/Mul_4:z:0lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_8/Add_4?
%lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_8/clip_by_value_2/Minimum/y?
#lstm_cell_8/clip_by_value_2/MinimumMinimumlstm_cell_8/Add_4:z:0.lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22%
#lstm_cell_8/clip_by_value_2/Minimum?
lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_8/clip_by_value_2/y?
lstm_cell_8/clip_by_value_2Maximum'lstm_cell_8/clip_by_value_2/Minimum:z:0&lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_8/clip_by_value_2p
lstm_cell_8/Tanh_1Tanhlstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_8/Tanh_1?
lstm_cell_8/mul_5Mullstm_cell_8/clip_by_value_2:z:0lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_8/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time?
ReadVariableOpReadVariableOp,lstm_cell_8_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_8_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_8_matmul_readvariableop_resource.lstm_cell_8_matmul_1_readvariableop_1_resource+lstm_cell_8_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_48504*
condR
while_cond_48503*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_2y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_8_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_8/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_8_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_8/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_8/BiasAdd/ReadVariableOp"^lstm_cell_8/MatMul/ReadVariableOp$^lstm_cell_8/MatMul_1/ReadVariableOp&^lstm_cell_8/MatMul_1/ReadVariableOp_1!^lstm_cell_8/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:
2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_8/BiasAdd/ReadVariableOp"lstm_cell_8/BiasAdd/ReadVariableOp2F
!lstm_cell_8/MatMul/ReadVariableOp!lstm_cell_8/MatMul/ReadVariableOp2J
#lstm_cell_8/MatMul_1/ReadVariableOp#lstm_cell_8/MatMul_1/ReadVariableOp2N
%lstm_cell_8/MatMul_1/ReadVariableOp_1%lstm_cell_8/MatMul_1/ReadVariableOp_12D
 lstm_cell_8/mul_2/ReadVariableOp lstm_cell_8/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
?X
?
while_body_48148
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_8_matmul_readvariableop_resource_0:	2?G
4while_lstm_cell_8_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_8_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_8_matmul_readvariableop_resource:	2?E
2while_lstm_cell_8_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_8_biasadd_readvariableop_resource:	???(while/lstm_cell_8/BiasAdd/ReadVariableOp?'while/lstm_cell_8/MatMul/ReadVariableOp?)while/lstm_cell_8/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_8/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_8_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02)
'while/lstm_cell_8/MatMul/ReadVariableOp?
while/lstm_cell_8/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/MatMul?
)while/lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_8_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_8/MatMul_1/ReadVariableOp?
while/lstm_cell_8/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_8/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/MatMul_1?
while/lstm_cell_8/addAddV2"while/lstm_cell_8/MatMul:product:0$while/lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/add?
(while/lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_8_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_8/BiasAdd/ReadVariableOp?
while/lstm_cell_8/BiasAddBiasAddwhile/lstm_cell_8/add:z:00while/lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/BiasAdd?
!while/lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_8/split/split_dim?
while/lstm_cell_8/splitSplit*while/lstm_cell_8/split/split_dim:output:0"while/lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_8/splitw
while/lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const{
while/lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_1?
while/lstm_cell_8/MulMul while/lstm_cell_8/split:output:0 while/lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul?
while/lstm_cell_8/Add_1AddV2while/lstm_cell_8/Mul:z:0"while/lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_1?
)while/lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_8/clip_by_value/Minimum/y?
'while/lstm_cell_8/clip_by_value/MinimumMinimumwhile/lstm_cell_8/Add_1:z:02while/lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22)
'while/lstm_cell_8/clip_by_value/Minimum?
!while/lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_8/clip_by_value/y?
while/lstm_cell_8/clip_by_valueMaximum+while/lstm_cell_8/clip_by_value/Minimum:z:0*while/lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22!
while/lstm_cell_8/clip_by_value{
while/lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const_2{
while/lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_3?
while/lstm_cell_8/Mul_1Mul while/lstm_cell_8/split:output:1"while/lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul_1?
while/lstm_cell_8/Add_2AddV2while/lstm_cell_8/Mul_1:z:0"while/lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_2?
+while/lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_8/clip_by_value_1/Minimum/y?
)while/lstm_cell_8/clip_by_value_1/MinimumMinimumwhile/lstm_cell_8/Add_2:z:04while/lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_8/clip_by_value_1/Minimum?
#while/lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_8/clip_by_value_1/y?
!while/lstm_cell_8/clip_by_value_1Maximum-while/lstm_cell_8/clip_by_value_1/Minimum:z:0,while/lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_8/clip_by_value_1?
while/lstm_cell_8/mul_2Mul%while/lstm_cell_8/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_2?
while/lstm_cell_8/TanhTanh while/lstm_cell_8/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_8/Tanh?
while/lstm_cell_8/mul_3Mul#while/lstm_cell_8/clip_by_value:z:0while/lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_3?
while/lstm_cell_8/add_3AddV2while/lstm_cell_8/mul_2:z:0while/lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_8/add_3{
while/lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const_4{
while/lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_5?
while/lstm_cell_8/Mul_4Mul while/lstm_cell_8/split:output:3"while/lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul_4?
while/lstm_cell_8/Add_4AddV2while/lstm_cell_8/Mul_4:z:0"while/lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_4?
+while/lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_8/clip_by_value_2/Minimum/y?
)while/lstm_cell_8/clip_by_value_2/MinimumMinimumwhile/lstm_cell_8/Add_4:z:04while/lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_8/clip_by_value_2/Minimum?
#while/lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_8/clip_by_value_2/y?
!while/lstm_cell_8/clip_by_value_2Maximum-while/lstm_cell_8/clip_by_value_2/Minimum:z:0,while/lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_8/clip_by_value_2?
while/lstm_cell_8/Tanh_1Tanhwhile/lstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Tanh_1?
while/lstm_cell_8/mul_5Mul%while/lstm_cell_8/clip_by_value_2:z:0while/lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_8/mul_5:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_8/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_8/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_8/BiasAdd/ReadVariableOp(^while/lstm_cell_8/MatMul/ReadVariableOp*^while/lstm_cell_8/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"h
1while_lstm_cell_8_biasadd_readvariableop_resource3while_lstm_cell_8_biasadd_readvariableop_resource_0"j
2while_lstm_cell_8_matmul_1_readvariableop_resource4while_lstm_cell_8_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_8_matmul_readvariableop_resource2while_lstm_cell_8_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2T
(while/lstm_cell_8/BiasAdd/ReadVariableOp(while/lstm_cell_8/BiasAdd/ReadVariableOp2R
'while/lstm_cell_8/MatMul/ReadVariableOp'while/lstm_cell_8/MatMul/ReadVariableOp2V
)while/lstm_cell_8/MatMul_1/ReadVariableOp)while/lstm_cell_8/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?.
?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49010

inputs
states_0
states_11
matmul_readvariableop_resource:	?5
"matmul_1_readvariableop_1_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?MatMul_1/ReadVariableOp_1?mul_2/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMulu
MatMul_1/ReadVariableOpReadVariableOpstates_0*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp?
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOp_1?
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpi
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*$
_output_shapes
::::*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1T
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes
:2
MulU
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes
:2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes
:2
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3Z
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes
:2
Mul_1W
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes
:2
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1o
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes
:*
dtype02
mul_2/ReadVariableOpk
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes
:2
mul_2G
TanhTanhsplit:output:2*
T0*
_output_shapes
:2
TanhU
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes
:2
mul_3P
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes
:2
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5Z
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes
:2
Mul_4W
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes
:2
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2F
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes
:2
Tanh_1Y
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes
:2
mul_5U
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

IdentityY

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

Identity_1Y

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes
:2

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^MatMul_1/ReadVariableOp_1^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$::2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
?
?
while_cond_48681
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_48681___redundant_placeholder03
/while_while_cond_48681___redundant_placeholder13
/while_while_cond_48681___redundant_placeholder23
/while_while_cond_48681___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?
?
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_48917

inputs8
&dense_3_matmul_readvariableop_resource:25
'dense_3_biasadd_readvariableop_resource:
identity??dense_3/BiasAdd/ReadVariableOp?dense_3/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2
Reshape/shapef
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes

:
22	
Reshape?
dense_3/MatMul/ReadVariableOpReadVariableOp&dense_3_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_3/MatMul/ReadVariableOp?
dense_3/MatMulMatMulReshape:output:0%dense_3/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_3/MatMul?
dense_3/BiasAdd/ReadVariableOpReadVariableOp'dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_3/BiasAdd/ReadVariableOp?
dense_3/BiasAddBiasAdddense_3/MatMul:product:0&dense_3/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_3/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      2
Reshape_1/shape?
	Reshape_1Reshapedense_3/BiasAdd:output:0Reshape_1/shape:output:0*
T0*"
_output_shapes
:
2
	Reshape_1h
IdentityIdentityReshape_1:output:0^NoOp*
T0*"
_output_shapes
:
2

Identity?
NoOpNoOp^dense_3/BiasAdd/ReadVariableOp^dense_3/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:
2: : 2@
dense_3/BiasAdd/ReadVariableOpdense_3/BiasAdd/ReadVariableOp2>
dense_3/MatMul/ReadVariableOpdense_3/MatMul/ReadVariableOp:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
?
?
&__inference_lstm_7_layer_call_fn_48060

inputs
unknown:	?
	unknown_0:2
	unknown_1:	2?
	unknown_2:	?
	unknown_3:2
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_7_layer_call_and_return_conditional_losses_456022
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:
: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:

 
_user_specified_nameinputs
?
?
2__inference_time_distributed_3_layer_call_fn_48926

inputs
unknown:2
	unknown_0:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *4
_output_shapes"
 :??????????????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_453142
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*4
_output_shapes"
 :??????????????????2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:??????????????????2: : 22
StatefulPartitionedCallStatefulPartitionedCall:\ X
4
_output_shapes"
 :??????????????????2
 
_user_specified_nameinputs
?
?
+__inference_lstm_cell_8_layer_call_fn_49683

inputs
states_0
states_1
unknown:	2?
	unknown_0:	2?
	unknown_1:	?
identity

identity_1

identity_2??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0states_1unknown	unknown_0	unknown_1*
Tin

2*
Tout
2*
_collective_manager_ids
 * 
_output_shapes
:::*'
_read_only_resource_inputs	
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_496702
StatefulPartitionedCalll
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
:2

Identityp

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*
_output_shapes
:2

Identity_1p

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*
_output_shapes
:2

Identity_2h
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:2:2:2: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:2
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
?.
?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49268

inputs

states
states_11
matmul_readvariableop_resource:	?5
"matmul_1_readvariableop_1_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?MatMul_1/ReadVariableOp_1?mul_2/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMuls
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp?
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOp_1?
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpi
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*$
_output_shapes
::::*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1T
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes
:2
MulU
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes
:2
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes
:2
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3Z
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes
:2
Mul_1W
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes
:2
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes
:2
clip_by_value_1o
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes
:*
dtype02
mul_2/ReadVariableOpk
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes
:2
mul_2G
TanhTanhsplit:output:2*
T0*
_output_shapes
:2
TanhU
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes
:2
mul_3P
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes
:2
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5Z
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes
:2
Mul_4W
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes
:2
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes
:2
clip_by_value_2F
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes
:2
Tanh_1Y
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes
:2
mul_5U
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

IdentityY

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes
:2

Identity_1Y

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes
:2

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^MatMul_1/ReadVariableOp_1^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$::2:2: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
?0
?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_44125

inputs
states:2
states_1:21
matmul_readvariableop_resource:	?5
"matmul_1_readvariableop_1_resource:	2?.
biasadd_readvariableop_resource:	?
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?MatMul_1/ReadVariableOp_1?mul_2/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	?*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:2*
dtype02
MatMul_1/ReadVariableOp?
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOp_1?
MatMul_1MatMulMatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
splitS
ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
ConstW
Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_1Z
MulMulsplit:output:0Const:output:0*
T0*
_output_shapes

:22
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:22
Add_1w
clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value/Minimum/y?
clip_by_value/MinimumMinimum	Add_1:z:0 clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value/Minimumg
clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value/y?
clip_by_valueMaximumclip_by_value/Minimum:z:0clip_by_value/y:output:0*
T0*
_output_shapes

:22
clip_by_valueW
Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_2W
Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_3`
Mul_1Mulsplit:output:1Const_2:output:0*
T0*
_output_shapes

:22
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:22
Add_2{
clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_1/Minimum/y?
clip_by_value_1/MinimumMinimum	Add_2:z:0"clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1/Minimumk
clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_1/y?
clip_by_value_1Maximumclip_by_value_1/Minimum:z:0clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
clip_by_value_1u
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes

:2*
dtype02
mul_2/ReadVariableOpq
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:22
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:22
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:22
add_3W
Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2	
Const_4W
Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2	
Const_5`
Mul_4Mulsplit:output:3Const_4:output:0*
T0*
_output_shapes

:22
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:22
Add_4{
clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2
clip_by_value_2/Minimum/y?
clip_by_value_2/MinimumMinimum	Add_4:z:0"clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2/Minimumk
clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
clip_by_value_2/y?
clip_by_value_2Maximumclip_by_value_2/Minimum:z:0clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:22
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:22
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:22

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:22

Identity_2?
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp^MatMul_1/ReadVariableOp_1^mul_2/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*'
_input_shapes
:: : : : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
?X
?
while_body_47554
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_9_matmul_readvariableop_resource_0:	?G
4while_lstm_cell_9_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_9_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_9_matmul_readvariableop_resource:	?E
2while_lstm_cell_9_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_9_biasadd_readvariableop_resource:	???(while/lstm_cell_9/BiasAdd/ReadVariableOp?'while/lstm_cell_9/MatMul/ReadVariableOp?)while/lstm_cell_9/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_9/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_9_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02)
'while/lstm_cell_9/MatMul/ReadVariableOp?
while/lstm_cell_9/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/MatMul?
)while/lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_9_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_9/MatMul_1/ReadVariableOp?
while/lstm_cell_9/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_9/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/MatMul_1?
while/lstm_cell_9/addAddV2"while/lstm_cell_9/MatMul:product:0$while/lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/add?
(while/lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_9_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_9/BiasAdd/ReadVariableOp?
while/lstm_cell_9/BiasAddBiasAddwhile/lstm_cell_9/add:z:00while/lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/BiasAdd?
!while/lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_9/split/split_dim?
while/lstm_cell_9/splitSplit*while/lstm_cell_9/split/split_dim:output:0"while/lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_9/splitw
while/lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const{
while/lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_1?
while/lstm_cell_9/MulMul while/lstm_cell_9/split:output:0 while/lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul?
while/lstm_cell_9/Add_1AddV2while/lstm_cell_9/Mul:z:0"while/lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_1?
)while/lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_9/clip_by_value/Minimum/y?
'while/lstm_cell_9/clip_by_value/MinimumMinimumwhile/lstm_cell_9/Add_1:z:02while/lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22)
'while/lstm_cell_9/clip_by_value/Minimum?
!while/lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_9/clip_by_value/y?
while/lstm_cell_9/clip_by_valueMaximum+while/lstm_cell_9/clip_by_value/Minimum:z:0*while/lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22!
while/lstm_cell_9/clip_by_value{
while/lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const_2{
while/lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_3?
while/lstm_cell_9/Mul_1Mul while/lstm_cell_9/split:output:1"while/lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul_1?
while/lstm_cell_9/Add_2AddV2while/lstm_cell_9/Mul_1:z:0"while/lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_2?
+while/lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_9/clip_by_value_1/Minimum/y?
)while/lstm_cell_9/clip_by_value_1/MinimumMinimumwhile/lstm_cell_9/Add_2:z:04while/lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_9/clip_by_value_1/Minimum?
#while/lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_9/clip_by_value_1/y?
!while/lstm_cell_9/clip_by_value_1Maximum-while/lstm_cell_9/clip_by_value_1/Minimum:z:0,while/lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_9/clip_by_value_1?
while/lstm_cell_9/mul_2Mul%while/lstm_cell_9/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_2?
while/lstm_cell_9/TanhTanh while/lstm_cell_9/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_9/Tanh?
while/lstm_cell_9/mul_3Mul#while/lstm_cell_9/clip_by_value:z:0while/lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_3?
while/lstm_cell_9/add_3AddV2while/lstm_cell_9/mul_2:z:0while/lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_9/add_3{
while/lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const_4{
while/lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_5?
while/lstm_cell_9/Mul_4Mul while/lstm_cell_9/split:output:3"while/lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul_4?
while/lstm_cell_9/Add_4AddV2while/lstm_cell_9/Mul_4:z:0"while/lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_4?
+while/lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_9/clip_by_value_2/Minimum/y?
)while/lstm_cell_9/clip_by_value_2/MinimumMinimumwhile/lstm_cell_9/Add_4:z:04while/lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_9/clip_by_value_2/Minimum?
#while/lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_9/clip_by_value_2/y?
!while/lstm_cell_9/clip_by_value_2Maximum-while/lstm_cell_9/clip_by_value_2/Minimum:z:0,while/lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_9/clip_by_value_2?
while/lstm_cell_9/Tanh_1Tanhwhile/lstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Tanh_1?
while/lstm_cell_9/mul_5Mul%while/lstm_cell_9/clip_by_value_2:z:0while/lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_9/mul_5:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_9/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_9/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_9/BiasAdd/ReadVariableOp(^while/lstm_cell_9/MatMul/ReadVariableOp*^while/lstm_cell_9/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"h
1while_lstm_cell_9_biasadd_readvariableop_resource3while_lstm_cell_9_biasadd_readvariableop_resource_0"j
2while_lstm_cell_9_matmul_1_readvariableop_resource4while_lstm_cell_9_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_9_matmul_readvariableop_resource2while_lstm_cell_9_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2T
(while/lstm_cell_9/BiasAdd/ReadVariableOp(while/lstm_cell_9/BiasAdd/ReadVariableOp2R
'while/lstm_cell_9/MatMul/ReadVariableOp'while/lstm_cell_9/MatMul/ReadVariableOp2V
)while/lstm_cell_9/MatMul_1/ReadVariableOp)while/lstm_cell_9/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?

?
#__inference_signature_wrapper_46517
lstm_7_input
unknown:	?
	unknown_0:2
	unknown_1:	2?
	unknown_2:	?
	unknown_3:2
	unknown_4:	2?
	unknown_5:2
	unknown_6:	2?
	unknown_7:	?
	unknown_8:2
	unknown_9:2

unknown_10:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCalllstm_7_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8? *)
f$R"
 __inference__wrapped_model_437272
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
"
_output_shapes
:

&
_user_specified_namelstm_7_input
?
?
'__inference_dense_3_layer_call_fn_49776

inputs
unknown:2
	unknown_0:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_dense_3_layer_call_and_return_conditional_losses_453032
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????2: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????2
 
_user_specified_nameinputs
?
?
while_cond_48503
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_48503___redundant_placeholder03
/while_while_cond_48503___redundant_placeholder13
/while_while_cond_48503___redundant_placeholder23
/while_while_cond_48503___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?
?
$sequential_3_lstm_7_while_cond_43435D
@sequential_3_lstm_7_while_sequential_3_lstm_7_while_loop_counterJ
Fsequential_3_lstm_7_while_sequential_3_lstm_7_while_maximum_iterations)
%sequential_3_lstm_7_while_placeholder+
'sequential_3_lstm_7_while_placeholder_1+
'sequential_3_lstm_7_while_placeholder_2+
'sequential_3_lstm_7_while_placeholder_3D
@sequential_3_lstm_7_while_less_sequential_3_lstm_7_strided_slice[
Wsequential_3_lstm_7_while_sequential_3_lstm_7_while_cond_43435___redundant_placeholder0[
Wsequential_3_lstm_7_while_sequential_3_lstm_7_while_cond_43435___redundant_placeholder1[
Wsequential_3_lstm_7_while_sequential_3_lstm_7_while_cond_43435___redundant_placeholder2[
Wsequential_3_lstm_7_while_sequential_3_lstm_7_while_cond_43435___redundant_placeholder3&
"sequential_3_lstm_7_while_identity
?
sequential_3/lstm_7/while/LessLess%sequential_3_lstm_7_while_placeholder@sequential_3_lstm_7_while_less_sequential_3_lstm_7_strided_slice*
T0*
_output_shapes
: 2 
sequential_3/lstm_7/while/Less?
"sequential_3/lstm_7/while/IdentityIdentity"sequential_3/lstm_7/while/Less:z:0*
T0
*
_output_shapes
: 2$
"sequential_3/lstm_7/while/Identity"Q
"sequential_3_lstm_7_while_identity+sequential_3/lstm_7/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?~
?
$sequential_3_lstm_6_while_body_43610D
@sequential_3_lstm_6_while_sequential_3_lstm_6_while_loop_counterJ
Fsequential_3_lstm_6_while_sequential_3_lstm_6_while_maximum_iterations)
%sequential_3_lstm_6_while_placeholder+
'sequential_3_lstm_6_while_placeholder_1+
'sequential_3_lstm_6_while_placeholder_2+
'sequential_3_lstm_6_while_placeholder_3A
=sequential_3_lstm_6_while_sequential_3_lstm_6_strided_slice_0
{sequential_3_lstm_6_while_tensorarrayv2read_tensorlistgetitem_sequential_3_lstm_6_tensorarrayunstack_tensorlistfromtensor_0Y
Fsequential_3_lstm_6_while_lstm_cell_8_matmul_readvariableop_resource_0:	2?[
Hsequential_3_lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource_0:	2?V
Gsequential_3_lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource_0:	?&
"sequential_3_lstm_6_while_identity(
$sequential_3_lstm_6_while_identity_1(
$sequential_3_lstm_6_while_identity_2(
$sequential_3_lstm_6_while_identity_3(
$sequential_3_lstm_6_while_identity_4(
$sequential_3_lstm_6_while_identity_5?
;sequential_3_lstm_6_while_sequential_3_lstm_6_strided_slice}
ysequential_3_lstm_6_while_tensorarrayv2read_tensorlistgetitem_sequential_3_lstm_6_tensorarrayunstack_tensorlistfromtensorW
Dsequential_3_lstm_6_while_lstm_cell_8_matmul_readvariableop_resource:	2?Y
Fsequential_3_lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource:	2?T
Esequential_3_lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource:	???<sequential_3/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp?;sequential_3/lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp?=sequential_3/lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp?
Ksequential_3/lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2M
Ksequential_3/lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shape?
=sequential_3/lstm_6/while/TensorArrayV2Read/TensorListGetItemTensorListGetItem{sequential_3_lstm_6_while_tensorarrayv2read_tensorlistgetitem_sequential_3_lstm_6_tensorarrayunstack_tensorlistfromtensor_0%sequential_3_lstm_6_while_placeholderTsequential_3/lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02?
=sequential_3/lstm_6/while/TensorArrayV2Read/TensorListGetItem?
;sequential_3/lstm_6/while/lstm_cell_8/MatMul/ReadVariableOpReadVariableOpFsequential_3_lstm_6_while_lstm_cell_8_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02=
;sequential_3/lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp?
,sequential_3/lstm_6/while/lstm_cell_8/MatMulMatMulDsequential_3/lstm_6/while/TensorArrayV2Read/TensorListGetItem:item:0Csequential_3/lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2.
,sequential_3/lstm_6/while/lstm_cell_8/MatMul?
=sequential_3/lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOpHsequential_3_lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02?
=sequential_3/lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp?
.sequential_3/lstm_6/while/lstm_cell_8/MatMul_1MatMul'sequential_3_lstm_6_while_placeholder_2Esequential_3/lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?20
.sequential_3/lstm_6/while/lstm_cell_8/MatMul_1?
)sequential_3/lstm_6/while/lstm_cell_8/addAddV26sequential_3/lstm_6/while/lstm_cell_8/MatMul:product:08sequential_3/lstm_6/while/lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2+
)sequential_3/lstm_6/while/lstm_cell_8/add?
<sequential_3/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOpGsequential_3_lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02>
<sequential_3/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp?
-sequential_3/lstm_6/while/lstm_cell_8/BiasAddBiasAdd-sequential_3/lstm_6/while/lstm_cell_8/add:z:0Dsequential_3/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2/
-sequential_3/lstm_6/while/lstm_cell_8/BiasAdd?
5sequential_3/lstm_6/while/lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :27
5sequential_3/lstm_6/while/lstm_cell_8/split/split_dim?
+sequential_3/lstm_6/while/lstm_cell_8/splitSplit>sequential_3/lstm_6/while/lstm_cell_8/split/split_dim:output:06sequential_3/lstm_6/while/lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2-
+sequential_3/lstm_6/while/lstm_cell_8/split?
+sequential_3/lstm_6/while/lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2-
+sequential_3/lstm_6/while/lstm_cell_8/Const?
-sequential_3/lstm_6/while/lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2/
-sequential_3/lstm_6/while/lstm_cell_8/Const_1?
)sequential_3/lstm_6/while/lstm_cell_8/MulMul4sequential_3/lstm_6/while/lstm_cell_8/split:output:04sequential_3/lstm_6/while/lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22+
)sequential_3/lstm_6/while/lstm_cell_8/Mul?
+sequential_3/lstm_6/while/lstm_cell_8/Add_1AddV2-sequential_3/lstm_6/while/lstm_cell_8/Mul:z:06sequential_3/lstm_6/while/lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_6/while/lstm_cell_8/Add_1?
=sequential_3/lstm_6/while/lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2?
=sequential_3/lstm_6/while/lstm_cell_8/clip_by_value/Minimum/y?
;sequential_3/lstm_6/while/lstm_cell_8/clip_by_value/MinimumMinimum/sequential_3/lstm_6/while/lstm_cell_8/Add_1:z:0Fsequential_3/lstm_6/while/lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22=
;sequential_3/lstm_6/while/lstm_cell_8/clip_by_value/Minimum?
5sequential_3/lstm_6/while/lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    27
5sequential_3/lstm_6/while/lstm_cell_8/clip_by_value/y?
3sequential_3/lstm_6/while/lstm_cell_8/clip_by_valueMaximum?sequential_3/lstm_6/while/lstm_cell_8/clip_by_value/Minimum:z:0>sequential_3/lstm_6/while/lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:225
3sequential_3/lstm_6/while/lstm_cell_8/clip_by_value?
-sequential_3/lstm_6/while/lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2/
-sequential_3/lstm_6/while/lstm_cell_8/Const_2?
-sequential_3/lstm_6/while/lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2/
-sequential_3/lstm_6/while/lstm_cell_8/Const_3?
+sequential_3/lstm_6/while/lstm_cell_8/Mul_1Mul4sequential_3/lstm_6/while/lstm_cell_8/split:output:16sequential_3/lstm_6/while/lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_6/while/lstm_cell_8/Mul_1?
+sequential_3/lstm_6/while/lstm_cell_8/Add_2AddV2/sequential_3/lstm_6/while/lstm_cell_8/Mul_1:z:06sequential_3/lstm_6/while/lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_6/while/lstm_cell_8/Add_2?
?sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2A
?sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum/y?
=sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_1/MinimumMinimum/sequential_3/lstm_6/while/lstm_cell_8/Add_2:z:0Hsequential_3/lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22?
=sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum?
7sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    29
7sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_1/y?
5sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_1MaximumAsequential_3/lstm_6/while/lstm_cell_8/clip_by_value_1/Minimum:z:0@sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:227
5sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_1?
+sequential_3/lstm_6/while/lstm_cell_8/mul_2Mul9sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_1:z:0'sequential_3_lstm_6_while_placeholder_3*
T0*
_output_shapes

:22-
+sequential_3/lstm_6/while/lstm_cell_8/mul_2?
*sequential_3/lstm_6/while/lstm_cell_8/TanhTanh4sequential_3/lstm_6/while/lstm_cell_8/split:output:2*
T0*
_output_shapes

:22,
*sequential_3/lstm_6/while/lstm_cell_8/Tanh?
+sequential_3/lstm_6/while/lstm_cell_8/mul_3Mul7sequential_3/lstm_6/while/lstm_cell_8/clip_by_value:z:0.sequential_3/lstm_6/while/lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_6/while/lstm_cell_8/mul_3?
+sequential_3/lstm_6/while/lstm_cell_8/add_3AddV2/sequential_3/lstm_6/while/lstm_cell_8/mul_2:z:0/sequential_3/lstm_6/while/lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_6/while/lstm_cell_8/add_3?
-sequential_3/lstm_6/while/lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2/
-sequential_3/lstm_6/while/lstm_cell_8/Const_4?
-sequential_3/lstm_6/while/lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2/
-sequential_3/lstm_6/while/lstm_cell_8/Const_5?
+sequential_3/lstm_6/while/lstm_cell_8/Mul_4Mul4sequential_3/lstm_6/while/lstm_cell_8/split:output:36sequential_3/lstm_6/while/lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_6/while/lstm_cell_8/Mul_4?
+sequential_3/lstm_6/while/lstm_cell_8/Add_4AddV2/sequential_3/lstm_6/while/lstm_cell_8/Mul_4:z:06sequential_3/lstm_6/while/lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_6/while/lstm_cell_8/Add_4?
?sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2A
?sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum/y?
=sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_2/MinimumMinimum/sequential_3/lstm_6/while/lstm_cell_8/Add_4:z:0Hsequential_3/lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22?
=sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum?
7sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    29
7sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_2/y?
5sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_2MaximumAsequential_3/lstm_6/while/lstm_cell_8/clip_by_value_2/Minimum:z:0@sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:227
5sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_2?
,sequential_3/lstm_6/while/lstm_cell_8/Tanh_1Tanh/sequential_3/lstm_6/while/lstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22.
,sequential_3/lstm_6/while/lstm_cell_8/Tanh_1?
+sequential_3/lstm_6/while/lstm_cell_8/mul_5Mul9sequential_3/lstm_6/while/lstm_cell_8/clip_by_value_2:z:00sequential_3/lstm_6/while/lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22-
+sequential_3/lstm_6/while/lstm_cell_8/mul_5?
>sequential_3/lstm_6/while/TensorArrayV2Write/TensorListSetItemTensorListSetItem'sequential_3_lstm_6_while_placeholder_1%sequential_3_lstm_6_while_placeholder/sequential_3/lstm_6/while/lstm_cell_8/mul_5:z:0*
_output_shapes
: *
element_dtype02@
>sequential_3/lstm_6/while/TensorArrayV2Write/TensorListSetItem?
sequential_3/lstm_6/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2!
sequential_3/lstm_6/while/add/y?
sequential_3/lstm_6/while/addAddV2%sequential_3_lstm_6_while_placeholder(sequential_3/lstm_6/while/add/y:output:0*
T0*
_output_shapes
: 2
sequential_3/lstm_6/while/add?
!sequential_3/lstm_6/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2#
!sequential_3/lstm_6/while/add_1/y?
sequential_3/lstm_6/while/add_1AddV2@sequential_3_lstm_6_while_sequential_3_lstm_6_while_loop_counter*sequential_3/lstm_6/while/add_1/y:output:0*
T0*
_output_shapes
: 2!
sequential_3/lstm_6/while/add_1?
"sequential_3/lstm_6/while/IdentityIdentity#sequential_3/lstm_6/while/add_1:z:0^sequential_3/lstm_6/while/NoOp*
T0*
_output_shapes
: 2$
"sequential_3/lstm_6/while/Identity?
$sequential_3/lstm_6/while/Identity_1IdentityFsequential_3_lstm_6_while_sequential_3_lstm_6_while_maximum_iterations^sequential_3/lstm_6/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_3/lstm_6/while/Identity_1?
$sequential_3/lstm_6/while/Identity_2Identity!sequential_3/lstm_6/while/add:z:0^sequential_3/lstm_6/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_3/lstm_6/while/Identity_2?
$sequential_3/lstm_6/while/Identity_3IdentityNsequential_3/lstm_6/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^sequential_3/lstm_6/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_3/lstm_6/while/Identity_3?
$sequential_3/lstm_6/while/Identity_4Identity/sequential_3/lstm_6/while/lstm_cell_8/mul_5:z:0^sequential_3/lstm_6/while/NoOp*
T0*
_output_shapes

:22&
$sequential_3/lstm_6/while/Identity_4?
$sequential_3/lstm_6/while/Identity_5Identity/sequential_3/lstm_6/while/lstm_cell_8/add_3:z:0^sequential_3/lstm_6/while/NoOp*
T0*
_output_shapes

:22&
$sequential_3/lstm_6/while/Identity_5?
sequential_3/lstm_6/while/NoOpNoOp=^sequential_3/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp<^sequential_3/lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp>^sequential_3/lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2 
sequential_3/lstm_6/while/NoOp"Q
"sequential_3_lstm_6_while_identity+sequential_3/lstm_6/while/Identity:output:0"U
$sequential_3_lstm_6_while_identity_1-sequential_3/lstm_6/while/Identity_1:output:0"U
$sequential_3_lstm_6_while_identity_2-sequential_3/lstm_6/while/Identity_2:output:0"U
$sequential_3_lstm_6_while_identity_3-sequential_3/lstm_6/while/Identity_3:output:0"U
$sequential_3_lstm_6_while_identity_4-sequential_3/lstm_6/while/Identity_4:output:0"U
$sequential_3_lstm_6_while_identity_5-sequential_3/lstm_6/while/Identity_5:output:0"?
Esequential_3_lstm_6_while_lstm_cell_8_biasadd_readvariableop_resourceGsequential_3_lstm_6_while_lstm_cell_8_biasadd_readvariableop_resource_0"?
Fsequential_3_lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resourceHsequential_3_lstm_6_while_lstm_cell_8_matmul_1_readvariableop_resource_0"?
Dsequential_3_lstm_6_while_lstm_cell_8_matmul_readvariableop_resourceFsequential_3_lstm_6_while_lstm_cell_8_matmul_readvariableop_resource_0"|
;sequential_3_lstm_6_while_sequential_3_lstm_6_strided_slice=sequential_3_lstm_6_while_sequential_3_lstm_6_strided_slice_0"?
ysequential_3_lstm_6_while_tensorarrayv2read_tensorlistgetitem_sequential_3_lstm_6_tensorarrayunstack_tensorlistfromtensor{sequential_3_lstm_6_while_tensorarrayv2read_tensorlistgetitem_sequential_3_lstm_6_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2|
<sequential_3/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp<sequential_3/lstm_6/while/lstm_cell_8/BiasAdd/ReadVariableOp2z
;sequential_3/lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp;sequential_3/lstm_6/while/lstm_cell_8/MatMul/ReadVariableOp2~
=sequential_3/lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp=sequential_3/lstm_6/while/lstm_cell_8/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?G
?

!__inference__traced_restore_49905
file_prefix=
*assignvariableop_lstm_7_lstm_cell_9_kernel:	?I
6assignvariableop_1_lstm_7_lstm_cell_9_recurrent_kernel:	2?9
*assignvariableop_2_lstm_7_lstm_cell_9_bias:	??
,assignvariableop_3_lstm_6_lstm_cell_8_kernel:	2?I
6assignvariableop_4_lstm_6_lstm_cell_8_recurrent_kernel:	2?9
*assignvariableop_5_lstm_6_lstm_cell_8_bias:	?>
,assignvariableop_6_time_distributed_3_kernel:28
*assignvariableop_7_time_distributed_3_bias:4
"assignvariableop_8_lstm_7_variable:26
$assignvariableop_9_lstm_7_variable_1:25
#assignvariableop_10_lstm_6_variable:27
%assignvariableop_11_lstm_6_variable_1:2#
assignvariableop_12_total: #
assignvariableop_13_count: %
assignvariableop_14_total_1: %
assignvariableop_15_count_1: 
identity_17??AssignVariableOp?AssignVariableOp_1?AssignVariableOp_10?AssignVariableOp_11?AssignVariableOp_12?AssignVariableOp_13?AssignVariableOp_14?AssignVariableOp_15?AssignVariableOp_2?AssignVariableOp_3?AssignVariableOp_4?AssignVariableOp_5?AssignVariableOp_6?AssignVariableOp_7?AssignVariableOp_8?AssignVariableOp_9?
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*?
value?B?B&variables/0/.ATTRIBUTES/VARIABLE_VALUEB&variables/1/.ATTRIBUTES/VARIABLE_VALUEB&variables/2/.ATTRIBUTES/VARIABLE_VALUEB&variables/3/.ATTRIBUTES/VARIABLE_VALUEB&variables/4/.ATTRIBUTES/VARIABLE_VALUEB&variables/5/.ATTRIBUTES/VARIABLE_VALUEB&variables/6/.ATTRIBUTES/VARIABLE_VALUEB&variables/7/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
RestoreV2/tensor_names?
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*5
value,B*B B B B B B B B B B B B B B B B B 2
RestoreV2/shape_and_slices?
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*X
_output_shapesF
D:::::::::::::::::*
dtypes
22
	RestoreV2g
IdentityIdentityRestoreV2:tensors:0"/device:CPU:0*
T0*
_output_shapes
:2

Identity?
AssignVariableOpAssignVariableOp*assignvariableop_lstm_7_lstm_cell_9_kernelIdentity:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOpk

Identity_1IdentityRestoreV2:tensors:1"/device:CPU:0*
T0*
_output_shapes
:2

Identity_1?
AssignVariableOp_1AssignVariableOp6assignvariableop_1_lstm_7_lstm_cell_9_recurrent_kernelIdentity_1:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_1k

Identity_2IdentityRestoreV2:tensors:2"/device:CPU:0*
T0*
_output_shapes
:2

Identity_2?
AssignVariableOp_2AssignVariableOp*assignvariableop_2_lstm_7_lstm_cell_9_biasIdentity_2:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_2k

Identity_3IdentityRestoreV2:tensors:3"/device:CPU:0*
T0*
_output_shapes
:2

Identity_3?
AssignVariableOp_3AssignVariableOp,assignvariableop_3_lstm_6_lstm_cell_8_kernelIdentity_3:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_3k

Identity_4IdentityRestoreV2:tensors:4"/device:CPU:0*
T0*
_output_shapes
:2

Identity_4?
AssignVariableOp_4AssignVariableOp6assignvariableop_4_lstm_6_lstm_cell_8_recurrent_kernelIdentity_4:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_4k

Identity_5IdentityRestoreV2:tensors:5"/device:CPU:0*
T0*
_output_shapes
:2

Identity_5?
AssignVariableOp_5AssignVariableOp*assignvariableop_5_lstm_6_lstm_cell_8_biasIdentity_5:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_5k

Identity_6IdentityRestoreV2:tensors:6"/device:CPU:0*
T0*
_output_shapes
:2

Identity_6?
AssignVariableOp_6AssignVariableOp,assignvariableop_6_time_distributed_3_kernelIdentity_6:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_6k

Identity_7IdentityRestoreV2:tensors:7"/device:CPU:0*
T0*
_output_shapes
:2

Identity_7?
AssignVariableOp_7AssignVariableOp*assignvariableop_7_time_distributed_3_biasIdentity_7:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_7k

Identity_8IdentityRestoreV2:tensors:8"/device:CPU:0*
T0*
_output_shapes
:2

Identity_8?
AssignVariableOp_8AssignVariableOp"assignvariableop_8_lstm_7_variableIdentity_8:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_8k

Identity_9IdentityRestoreV2:tensors:9"/device:CPU:0*
T0*
_output_shapes
:2

Identity_9?
AssignVariableOp_9AssignVariableOp$assignvariableop_9_lstm_7_variable_1Identity_9:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_9n
Identity_10IdentityRestoreV2:tensors:10"/device:CPU:0*
T0*
_output_shapes
:2
Identity_10?
AssignVariableOp_10AssignVariableOp#assignvariableop_10_lstm_6_variableIdentity_10:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_10n
Identity_11IdentityRestoreV2:tensors:11"/device:CPU:0*
T0*
_output_shapes
:2
Identity_11?
AssignVariableOp_11AssignVariableOp%assignvariableop_11_lstm_6_variable_1Identity_11:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_11n
Identity_12IdentityRestoreV2:tensors:12"/device:CPU:0*
T0*
_output_shapes
:2
Identity_12?
AssignVariableOp_12AssignVariableOpassignvariableop_12_totalIdentity_12:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_12n
Identity_13IdentityRestoreV2:tensors:13"/device:CPU:0*
T0*
_output_shapes
:2
Identity_13?
AssignVariableOp_13AssignVariableOpassignvariableop_13_countIdentity_13:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_13n
Identity_14IdentityRestoreV2:tensors:14"/device:CPU:0*
T0*
_output_shapes
:2
Identity_14?
AssignVariableOp_14AssignVariableOpassignvariableop_14_total_1Identity_14:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_14n
Identity_15IdentityRestoreV2:tensors:15"/device:CPU:0*
T0*
_output_shapes
:2
Identity_15?
AssignVariableOp_15AssignVariableOpassignvariableop_15_count_1Identity_15:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_159
NoOpNoOp"/device:CPU:0*
_output_shapes
 2
NoOp?
Identity_16Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_2^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9^NoOp"/device:CPU:0*
T0*
_output_shapes
: 2
Identity_16f
Identity_17IdentityIdentity_16:output:0^NoOp_1*
T0*
_output_shapes
: 2
Identity_17?
NoOp_1NoOp^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_2^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9*"
_acd_function_control_output(*
_output_shapes
 2
NoOp_1"#
identity_17Identity_17:output:0*5
_input_shapes$
": : : : : : : : : : : : : : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12*
AssignVariableOp_10AssignVariableOp_102*
AssignVariableOp_11AssignVariableOp_112*
AssignVariableOp_12AssignVariableOp_122*
AssignVariableOp_13AssignVariableOp_132*
AssignVariableOp_14AssignVariableOp_142*
AssignVariableOp_15AssignVariableOp_152(
AssignVariableOp_2AssignVariableOp_22(
AssignVariableOp_3AssignVariableOp_32(
AssignVariableOp_4AssignVariableOp_42(
AssignVariableOp_5AssignVariableOp_52(
AssignVariableOp_6AssignVariableOp_62(
AssignVariableOp_7AssignVariableOp_72(
AssignVariableOp_8AssignVariableOp_82(
AssignVariableOp_9AssignVariableOp_9:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix
?
?
+__inference_lstm_cell_9_layer_call_fn_49355

inputs
states_0
states_1
unknown:	?
	unknown_0:	2?
	unknown_1:	?
identity

identity_1

identity_2??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0states_1unknown	unknown_0	unknown_1*
Tin

2*
Tout
2*
_collective_manager_ids
 * 
_output_shapes
:::*'
_read_only_resource_inputs	
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_493422
StatefulPartitionedCalll
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
:2

Identityp

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*
_output_shapes
:2

Identity_1p

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*
_output_shapes
:2

Identity_2h
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$::2:2: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
?
?
while_cond_45496
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_45496___redundant_placeholder03
/while_while_cond_45496___redundant_placeholder13
/while_while_cond_45496___redundant_placeholder23
/while_while_cond_45496___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?X
?
while_body_45497
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_9_matmul_readvariableop_resource_0:	?G
4while_lstm_cell_9_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_9_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_9_matmul_readvariableop_resource:	?E
2while_lstm_cell_9_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_9_biasadd_readvariableop_resource:	???(while/lstm_cell_9/BiasAdd/ReadVariableOp?'while/lstm_cell_9/MatMul/ReadVariableOp?)while/lstm_cell_9/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_9/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_9_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02)
'while/lstm_cell_9/MatMul/ReadVariableOp?
while/lstm_cell_9/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/MatMul?
)while/lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_9_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_9/MatMul_1/ReadVariableOp?
while/lstm_cell_9/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_9/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/MatMul_1?
while/lstm_cell_9/addAddV2"while/lstm_cell_9/MatMul:product:0$while/lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/add?
(while/lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_9_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_9/BiasAdd/ReadVariableOp?
while/lstm_cell_9/BiasAddBiasAddwhile/lstm_cell_9/add:z:00while/lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/BiasAdd?
!while/lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_9/split/split_dim?
while/lstm_cell_9/splitSplit*while/lstm_cell_9/split/split_dim:output:0"while/lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_9/splitw
while/lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const{
while/lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_1?
while/lstm_cell_9/MulMul while/lstm_cell_9/split:output:0 while/lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul?
while/lstm_cell_9/Add_1AddV2while/lstm_cell_9/Mul:z:0"while/lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_1?
)while/lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_9/clip_by_value/Minimum/y?
'while/lstm_cell_9/clip_by_value/MinimumMinimumwhile/lstm_cell_9/Add_1:z:02while/lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22)
'while/lstm_cell_9/clip_by_value/Minimum?
!while/lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_9/clip_by_value/y?
while/lstm_cell_9/clip_by_valueMaximum+while/lstm_cell_9/clip_by_value/Minimum:z:0*while/lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22!
while/lstm_cell_9/clip_by_value{
while/lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const_2{
while/lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_3?
while/lstm_cell_9/Mul_1Mul while/lstm_cell_9/split:output:1"while/lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul_1?
while/lstm_cell_9/Add_2AddV2while/lstm_cell_9/Mul_1:z:0"while/lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_2?
+while/lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_9/clip_by_value_1/Minimum/y?
)while/lstm_cell_9/clip_by_value_1/MinimumMinimumwhile/lstm_cell_9/Add_2:z:04while/lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_9/clip_by_value_1/Minimum?
#while/lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_9/clip_by_value_1/y?
!while/lstm_cell_9/clip_by_value_1Maximum-while/lstm_cell_9/clip_by_value_1/Minimum:z:0,while/lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_9/clip_by_value_1?
while/lstm_cell_9/mul_2Mul%while/lstm_cell_9/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_2?
while/lstm_cell_9/TanhTanh while/lstm_cell_9/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_9/Tanh?
while/lstm_cell_9/mul_3Mul#while/lstm_cell_9/clip_by_value:z:0while/lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_3?
while/lstm_cell_9/add_3AddV2while/lstm_cell_9/mul_2:z:0while/lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_9/add_3{
while/lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const_4{
while/lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_5?
while/lstm_cell_9/Mul_4Mul while/lstm_cell_9/split:output:3"while/lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul_4?
while/lstm_cell_9/Add_4AddV2while/lstm_cell_9/Mul_4:z:0"while/lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_4?
+while/lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_9/clip_by_value_2/Minimum/y?
)while/lstm_cell_9/clip_by_value_2/MinimumMinimumwhile/lstm_cell_9/Add_4:z:04while/lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_9/clip_by_value_2/Minimum?
#while/lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_9/clip_by_value_2/y?
!while/lstm_cell_9/clip_by_value_2Maximum-while/lstm_cell_9/clip_by_value_2/Minimum:z:0,while/lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_9/clip_by_value_2?
while/lstm_cell_9/Tanh_1Tanhwhile/lstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Tanh_1?
while/lstm_cell_9/mul_5Mul%while/lstm_cell_9/clip_by_value_2:z:0while/lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_9/mul_5:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_9/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_9/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_9/BiasAdd/ReadVariableOp(^while/lstm_cell_9/MatMul/ReadVariableOp*^while/lstm_cell_9/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"h
1while_lstm_cell_9_biasadd_readvariableop_resource3while_lstm_cell_9_biasadd_readvariableop_resource_0"j
2while_lstm_cell_9_matmul_1_readvariableop_resource4while_lstm_cell_9_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_9_matmul_readvariableop_resource2while_lstm_cell_9_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2T
(while/lstm_cell_9/BiasAdd/ReadVariableOp(while/lstm_cell_9/BiasAdd/ReadVariableOp2R
'while/lstm_cell_9/MatMul/ReadVariableOp'while/lstm_cell_9/MatMul/ReadVariableOp2V
)while/lstm_cell_9/MatMul_1/ReadVariableOp)while/lstm_cell_9/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?
?
while_cond_44599
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_44599___redundant_placeholder03
/while_while_cond_44599___redundant_placeholder13
/while_while_cond_44599___redundant_placeholder23
/while_while_cond_44599___redundant_placeholder3
while_identity
n

while/LessLesswhile_placeholderwhile_less_strided_slice*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?e
?

lstm_7_while_body_46954*
&lstm_7_while_lstm_7_while_loop_counter0
,lstm_7_while_lstm_7_while_maximum_iterations
lstm_7_while_placeholder
lstm_7_while_placeholder_1
lstm_7_while_placeholder_2
lstm_7_while_placeholder_3'
#lstm_7_while_lstm_7_strided_slice_0e
alstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor_0L
9lstm_7_while_lstm_cell_9_matmul_readvariableop_resource_0:	?N
;lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource_0:	2?I
:lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource_0:	?
lstm_7_while_identity
lstm_7_while_identity_1
lstm_7_while_identity_2
lstm_7_while_identity_3
lstm_7_while_identity_4
lstm_7_while_identity_5%
!lstm_7_while_lstm_7_strided_slicec
_lstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensorJ
7lstm_7_while_lstm_cell_9_matmul_readvariableop_resource:	?L
9lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource:	2?G
8lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource:	???/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp?.lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp?0lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp?
>lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      2@
>lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shape?
0lstm_7/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemalstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor_0lstm_7_while_placeholderGlstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype022
0lstm_7/while/TensorArrayV2Read/TensorListGetItem?
.lstm_7/while/lstm_cell_9/MatMul/ReadVariableOpReadVariableOp9lstm_7_while_lstm_cell_9_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype020
.lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp?
lstm_7/while/lstm_cell_9/MatMulMatMul7lstm_7/while/TensorArrayV2Read/TensorListGetItem:item:06lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2!
lstm_7/while/lstm_cell_9/MatMul?
0lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp;lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype022
0lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp?
!lstm_7/while/lstm_cell_9/MatMul_1MatMullstm_7_while_placeholder_28lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2#
!lstm_7/while/lstm_cell_9/MatMul_1?
lstm_7/while/lstm_cell_9/addAddV2)lstm_7/while/lstm_cell_9/MatMul:product:0+lstm_7/while/lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
lstm_7/while/lstm_cell_9/add?
/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp:lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype021
/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp?
 lstm_7/while/lstm_cell_9/BiasAddBiasAdd lstm_7/while/lstm_cell_9/add:z:07lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2"
 lstm_7/while/lstm_cell_9/BiasAdd?
(lstm_7/while/lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2*
(lstm_7/while/lstm_cell_9/split/split_dim?
lstm_7/while/lstm_cell_9/splitSplit1lstm_7/while/lstm_cell_9/split/split_dim:output:0)lstm_7/while/lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2 
lstm_7/while/lstm_cell_9/split?
lstm_7/while/lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2 
lstm_7/while/lstm_cell_9/Const?
 lstm_7/while/lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_7/while/lstm_cell_9/Const_1?
lstm_7/while/lstm_cell_9/MulMul'lstm_7/while/lstm_cell_9/split:output:0'lstm_7/while/lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
lstm_7/while/lstm_cell_9/Mul?
lstm_7/while/lstm_cell_9/Add_1AddV2 lstm_7/while/lstm_cell_9/Mul:z:0)lstm_7/while/lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/Add_1?
0lstm_7/while/lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??22
0lstm_7/while/lstm_cell_9/clip_by_value/Minimum/y?
.lstm_7/while/lstm_cell_9/clip_by_value/MinimumMinimum"lstm_7/while/lstm_cell_9/Add_1:z:09lstm_7/while/lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:220
.lstm_7/while/lstm_cell_9/clip_by_value/Minimum?
(lstm_7/while/lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2*
(lstm_7/while/lstm_cell_9/clip_by_value/y?
&lstm_7/while/lstm_cell_9/clip_by_valueMaximum2lstm_7/while/lstm_cell_9/clip_by_value/Minimum:z:01lstm_7/while/lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22(
&lstm_7/while/lstm_cell_9/clip_by_value?
 lstm_7/while/lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_7/while/lstm_cell_9/Const_2?
 lstm_7/while/lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_7/while/lstm_cell_9/Const_3?
lstm_7/while/lstm_cell_9/Mul_1Mul'lstm_7/while/lstm_cell_9/split:output:1)lstm_7/while/lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/Mul_1?
lstm_7/while/lstm_cell_9/Add_2AddV2"lstm_7/while/lstm_cell_9/Mul_1:z:0)lstm_7/while/lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/Add_2?
2lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum/y?
0lstm_7/while/lstm_cell_9/clip_by_value_1/MinimumMinimum"lstm_7/while/lstm_cell_9/Add_2:z:0;lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222
0lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum?
*lstm_7/while/lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_7/while/lstm_cell_9/clip_by_value_1/y?
(lstm_7/while/lstm_cell_9/clip_by_value_1Maximum4lstm_7/while/lstm_cell_9/clip_by_value_1/Minimum:z:03lstm_7/while/lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22*
(lstm_7/while/lstm_cell_9/clip_by_value_1?
lstm_7/while/lstm_cell_9/mul_2Mul,lstm_7/while/lstm_cell_9/clip_by_value_1:z:0lstm_7_while_placeholder_3*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/mul_2?
lstm_7/while/lstm_cell_9/TanhTanh'lstm_7/while/lstm_cell_9/split:output:2*
T0*
_output_shapes

:22
lstm_7/while/lstm_cell_9/Tanh?
lstm_7/while/lstm_cell_9/mul_3Mul*lstm_7/while/lstm_cell_9/clip_by_value:z:0!lstm_7/while/lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/mul_3?
lstm_7/while/lstm_cell_9/add_3AddV2"lstm_7/while/lstm_cell_9/mul_2:z:0"lstm_7/while/lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/add_3?
 lstm_7/while/lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_7/while/lstm_cell_9/Const_4?
 lstm_7/while/lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_7/while/lstm_cell_9/Const_5?
lstm_7/while/lstm_cell_9/Mul_4Mul'lstm_7/while/lstm_cell_9/split:output:3)lstm_7/while/lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/Mul_4?
lstm_7/while/lstm_cell_9/Add_4AddV2"lstm_7/while/lstm_cell_9/Mul_4:z:0)lstm_7/while/lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/Add_4?
2lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum/y?
0lstm_7/while/lstm_cell_9/clip_by_value_2/MinimumMinimum"lstm_7/while/lstm_cell_9/Add_4:z:0;lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222
0lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum?
*lstm_7/while/lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_7/while/lstm_cell_9/clip_by_value_2/y?
(lstm_7/while/lstm_cell_9/clip_by_value_2Maximum4lstm_7/while/lstm_cell_9/clip_by_value_2/Minimum:z:03lstm_7/while/lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22*
(lstm_7/while/lstm_cell_9/clip_by_value_2?
lstm_7/while/lstm_cell_9/Tanh_1Tanh"lstm_7/while/lstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22!
lstm_7/while/lstm_cell_9/Tanh_1?
lstm_7/while/lstm_cell_9/mul_5Mul,lstm_7/while/lstm_cell_9/clip_by_value_2:z:0#lstm_7/while/lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22 
lstm_7/while/lstm_cell_9/mul_5?
1lstm_7/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_7_while_placeholder_1lstm_7_while_placeholder"lstm_7/while/lstm_cell_9/mul_5:z:0*
_output_shapes
: *
element_dtype023
1lstm_7/while/TensorArrayV2Write/TensorListSetItemj
lstm_7/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/while/add/y?
lstm_7/while/addAddV2lstm_7_while_placeholderlstm_7/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_7/while/addn
lstm_7/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/while/add_1/y?
lstm_7/while/add_1AddV2&lstm_7_while_lstm_7_while_loop_counterlstm_7/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_7/while/add_1?
lstm_7/while/IdentityIdentitylstm_7/while/add_1:z:0^lstm_7/while/NoOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity?
lstm_7/while/Identity_1Identity,lstm_7_while_lstm_7_while_maximum_iterations^lstm_7/while/NoOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity_1?
lstm_7/while/Identity_2Identitylstm_7/while/add:z:0^lstm_7/while/NoOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity_2?
lstm_7/while/Identity_3IdentityAlstm_7/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_7/while/NoOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity_3?
lstm_7/while/Identity_4Identity"lstm_7/while/lstm_cell_9/mul_5:z:0^lstm_7/while/NoOp*
T0*
_output_shapes

:22
lstm_7/while/Identity_4?
lstm_7/while/Identity_5Identity"lstm_7/while/lstm_cell_9/add_3:z:0^lstm_7/while/NoOp*
T0*
_output_shapes

:22
lstm_7/while/Identity_5?
lstm_7/while/NoOpNoOp0^lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp/^lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp1^lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_7/while/NoOp"7
lstm_7_while_identitylstm_7/while/Identity:output:0";
lstm_7_while_identity_1 lstm_7/while/Identity_1:output:0";
lstm_7_while_identity_2 lstm_7/while/Identity_2:output:0";
lstm_7_while_identity_3 lstm_7/while/Identity_3:output:0";
lstm_7_while_identity_4 lstm_7/while/Identity_4:output:0";
lstm_7_while_identity_5 lstm_7/while/Identity_5:output:0"H
!lstm_7_while_lstm_7_strided_slice#lstm_7_while_lstm_7_strided_slice_0"v
8lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource:lstm_7_while_lstm_cell_9_biasadd_readvariableop_resource_0"x
9lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource;lstm_7_while_lstm_cell_9_matmul_1_readvariableop_resource_0"t
7lstm_7_while_lstm_cell_9_matmul_readvariableop_resource9lstm_7_while_lstm_cell_9_matmul_readvariableop_resource_0"?
_lstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensoralstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2b
/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp/lstm_7/while/lstm_cell_9/BiasAdd/ReadVariableOp2`
.lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp.lstm_7/while/lstm_cell_9/MatMul/ReadVariableOp2d
0lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp0lstm_7/while/lstm_cell_9/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?

?
B__inference_dense_3_layer_call_and_return_conditional_losses_45303

inputs0
matmul_readvariableop_resource:2-
biasadd_readvariableop_resource:
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:2*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2	
BiasAddk
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:?????????2

Identity
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:?????????2: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????2
 
_user_specified_nameinputs
?X
?
while_body_48682
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_8_matmul_readvariableop_resource_0:	2?G
4while_lstm_cell_8_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_8_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_8_matmul_readvariableop_resource:	2?E
2while_lstm_cell_8_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_8_biasadd_readvariableop_resource:	???(while/lstm_cell_8/BiasAdd/ReadVariableOp?'while/lstm_cell_8/MatMul/ReadVariableOp?)while/lstm_cell_8/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_8/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_8_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02)
'while/lstm_cell_8/MatMul/ReadVariableOp?
while/lstm_cell_8/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/MatMul?
)while/lstm_cell_8/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_8_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_8/MatMul_1/ReadVariableOp?
while/lstm_cell_8/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_8/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/MatMul_1?
while/lstm_cell_8/addAddV2"while/lstm_cell_8/MatMul:product:0$while/lstm_cell_8/MatMul_1:product:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/add?
(while/lstm_cell_8/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_8_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_8/BiasAdd/ReadVariableOp?
while/lstm_cell_8/BiasAddBiasAddwhile/lstm_cell_8/add:z:00while/lstm_cell_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_8/BiasAdd?
!while/lstm_cell_8/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_8/split/split_dim?
while/lstm_cell_8/splitSplit*while/lstm_cell_8/split/split_dim:output:0"while/lstm_cell_8/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_8/splitw
while/lstm_cell_8/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const{
while/lstm_cell_8/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_1?
while/lstm_cell_8/MulMul while/lstm_cell_8/split:output:0 while/lstm_cell_8/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul?
while/lstm_cell_8/Add_1AddV2while/lstm_cell_8/Mul:z:0"while/lstm_cell_8/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_1?
)while/lstm_cell_8/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_8/clip_by_value/Minimum/y?
'while/lstm_cell_8/clip_by_value/MinimumMinimumwhile/lstm_cell_8/Add_1:z:02while/lstm_cell_8/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22)
'while/lstm_cell_8/clip_by_value/Minimum?
!while/lstm_cell_8/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_8/clip_by_value/y?
while/lstm_cell_8/clip_by_valueMaximum+while/lstm_cell_8/clip_by_value/Minimum:z:0*while/lstm_cell_8/clip_by_value/y:output:0*
T0*
_output_shapes

:22!
while/lstm_cell_8/clip_by_value{
while/lstm_cell_8/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const_2{
while/lstm_cell_8/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_3?
while/lstm_cell_8/Mul_1Mul while/lstm_cell_8/split:output:1"while/lstm_cell_8/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul_1?
while/lstm_cell_8/Add_2AddV2while/lstm_cell_8/Mul_1:z:0"while/lstm_cell_8/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_2?
+while/lstm_cell_8/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_8/clip_by_value_1/Minimum/y?
)while/lstm_cell_8/clip_by_value_1/MinimumMinimumwhile/lstm_cell_8/Add_2:z:04while/lstm_cell_8/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_8/clip_by_value_1/Minimum?
#while/lstm_cell_8/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_8/clip_by_value_1/y?
!while/lstm_cell_8/clip_by_value_1Maximum-while/lstm_cell_8/clip_by_value_1/Minimum:z:0,while/lstm_cell_8/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_8/clip_by_value_1?
while/lstm_cell_8/mul_2Mul%while/lstm_cell_8/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_2?
while/lstm_cell_8/TanhTanh while/lstm_cell_8/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_8/Tanh?
while/lstm_cell_8/mul_3Mul#while/lstm_cell_8/clip_by_value:z:0while/lstm_cell_8/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_3?
while/lstm_cell_8/add_3AddV2while/lstm_cell_8/mul_2:z:0while/lstm_cell_8/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_8/add_3{
while/lstm_cell_8/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_8/Const_4{
while/lstm_cell_8/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_8/Const_5?
while/lstm_cell_8/Mul_4Mul while/lstm_cell_8/split:output:3"while/lstm_cell_8/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Mul_4?
while/lstm_cell_8/Add_4AddV2while/lstm_cell_8/Mul_4:z:0"while/lstm_cell_8/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Add_4?
+while/lstm_cell_8/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_8/clip_by_value_2/Minimum/y?
)while/lstm_cell_8/clip_by_value_2/MinimumMinimumwhile/lstm_cell_8/Add_4:z:04while/lstm_cell_8/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_8/clip_by_value_2/Minimum?
#while/lstm_cell_8/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_8/clip_by_value_2/y?
!while/lstm_cell_8/clip_by_value_2Maximum-while/lstm_cell_8/clip_by_value_2/Minimum:z:0,while/lstm_cell_8/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_8/clip_by_value_2?
while/lstm_cell_8/Tanh_1Tanhwhile/lstm_cell_8/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_8/Tanh_1?
while/lstm_cell_8/mul_5Mul%while/lstm_cell_8/clip_by_value_2:z:0while/lstm_cell_8/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_8/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_8/mul_5:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_8/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_8/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_8/BiasAdd/ReadVariableOp(^while/lstm_cell_8/MatMul/ReadVariableOp*^while/lstm_cell_8/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"h
1while_lstm_cell_8_biasadd_readvariableop_resource3while_lstm_cell_8_biasadd_readvariableop_resource_0"j
2while_lstm_cell_8_matmul_1_readvariableop_resource4while_lstm_cell_8_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_8_matmul_readvariableop_resource2while_lstm_cell_8_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2T
(while/lstm_cell_8/BiasAdd/ReadVariableOp(while/lstm_cell_8/BiasAdd/ReadVariableOp2R
'while/lstm_cell_8/MatMul/ReadVariableOp'while/lstm_cell_8/MatMul/ReadVariableOp2V
)while/lstm_cell_8/MatMul_1/ReadVariableOp)while/lstm_cell_8/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?
?
2__inference_time_distributed_3_layer_call_fn_48953

inputs
unknown:2
	unknown_0:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_458772
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:
2: : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
?X
?
while_body_47910
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_9_matmul_readvariableop_resource_0:	?G
4while_lstm_cell_9_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_9_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_9_matmul_readvariableop_resource:	?E
2while_lstm_cell_9_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_9_biasadd_readvariableop_resource:	???(while/lstm_cell_9/BiasAdd/ReadVariableOp?'while/lstm_cell_9/MatMul/ReadVariableOp?)while/lstm_cell_9/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_9/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_9_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02)
'while/lstm_cell_9/MatMul/ReadVariableOp?
while/lstm_cell_9/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_9/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/MatMul?
)while/lstm_cell_9/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_9_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_9/MatMul_1/ReadVariableOp?
while/lstm_cell_9/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_9/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/MatMul_1?
while/lstm_cell_9/addAddV2"while/lstm_cell_9/MatMul:product:0$while/lstm_cell_9/MatMul_1:product:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/add?
(while/lstm_cell_9/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_9_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_9/BiasAdd/ReadVariableOp?
while/lstm_cell_9/BiasAddBiasAddwhile/lstm_cell_9/add:z:00while/lstm_cell_9/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
while/lstm_cell_9/BiasAdd?
!while/lstm_cell_9/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_9/split/split_dim?
while/lstm_cell_9/splitSplit*while/lstm_cell_9/split/split_dim:output:0"while/lstm_cell_9/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_9/splitw
while/lstm_cell_9/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const{
while/lstm_cell_9/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_1?
while/lstm_cell_9/MulMul while/lstm_cell_9/split:output:0 while/lstm_cell_9/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul?
while/lstm_cell_9/Add_1AddV2while/lstm_cell_9/Mul:z:0"while/lstm_cell_9/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_1?
)while/lstm_cell_9/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_9/clip_by_value/Minimum/y?
'while/lstm_cell_9/clip_by_value/MinimumMinimumwhile/lstm_cell_9/Add_1:z:02while/lstm_cell_9/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22)
'while/lstm_cell_9/clip_by_value/Minimum?
!while/lstm_cell_9/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_9/clip_by_value/y?
while/lstm_cell_9/clip_by_valueMaximum+while/lstm_cell_9/clip_by_value/Minimum:z:0*while/lstm_cell_9/clip_by_value/y:output:0*
T0*
_output_shapes

:22!
while/lstm_cell_9/clip_by_value{
while/lstm_cell_9/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const_2{
while/lstm_cell_9/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_3?
while/lstm_cell_9/Mul_1Mul while/lstm_cell_9/split:output:1"while/lstm_cell_9/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul_1?
while/lstm_cell_9/Add_2AddV2while/lstm_cell_9/Mul_1:z:0"while/lstm_cell_9/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_2?
+while/lstm_cell_9/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_9/clip_by_value_1/Minimum/y?
)while/lstm_cell_9/clip_by_value_1/MinimumMinimumwhile/lstm_cell_9/Add_2:z:04while/lstm_cell_9/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_9/clip_by_value_1/Minimum?
#while/lstm_cell_9/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_9/clip_by_value_1/y?
!while/lstm_cell_9/clip_by_value_1Maximum-while/lstm_cell_9/clip_by_value_1/Minimum:z:0,while/lstm_cell_9/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_9/clip_by_value_1?
while/lstm_cell_9/mul_2Mul%while/lstm_cell_9/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_2?
while/lstm_cell_9/TanhTanh while/lstm_cell_9/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_9/Tanh?
while/lstm_cell_9/mul_3Mul#while/lstm_cell_9/clip_by_value:z:0while/lstm_cell_9/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_3?
while/lstm_cell_9/add_3AddV2while/lstm_cell_9/mul_2:z:0while/lstm_cell_9/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_9/add_3{
while/lstm_cell_9/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_9/Const_4{
while/lstm_cell_9/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_9/Const_5?
while/lstm_cell_9/Mul_4Mul while/lstm_cell_9/split:output:3"while/lstm_cell_9/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Mul_4?
while/lstm_cell_9/Add_4AddV2while/lstm_cell_9/Mul_4:z:0"while/lstm_cell_9/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Add_4?
+while/lstm_cell_9/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_9/clip_by_value_2/Minimum/y?
)while/lstm_cell_9/clip_by_value_2/MinimumMinimumwhile/lstm_cell_9/Add_4:z:04while/lstm_cell_9/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22+
)while/lstm_cell_9/clip_by_value_2/Minimum?
#while/lstm_cell_9/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_9/clip_by_value_2/y?
!while/lstm_cell_9/clip_by_value_2Maximum-while/lstm_cell_9/clip_by_value_2/Minimum:z:0,while/lstm_cell_9/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22#
!while/lstm_cell_9/clip_by_value_2?
while/lstm_cell_9/Tanh_1Tanhwhile/lstm_cell_9/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_9/Tanh_1?
while/lstm_cell_9/mul_5Mul%while/lstm_cell_9/clip_by_value_2:z:0while/lstm_cell_9/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_9/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_9/mul_5:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1k
while/IdentityIdentitywhile/add_1:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity~
while/Identity_1Identitywhile_while_maximum_iterations^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_1m
while/Identity_2Identitywhile/add:z:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_9/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_9/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_9/BiasAdd/ReadVariableOp(^while/lstm_cell_9/MatMul/ReadVariableOp*^while/lstm_cell_9/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"h
1while_lstm_cell_9_biasadd_readvariableop_resource3while_lstm_cell_9_biasadd_readvariableop_resource_0"j
2while_lstm_cell_9_matmul_1_readvariableop_resource4while_lstm_cell_9_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_9_matmul_readvariableop_resource2while_lstm_cell_9_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2T
(while/lstm_cell_9/BiasAdd/ReadVariableOp(while/lstm_cell_9/BiasAdd/ReadVariableOp2R
'while/lstm_cell_9/MatMul/ReadVariableOp'while/lstm_cell_9/MatMul/ReadVariableOp2V
)while/lstm_cell_9/MatMul_1/ReadVariableOp)while/lstm_cell_9/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
: 
?
?
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_45877

inputs8
&dense_3_matmul_readvariableop_resource:25
'dense_3_biasadd_readvariableop_resource:
identity??dense_3/BiasAdd/ReadVariableOp?dense_3/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2
Reshape/shapef
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes

:
22	
Reshape?
dense_3/MatMul/ReadVariableOpReadVariableOp&dense_3_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_3/MatMul/ReadVariableOp?
dense_3/MatMulMatMulReshape:output:0%dense_3/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_3/MatMul?
dense_3/BiasAdd/ReadVariableOpReadVariableOp'dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_3/BiasAdd/ReadVariableOp?
dense_3/BiasAddBiasAdddense_3/MatMul:product:0&dense_3/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_3/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      2
Reshape_1/shape?
	Reshape_1Reshapedense_3/BiasAdd:output:0Reshape_1/shape:output:0*
T0*"
_output_shapes
:
2
	Reshape_1h
IdentityIdentityReshape_1:output:0^NoOp*
T0*"
_output_shapes
:
2

Identity?
NoOpNoOp^dense_3/BiasAdd/ReadVariableOp^dense_3/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:
2: : 2@
dense_3/BiasAdd/ReadVariableOpdense_3/BiasAdd/ReadVariableOp2>
dense_3/MatMul/ReadVariableOpdense_3/MatMul/ReadVariableOp:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
?
?
+__inference_lstm_cell_8_layer_call_fn_49757

inputs
states_0
states_1
unknown:	2?
	unknown_0:	2?
	unknown_1:	?
identity

identity_1

identity_2??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0states_1unknown	unknown_0	unknown_1*
Tin

2*
Tout
2*
_collective_manager_ids
 * 
_output_shapes
:::*'
_read_only_resource_inputs	
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_497442
StatefulPartitionedCalll
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes
:2

Identityp

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*
_output_shapes
:2

Identity_1p

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*
_output_shapes
:2

Identity_2h
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:2:2:2: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:2
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
?	
?
lstm_6_while_cond_46763*
&lstm_6_while_lstm_6_while_loop_counter0
,lstm_6_while_lstm_6_while_maximum_iterations
lstm_6_while_placeholder
lstm_6_while_placeholder_1
lstm_6_while_placeholder_2
lstm_6_while_placeholder_3*
&lstm_6_while_less_lstm_6_strided_sliceA
=lstm_6_while_lstm_6_while_cond_46763___redundant_placeholder0A
=lstm_6_while_lstm_6_while_cond_46763___redundant_placeholder1A
=lstm_6_while_lstm_6_while_cond_46763___redundant_placeholder2A
=lstm_6_while_lstm_6_while_cond_46763___redundant_placeholder3
lstm_6_while_identity
?
lstm_6/while/LessLesslstm_6_while_placeholder&lstm_6_while_less_lstm_6_strided_slice*
T0*
_output_shapes
: 2
lstm_6/while/Lessr
lstm_6/while/IdentityIdentitylstm_6/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_6/while/Identity"7
lstm_6_while_identitylstm_6/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :2:2: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:2:$ 

_output_shapes

:2:

_output_shapes
: :

_output_shapes
:
?
?
,__inference_sequential_3_layer_call_fn_47303

inputs
unknown:	?
	unknown_0:2
	unknown_1:	2?
	unknown_2:	?
	unknown_3:2
	unknown_4:	2?
	unknown_5:2
	unknown_6:	2?
	unknown_7:	?
	unknown_8:2
	unknown_9:2

unknown_10:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_sequential_3_layer_call_and_return_conditional_losses_463642
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:

 
_user_specified_nameinputs
?
?
G__inference_sequential_3_layer_call_and_return_conditional_losses_46453
lstm_7_input
lstm_7_46423:	?
lstm_7_46425:2
lstm_7_46427:	2?
lstm_7_46429:	?
lstm_7_46431:2
lstm_6_46434:	2?
lstm_6_46436:2
lstm_6_46438:	2?
lstm_6_46440:	?
lstm_6_46442:2*
time_distributed_3_46445:2&
time_distributed_3_46447:
identity??lstm_6/StatefulPartitionedCall?lstm_7/StatefulPartitionedCall?*time_distributed_3/StatefulPartitionedCall?
lstm_7/StatefulPartitionedCallStatefulPartitionedCalllstm_7_inputlstm_7_46423lstm_7_46425lstm_7_46427lstm_7_46429lstm_7_46431*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_7_layer_call_and_return_conditional_losses_456022 
lstm_7/StatefulPartitionedCall?
lstm_6/StatefulPartitionedCallStatefulPartitionedCall'lstm_7/StatefulPartitionedCall:output:0lstm_6_46434lstm_6_46436lstm_6_46438lstm_6_46440lstm_6_46442*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_6_layer_call_and_return_conditional_losses_457912 
lstm_6/StatefulPartitionedCall?
*time_distributed_3/StatefulPartitionedCallStatefulPartitionedCall'lstm_6/StatefulPartitionedCall:output:0time_distributed_3_46445time_distributed_3_46447*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_458162,
*time_distributed_3/StatefulPartitionedCall?
 time_distributed_3/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2"
 time_distributed_3/Reshape/shape?
time_distributed_3/ReshapeReshape'lstm_6/StatefulPartitionedCall:output:0)time_distributed_3/Reshape/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_3/Reshape?
IdentityIdentity3time_distributed_3/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

Identity?
NoOpNoOp^lstm_6/StatefulPartitionedCall^lstm_7/StatefulPartitionedCall+^time_distributed_3/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 2@
lstm_6/StatefulPartitionedCalllstm_6/StatefulPartitionedCall2@
lstm_7/StatefulPartitionedCalllstm_7/StatefulPartitionedCall2X
*time_distributed_3/StatefulPartitionedCall*time_distributed_3/StatefulPartitionedCall:P L
"
_output_shapes
:

&
_user_specified_namelstm_7_input
?
?
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_48868

inputs8
&dense_3_matmul_readvariableop_resource:25
'dense_3_biasadd_readvariableop_resource:
identity??dense_3/BiasAdd/ReadVariableOp?dense_3/MatMul/ReadVariableOpD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_sliceo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2
Reshape/shapeo
ReshapeReshapeinputsReshape/shape:output:0*
T0*'
_output_shapes
:?????????22	
Reshape?
dense_3/MatMul/ReadVariableOpReadVariableOp&dense_3_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_3/MatMul/ReadVariableOp?
dense_3/MatMulMatMulReshape:output:0%dense_3/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_3/MatMul?
dense_3/BiasAdd/ReadVariableOpReadVariableOp'dense_3_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_3/BiasAdd/ReadVariableOp?
dense_3/BiasAddBiasAdddense_3/MatMul:product:0&dense_3/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_3/BiasAddq
Reshape_1/shape/0Const*
_output_shapes
: *
dtype0*
valueB :
?????????2
Reshape_1/shape/0h
Reshape_1/shape/2Const*
_output_shapes
: *
dtype0*
value	B :2
Reshape_1/shape/2?
Reshape_1/shapePackReshape_1/shape/0:output:0strided_slice:output:0Reshape_1/shape/2:output:0*
N*
T0*
_output_shapes
:2
Reshape_1/shape?
	Reshape_1Reshapedense_3/BiasAdd:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :??????????????????2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :??????????????????2

Identity?
NoOpNoOp^dense_3/BiasAdd/ReadVariableOp^dense_3/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:??????????????????2: : 2@
dense_3/BiasAdd/ReadVariableOpdense_3/BiasAdd/ReadVariableOp2>
dense_3/MatMul/ReadVariableOpdense_3/MatMul/ReadVariableOp:\ X
4
_output_shapes"
 :??????????????????2
 
_user_specified_nameinputs"?L
saver_filename:0StatefulPartitionedCall_1:0StatefulPartitionedCall_28"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*?
serving_default?
@
lstm_7_input0
serving_default_lstm_7_input:0
A
time_distributed_3+
StatefulPartitionedCall:0
tensorflow/serving/predict:??
?
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
layer_with_weights-2
layer-2
	optimizer
	variables
trainable_variables
regularization_losses
	keras_api
	
signatures
*e&call_and_return_all_conditional_losses
f_default_save_signature
g__call__"
_tf_keras_sequential
?

cell

state_spec
	variables
trainable_variables
regularization_losses
	keras_api
*h&call_and_return_all_conditional_losses
i__call__"
_tf_keras_rnn_layer
?
cell

state_spec
	variables
trainable_variables
regularization_losses
	keras_api
*j&call_and_return_all_conditional_losses
k__call__"
_tf_keras_rnn_layer
?
	layer
	variables
trainable_variables
regularization_losses
	keras_api
*l&call_and_return_all_conditional_losses
m__call__"
_tf_keras_layer
"
	optimizer
X
0
1
2
3
4
 5
!6
"7"
trackable_list_wrapper
X
0
1
2
3
4
 5
!6
"7"
trackable_list_wrapper
 "
trackable_list_wrapper
?
#layer_regularization_losses
	variables
$non_trainable_variables
trainable_variables

%layers
&layer_metrics
regularization_losses
'metrics
g__call__
f_default_save_signature
*e&call_and_return_all_conditional_losses
&e"call_and_return_conditional_losses"
_generic_user_object
,
nserving_default"
signature_map
?
(
state_size

kernel
recurrent_kernel
bias
)	variables
*trainable_variables
+regularization_losses
,	keras_api
*o&call_and_return_all_conditional_losses
p__call__"
_tf_keras_layer
 "
trackable_list_wrapper
5
0
1
2"
trackable_list_wrapper
5
0
1
2"
trackable_list_wrapper
 "
trackable_list_wrapper
?
-layer_regularization_losses
	variables
.non_trainable_variables
trainable_variables

/layers
0layer_metrics

1states
regularization_losses
2metrics
i__call__
*h&call_and_return_all_conditional_losses
&h"call_and_return_conditional_losses"
_generic_user_object
?
3
state_size

kernel
recurrent_kernel
 bias
4	variables
5trainable_variables
6regularization_losses
7	keras_api
*q&call_and_return_all_conditional_losses
r__call__"
_tf_keras_layer
 "
trackable_list_wrapper
5
0
1
 2"
trackable_list_wrapper
5
0
1
 2"
trackable_list_wrapper
 "
trackable_list_wrapper
?
8layer_regularization_losses
	variables
9non_trainable_variables
trainable_variables

:layers
;layer_metrics

<states
regularization_losses
=metrics
k__call__
*j&call_and_return_all_conditional_losses
&j"call_and_return_conditional_losses"
_generic_user_object
?

!kernel
"bias
>	variables
?trainable_variables
@regularization_losses
A	keras_api
*s&call_and_return_all_conditional_losses
t__call__"
_tf_keras_layer
.
!0
"1"
trackable_list_wrapper
.
!0
"1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
Blayer_regularization_losses
Cnon_trainable_variables
	variables
trainable_variables

Dlayers
Elayer_metrics
regularization_losses
Fmetrics
m__call__
*l&call_and_return_all_conditional_losses
&l"call_and_return_conditional_losses"
_generic_user_object
,:*	?2lstm_7/lstm_cell_9/kernel
6:4	2?2#lstm_7/lstm_cell_9/recurrent_kernel
&:$?2lstm_7/lstm_cell_9/bias
,:*	2?2lstm_6/lstm_cell_8/kernel
6:4	2?2#lstm_6/lstm_cell_8/recurrent_kernel
&:$?2lstm_6/lstm_cell_8/bias
+:)22time_distributed_3/kernel
%:#2time_distributed_3/bias
 "
trackable_list_wrapper
 "
trackable_list_wrapper
5
0
1
2"
trackable_list_wrapper
 "
trackable_dict_wrapper
.
G0
H1"
trackable_list_wrapper
 "
trackable_list_wrapper
5
0
1
2"
trackable_list_wrapper
5
0
1
2"
trackable_list_wrapper
 "
trackable_list_wrapper
?
Ilayer_regularization_losses
Jnon_trainable_variables
)	variables
*trainable_variables

Klayers
Llayer_metrics
+regularization_losses
Mmetrics
p__call__
*o&call_and_return_all_conditional_losses
&o"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
'

0"
trackable_list_wrapper
 "
trackable_dict_wrapper
.
N0
O1"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
5
0
1
 2"
trackable_list_wrapper
5
0
1
 2"
trackable_list_wrapper
 "
trackable_list_wrapper
?
Player_regularization_losses
Qnon_trainable_variables
4	variables
5trainable_variables

Rlayers
Slayer_metrics
6regularization_losses
Tmetrics
r__call__
*q&call_and_return_all_conditional_losses
&q"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
'
0"
trackable_list_wrapper
 "
trackable_dict_wrapper
.
U0
V1"
trackable_list_wrapper
 "
trackable_list_wrapper
.
!0
"1"
trackable_list_wrapper
.
!0
"1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
Wlayer_regularization_losses
Xnon_trainable_variables
>	variables
?trainable_variables

Ylayers
Zlayer_metrics
@regularization_losses
[metrics
t__call__
*s&call_and_return_all_conditional_losses
&s"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_list_wrapper
'
0"
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
N
	\total
	]count
^	variables
_	keras_api"
_tf_keras_metric
^
	`total
	acount
b
_fn_kwargs
c	variables
d	keras_api"
_tf_keras_metric
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
!:22lstm_7/Variable
!:22lstm_7/Variable
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
!:22lstm_6/Variable
!:22lstm_6/Variable
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
:  (2total
:  (2count
.
\0
]1"
trackable_list_wrapper
-
^	variables"
_generic_user_object
:  (2total
:  (2count
 "
trackable_dict_wrapper
.
`0
a1"
trackable_list_wrapper
-
c	variables"
_generic_user_object
?2?
G__inference_sequential_3_layer_call_and_return_conditional_losses_46881
G__inference_sequential_3_layer_call_and_return_conditional_losses_47245
G__inference_sequential_3_layer_call_and_return_conditional_losses_46453
G__inference_sequential_3_layer_call_and_return_conditional_losses_46486?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?B?
 __inference__wrapped_model_43727lstm_7_input"?
???
FullArgSpec
args? 
varargsjargs
varkwjkwargs
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
,__inference_sequential_3_layer_call_fn_45852
,__inference_sequential_3_layer_call_fn_47274
,__inference_sequential_3_layer_call_fn_47303
,__inference_sequential_3_layer_call_fn_46420?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
A__inference_lstm_7_layer_call_and_return_conditional_losses_47481
A__inference_lstm_7_layer_call_and_return_conditional_losses_47659
A__inference_lstm_7_layer_call_and_return_conditional_losses_47837
A__inference_lstm_7_layer_call_and_return_conditional_losses_48015?
???
FullArgSpecB
args:?7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults?

 
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
&__inference_lstm_7_layer_call_fn_48030
&__inference_lstm_7_layer_call_fn_48045
&__inference_lstm_7_layer_call_fn_48060
&__inference_lstm_7_layer_call_fn_48075?
???
FullArgSpecB
args:?7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults?

 
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
A__inference_lstm_6_layer_call_and_return_conditional_losses_48253
A__inference_lstm_6_layer_call_and_return_conditional_losses_48431
A__inference_lstm_6_layer_call_and_return_conditional_losses_48609
A__inference_lstm_6_layer_call_and_return_conditional_losses_48787?
???
FullArgSpecB
args:?7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults?

 
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
&__inference_lstm_6_layer_call_fn_48802
&__inference_lstm_6_layer_call_fn_48817
&__inference_lstm_6_layer_call_fn_48832
&__inference_lstm_6_layer_call_fn_48847?
???
FullArgSpecB
args:?7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults?

 
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_48868
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_48889
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_48903
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_48917?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
2__inference_time_distributed_3_layer_call_fn_48926
2__inference_time_distributed_3_layer_call_fn_48935
2__inference_time_distributed_3_layer_call_fn_48944
2__inference_time_distributed_3_layer_call_fn_48953?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?B?
#__inference_signature_wrapper_46517lstm_7_input"?
???
FullArgSpec
args? 
varargs
 
varkwjkwargs
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49010
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49063
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49116
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49173?
???
FullArgSpec3
args+?(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
+__inference_lstm_cell_9_layer_call_fn_49190
+__inference_lstm_cell_9_layer_call_fn_49207
+__inference_lstm_cell_9_layer_call_fn_49281
+__inference_lstm_cell_9_layer_call_fn_49355?
???
FullArgSpec3
args+?(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49412
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49465
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49518
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49575?
???
FullArgSpec3
args+?(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
+__inference_lstm_cell_8_layer_call_fn_49592
+__inference_lstm_cell_8_layer_call_fn_49609
+__inference_lstm_cell_8_layer_call_fn_49683
+__inference_lstm_cell_8_layer_call_fn_49757?
???
FullArgSpec3
args+?(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
B__inference_dense_3_layer_call_and_return_conditional_losses_49767?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
'__inference_dense_3_layer_call_fn_49776?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 ?
 __inference__wrapped_model_43727?NOU V!"0?-
&?#
!?
lstm_7_input

? "B??
=
time_distributed_3'?$
time_distributed_3
?
B__inference_dense_3_layer_call_and_return_conditional_losses_49767\!"/?,
%?"
 ?
inputs?????????2
? "%?"
?
0?????????
? z
'__inference_dense_3_layer_call_fn_49776O!"/?,
%?"
 ?
inputs?????????2
? "???????????
A__inference_lstm_6_layer_call_and_return_conditional_losses_48253zU VF?C
<?9
+?(
&?#
inputs/0?????????2

 
p 

 
? ")?&
?
0?????????2
? ?
A__inference_lstm_6_layer_call_and_return_conditional_losses_48431zU VF?C
<?9
+?(
&?#
inputs/0?????????2

 
p

 
? ")?&
?
0?????????2
? ?
A__inference_lstm_6_layer_call_and_return_conditional_losses_48609aU V6?3
,?)
?
inputs
2

 
p 

 
? " ?
?
0
2
? ?
A__inference_lstm_6_layer_call_and_return_conditional_losses_48787aU V6?3
,?)
?
inputs
2

 
p

 
? " ?
?
0
2
? ?
&__inference_lstm_6_layer_call_fn_48802mUV F?C
<?9
+?(
&?#
inputs/0?????????2

 
p 

 
? "??????????2?
&__inference_lstm_6_layer_call_fn_48817mUV F?C
<?9
+?(
&?#
inputs/0?????????2

 
p

 
? "??????????2~
&__inference_lstm_6_layer_call_fn_48832TU V6?3
,?)
?
inputs
2

 
p 

 
? "?
2~
&__inference_lstm_6_layer_call_fn_48847TU V6?3
,?)
?
inputs
2

 
p

 
? "?
2?
A__inference_lstm_7_layer_call_and_return_conditional_losses_47481zNOF?C
<?9
+?(
&?#
inputs/0?????????

 
p 

 
? ")?&
?
0?????????2
? ?
A__inference_lstm_7_layer_call_and_return_conditional_losses_47659zNOF?C
<?9
+?(
&?#
inputs/0?????????

 
p

 
? ")?&
?
0?????????2
? ?
A__inference_lstm_7_layer_call_and_return_conditional_losses_47837aNO6?3
,?)
?
inputs


 
p 

 
? " ?
?
0
2
? ?
A__inference_lstm_7_layer_call_and_return_conditional_losses_48015aNO6?3
,?)
?
inputs


 
p

 
? " ?
?
0
2
? ?
&__inference_lstm_7_layer_call_fn_48030mNOF?C
<?9
+?(
&?#
inputs/0?????????

 
p 

 
? "??????????2?
&__inference_lstm_7_layer_call_fn_48045mNOF?C
<?9
+?(
&?#
inputs/0?????????

 
p

 
? "??????????2~
&__inference_lstm_7_layer_call_fn_48060TNO6?3
,?)
?
inputs


 
p 

 
? "?
2~
&__inference_lstm_7_layer_call_fn_48075TNO6?3
,?)
?
inputs


 
p

 
? "?
2?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49412? ???
???
?
inputs2
s?p
6?3	!?
?2
?

jstates/0VariableSpec
6?3	!?
?2
?

jstates/1VariableSpec
p 
? "F?C
<?9
?
0/0
'?$
?
0/1/0
?
0/1/1
? ?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49465? e?b
[?X
?
inputs2
9?6
?
states/02
?
states/12
p 
? "X?U
N?K
?
0/02
3?0
?
0/1/02
?
0/1/12
? ?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49518? e?b
[?X
?
inputs2
9?6
?
states/02
?
states/12
p
? "X?U
N?K
?
0/02
3?0
?
0/1/02
?
0/1/12
? ?
F__inference_lstm_cell_8_layer_call_and_return_conditional_losses_49575? ???
???
?
inputs2
s?p
6?3	!?
?2
?

jstates/0VariableSpec
6?3	!?
?2
?

jstates/1VariableSpec
p
? "F?C
<?9
?
0/0
'?$
?
0/1/0
?
0/1/1
? ?
+__inference_lstm_cell_8_layer_call_fn_49592? e?b
[?X
?
inputs2
9?6
?
states/02
?
states/12
p 
? "H?E
?
02
/?,
?
1/02
?
1/12?
+__inference_lstm_cell_8_layer_call_fn_49609? e?b
[?X
?
inputs2
9?6
?
states/02
?
states/12
p
? "H?E
?
02
/?,
?
1/02
?
1/12?
+__inference_lstm_cell_8_layer_call_fn_49683? ???
???
?
inputs2
s?p
6?3	!?
?2
?

jstates/0VariableSpec
6?3	!?
?2
?

jstates/1VariableSpec
p 
? "6?3
?	
0
#? 
?
1/0
?
1/1?
+__inference_lstm_cell_8_layer_call_fn_49757? ???
???
?
inputs2
s?p
6?3	!?
?2
?

jstates/0VariableSpec
6?3	!?
?2
?

jstates/1VariableSpec
p
? "6?3
?	
0
#? 
?
1/0
?
1/1?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49010????
???
?
inputs
s?p
6?3	!?
?2
?

jstates/0VariableSpec
6?3	!?
?2
?

jstates/1VariableSpec
p 
? "F?C
<?9
?
0/0
'?$
?
0/1/0
?
0/1/1
? ?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49063?e?b
[?X
?
inputs
9?6
?
states/02
?
states/12
p 
? "X?U
N?K
?
0/02
3?0
?
0/1/02
?
0/1/12
? ?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49116?e?b
[?X
?
inputs
9?6
?
states/02
?
states/12
p
? "X?U
N?K
?
0/02
3?0
?
0/1/02
?
0/1/12
? ?
F__inference_lstm_cell_9_layer_call_and_return_conditional_losses_49173????
???
?
inputs
s?p
6?3	!?
?2
?

jstates/0VariableSpec
6?3	!?
?2
?

jstates/1VariableSpec
p
? "F?C
<?9
?
0/0
'?$
?
0/1/0
?
0/1/1
? ?
+__inference_lstm_cell_9_layer_call_fn_49190?e?b
[?X
?
inputs
9?6
?
states/02
?
states/12
p 
? "H?E
?
02
/?,
?
1/02
?
1/12?
+__inference_lstm_cell_9_layer_call_fn_49207?e?b
[?X
?
inputs
9?6
?
states/02
?
states/12
p
? "H?E
?
02
/?,
?
1/02
?
1/12?
+__inference_lstm_cell_9_layer_call_fn_49281????
???
?
inputs
s?p
6?3	!?
?2
?

jstates/0VariableSpec
6?3	!?
?2
?

jstates/1VariableSpec
p 
? "6?3
?	
0
#? 
?
1/0
?
1/1?
+__inference_lstm_cell_9_layer_call_fn_49355????
???
?
inputs
s?p
6?3	!?
?2
?

jstates/0VariableSpec
6?3	!?
?2
?

jstates/1VariableSpec
p
? "6?3
?	
0
#? 
?
1/0
?
1/1?
G__inference_sequential_3_layer_call_and_return_conditional_losses_46453jNOU V!"8?5
.?+
!?
lstm_7_input

p 

 
? " ?
?
0

? ?
G__inference_sequential_3_layer_call_and_return_conditional_losses_46486jNOU V!"8?5
.?+
!?
lstm_7_input

p

 
? " ?
?
0

? ?
G__inference_sequential_3_layer_call_and_return_conditional_losses_46881dNOU V!"2?/
(?%
?
inputs

p 

 
? " ?
?
0

? ?
G__inference_sequential_3_layer_call_and_return_conditional_losses_47245dNOU V!"2?/
(?%
?
inputs

p

 
? " ?
?
0

? ?
,__inference_sequential_3_layer_call_fn_45852]NOU V!"8?5
.?+
!?
lstm_7_input

p 

 
? "?
?
,__inference_sequential_3_layer_call_fn_46420]NOU V!"8?5
.?+
!?
lstm_7_input

p

 
? "?
?
,__inference_sequential_3_layer_call_fn_47274WNOU V!"2?/
(?%
?
inputs

p 

 
? "?
?
,__inference_sequential_3_layer_call_fn_47303WNOU V!"2?/
(?%
?
inputs

p

 
? "?
?
#__inference_signature_wrapper_46517?NOU V!"@?=
? 
6?3
1
lstm_7_input!?
lstm_7_input
"B??
=
time_distributed_3'?$
time_distributed_3
?
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_48868~!"D?A
:?7
-?*
inputs??????????????????2
p 

 
? "2?/
(?%
0??????????????????
? ?
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_48889~!"D?A
:?7
-?*
inputs??????????????????2
p

 
? "2?/
(?%
0??????????????????
? ?
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_48903Z!"2?/
(?%
?
inputs
2
p 

 
? " ?
?
0

? ?
M__inference_time_distributed_3_layer_call_and_return_conditional_losses_48917Z!"2?/
(?%
?
inputs
2
p

 
? " ?
?
0

? ?
2__inference_time_distributed_3_layer_call_fn_48926q!"D?A
:?7
-?*
inputs??????????????????2
p 

 
? "%?"???????????????????
2__inference_time_distributed_3_layer_call_fn_48935q!"D?A
:?7
-?*
inputs??????????????????2
p

 
? "%?"???????????????????
2__inference_time_distributed_3_layer_call_fn_48944M!"2?/
(?%
?
inputs
2
p 

 
? "?
?
2__inference_time_distributed_3_layer_call_fn_48953M!"2?/
(?%
?
inputs
2
p

 
? "?
