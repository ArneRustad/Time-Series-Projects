³2
ä
D
AddV2
x"T
y"T
z"T"
Ttype:
2	
B
AssignVariableOp
resource
value"dtype"
dtypetype
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
delete_old_dirsbool(
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
2	
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
dtypetype
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
list(type)(0
l
SaveV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0
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
¾
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
executor_typestring 
@
StaticRegexFullMatch	
input

output
"
patternstring
ö
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
«
TensorListFromTensor
tensor"element_dtype
element_shape"
shape_type*
output_handleéelement_dtype"
element_dtypetype"

shape_typetype:
2	

TensorListReserve
element_shape"
shape_type
num_elements#
handleéelement_dtype"
element_dtypetype"

shape_typetype:
2	

TensorListStack
input_handle
element_shape
tensor"element_dtype"
element_dtypetype" 
num_elementsintÿÿÿÿÿÿÿÿÿ
P
	Transpose
x"T
perm"Tperm
y"T"	
Ttype"
Tpermtype0:
2	

VarHandleOp
resource"
	containerstring "
shared_namestring "
dtypetype"
shapeshape"#
allowed_deviceslist(string)
 

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
"serve*2.6.02v2.6.0-rc2-32-g919f693420e8Ý0

lstm_21/lstm_cell_21/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	È*,
shared_namelstm_21/lstm_cell_21/kernel

/lstm_21/lstm_cell_21/kernel/Read/ReadVariableOpReadVariableOplstm_21/lstm_cell_21/kernel*
_output_shapes
:	È*
dtype0
§
%lstm_21/lstm_cell_21/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2È*6
shared_name'%lstm_21/lstm_cell_21/recurrent_kernel
 
9lstm_21/lstm_cell_21/recurrent_kernel/Read/ReadVariableOpReadVariableOp%lstm_21/lstm_cell_21/recurrent_kernel*
_output_shapes
:	2È*
dtype0

lstm_21/lstm_cell_21/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:È**
shared_namelstm_21/lstm_cell_21/bias

-lstm_21/lstm_cell_21/bias/Read/ReadVariableOpReadVariableOplstm_21/lstm_cell_21/bias*
_output_shapes	
:È*
dtype0

lstm_20/lstm_cell_20/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2È*,
shared_namelstm_20/lstm_cell_20/kernel

/lstm_20/lstm_cell_20/kernel/Read/ReadVariableOpReadVariableOplstm_20/lstm_cell_20/kernel*
_output_shapes
:	2È*
dtype0
§
%lstm_20/lstm_cell_20/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2È*6
shared_name'%lstm_20/lstm_cell_20/recurrent_kernel
 
9lstm_20/lstm_cell_20/recurrent_kernel/Read/ReadVariableOpReadVariableOp%lstm_20/lstm_cell_20/recurrent_kernel*
_output_shapes
:	2È*
dtype0

lstm_20/lstm_cell_20/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:È**
shared_namelstm_20/lstm_cell_20/bias

-lstm_20/lstm_cell_20/bias/Read/ReadVariableOpReadVariableOplstm_20/lstm_cell_20/bias*
_output_shapes	
:È*
dtype0

time_distributed_10/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:2*+
shared_nametime_distributed_10/kernel

.time_distributed_10/kernel/Read/ReadVariableOpReadVariableOptime_distributed_10/kernel*
_output_shapes

:2*
dtype0

time_distributed_10/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*)
shared_nametime_distributed_10/bias

,time_distributed_10/bias/Read/ReadVariableOpReadVariableOptime_distributed_10/bias*
_output_shapes
:*
dtype0
|
lstm_21/VariableVarHandleOp*
_output_shapes
: *
dtype0*
shape
:2*!
shared_namelstm_21/Variable
u
$lstm_21/Variable/Read/ReadVariableOpReadVariableOplstm_21/Variable*
_output_shapes

:2*
dtype0

lstm_21/Variable_1VarHandleOp*
_output_shapes
: *
dtype0*
shape
:2*#
shared_namelstm_21/Variable_1
y
&lstm_21/Variable_1/Read/ReadVariableOpReadVariableOplstm_21/Variable_1*
_output_shapes

:2*
dtype0
|
lstm_20/VariableVarHandleOp*
_output_shapes
: *
dtype0*
shape
:2*!
shared_namelstm_20/Variable
u
$lstm_20/Variable/Read/ReadVariableOpReadVariableOplstm_20/Variable*
_output_shapes

:2*
dtype0

lstm_20/Variable_1VarHandleOp*
_output_shapes
: *
dtype0*
shape
:2*#
shared_namelstm_20/Variable_1
y
&lstm_20/Variable_1/Read/ReadVariableOpReadVariableOplstm_20/Variable_1*
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
í#
ConstConst"/device:CPU:0*
_output_shapes
: *
dtype0*¨#
value#B# B#
æ
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
layer_with_weights-2
layer-2
	optimizer
trainable_variables
	variables
regularization_losses
	keras_api
	
signatures
l

cell

state_spec
trainable_variables
	variables
regularization_losses
	keras_api
l
cell

state_spec
trainable_variables
	variables
regularization_losses
	keras_api
]
	layer
trainable_variables
	variables
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
­
trainable_variables

#layers
	variables
$metrics
regularization_losses
%layer_metrics
&layer_regularization_losses
'non_trainable_variables
 

(
state_size

kernel
recurrent_kernel
bias
)trainable_variables
*	variables
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
¹
trainable_variables

-layers
	variables
.metrics
regularization_losses
/layer_metrics
0layer_regularization_losses
1non_trainable_variables

2states

3
state_size

kernel
recurrent_kernel
 bias
4trainable_variables
5	variables
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
¹
trainable_variables

8layers
	variables
9metrics
regularization_losses
:layer_metrics
;layer_regularization_losses
<non_trainable_variables

=states
h

!kernel
"bias
>trainable_variables
?	variables
@regularization_losses
A	keras_api

!0
"1

!0
"1
 
­
trainable_variables

Blayers
	variables
Cmetrics
regularization_losses
Dlayer_metrics
Elayer_regularization_losses
Fnon_trainable_variables
a_
VARIABLE_VALUElstm_21/lstm_cell_21/kernel0trainable_variables/0/.ATTRIBUTES/VARIABLE_VALUE
ki
VARIABLE_VALUE%lstm_21/lstm_cell_21/recurrent_kernel0trainable_variables/1/.ATTRIBUTES/VARIABLE_VALUE
_]
VARIABLE_VALUElstm_21/lstm_cell_21/bias0trainable_variables/2/.ATTRIBUTES/VARIABLE_VALUE
a_
VARIABLE_VALUElstm_20/lstm_cell_20/kernel0trainable_variables/3/.ATTRIBUTES/VARIABLE_VALUE
ki
VARIABLE_VALUE%lstm_20/lstm_cell_20/recurrent_kernel0trainable_variables/4/.ATTRIBUTES/VARIABLE_VALUE
_]
VARIABLE_VALUElstm_20/lstm_cell_20/bias0trainable_variables/5/.ATTRIBUTES/VARIABLE_VALUE
`^
VARIABLE_VALUEtime_distributed_10/kernel0trainable_variables/6/.ATTRIBUTES/VARIABLE_VALUE
^\
VARIABLE_VALUEtime_distributed_10/bias0trainable_variables/7/.ATTRIBUTES/VARIABLE_VALUE

0
1
2

G0
H1
 
 
 
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
­
)trainable_variables

Ilayers
*	variables
Jmetrics
+regularization_losses
Klayer_metrics
Llayer_regularization_losses
Mnon_trainable_variables


0
 
 
 
 

N0
O1
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
­
4trainable_variables

Players
5	variables
Qmetrics
6regularization_losses
Rlayer_metrics
Slayer_regularization_losses
Tnon_trainable_variables

0
 
 
 
 

U0
V1

!0
"1

!0
"1
 
­
>trainable_variables

Wlayers
?	variables
Xmetrics
@regularization_losses
Ylayer_metrics
Zlayer_regularization_losses
[non_trainable_variables

0
 
 
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
hf
VARIABLE_VALUElstm_21/VariableBlayer_with_weights-0/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUE
jh
VARIABLE_VALUElstm_21/Variable_1Blayer_with_weights-0/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUE
 
 
 
 
 
hf
VARIABLE_VALUElstm_20/VariableBlayer_with_weights-1/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUE
jh
VARIABLE_VALUElstm_20/Variable_1Blayer_with_weights-1/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUE
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
v
serving_default_lstm_21_inputPlaceholder*"
_output_shapes
:
*
dtype0*
shape:


StatefulPartitionedCallStatefulPartitionedCallserving_default_lstm_21_inputlstm_21/lstm_cell_21/kernellstm_21/Variable%lstm_21/lstm_cell_21/recurrent_kernellstm_21/lstm_cell_21/biaslstm_21/Variable_1lstm_20/lstm_cell_20/kernellstm_20/Variable%lstm_20/lstm_cell_20/recurrent_kernellstm_20/lstm_cell_20/biaslstm_20/Variable_1time_distributed_10/kerneltime_distributed_10/bias*
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
GPU 2J 8 *,
f'R%
#__inference_signature_wrapper_58013
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
Ê
StatefulPartitionedCall_1StatefulPartitionedCallsaver_filename/lstm_21/lstm_cell_21/kernel/Read/ReadVariableOp9lstm_21/lstm_cell_21/recurrent_kernel/Read/ReadVariableOp-lstm_21/lstm_cell_21/bias/Read/ReadVariableOp/lstm_20/lstm_cell_20/kernel/Read/ReadVariableOp9lstm_20/lstm_cell_20/recurrent_kernel/Read/ReadVariableOp-lstm_20/lstm_cell_20/bias/Read/ReadVariableOp.time_distributed_10/kernel/Read/ReadVariableOp,time_distributed_10/bias/Read/ReadVariableOp$lstm_21/Variable/Read/ReadVariableOp&lstm_21/Variable_1/Read/ReadVariableOp$lstm_20/Variable/Read/ReadVariableOp&lstm_20/Variable_1/Read/ReadVariableOptotal/Read/ReadVariableOpcount/Read/ReadVariableOptotal_1/Read/ReadVariableOpcount_1/Read/ReadVariableOpConst*
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
GPU 2J 8 *'
f"R 
__inference__traced_save_61343

StatefulPartitionedCall_2StatefulPartitionedCallsaver_filenamelstm_21/lstm_cell_21/kernel%lstm_21/lstm_cell_21/recurrent_kernellstm_21/lstm_cell_21/biaslstm_20/lstm_cell_20/kernel%lstm_20/lstm_cell_20/recurrent_kernellstm_20/lstm_cell_20/biastime_distributed_10/kerneltime_distributed_10/biaslstm_21/Variablelstm_21/Variable_1lstm_20/Variablelstm_20/Variable_1totalcounttotal_1count_1*
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
GPU 2J 8 **
f%R#
!__inference__traced_restore_61401Î0
È
õ
,__inference_lstm_cell_21_layer_call_fn_60686

inputs
states_0
states_1
unknown:	È
	unknown_0:	2È
	unknown_1:	È
identity

identity_1

identity_2¢StatefulPartitionedCall§
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_553962
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
÷,

G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60612

inputs
states_0
states_11
matmul_readvariableop_resource:	È3
 matmul_1_readvariableop_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2
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
­
Ð
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_56810

inputs 
dense_10_56800:2
dense_10_56802:
identity¢ dense_10/StatefulPartitionedCallD
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
strided_slice/stack_2â
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
valueB"ÿÿÿÿ2   2
Reshape/shapeo
ReshapeReshapeinputsReshape/shape:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22	
Reshape
 dense_10/StatefulPartitionedCallStatefulPartitionedCallReshape:output:0dense_10_56800dense_10_56802*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *L
fGRE
C__inference_dense_10_layer_call_and_return_conditional_losses_567992"
 dense_10/StatefulPartitionedCallq
Reshape_1/shape/0Const*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
Reshape_1/shape/0h
Reshape_1/shape/2Const*
_output_shapes
: *
dtype0*
value	B :2
Reshape_1/shape/2¨
Reshape_1/shapePackReshape_1/shape/0:output:0strided_slice:output:0Reshape_1/shape/2:output:0*
N*
T0*
_output_shapes
:2
Reshape_1/shape¥
	Reshape_1Reshape)dense_10/StatefulPartitionedCall:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2

Identityq
NoOpNoOp!^dense_10/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2: : 2D
 dense_10/StatefulPartitionedCall dense_10/StatefulPartitionedCall:\ X
4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
÷,

G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_60961

inputs
states_0
states_11
matmul_readvariableop_resource:	2È3
 matmul_1_readvariableop_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2
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
¥

ô
C__inference_dense_10_layer_call_and_return_conditional_losses_61263

inputs0
matmul_readvariableop_resource:2-
biasadd_readvariableop_resource:
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:2*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
MatMul
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOp
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2	
BiasAddk
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2

Identity
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ2: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
ø9

B__inference_lstm_20_layer_call_and_return_conditional_losses_56218

inputs$
lstm_cell_20_56077:2$
lstm_cell_20_56079:2%
lstm_cell_20_56081:	2È%
lstm_cell_20_56083:	2È!
lstm_cell_20_56085:	È
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢$lstm_cell_20/StatefulPartitionedCall¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1
$lstm_cell_20/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_20_56077lstm_cell_20_56079lstm_cell_20_56081lstm_cell_20_56083lstm_cell_20_56085*
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_560762&
$lstm_cell_20/StatefulPartitionedCall
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
times
ReadVariableOpReadVariableOplstm_cell_20_56077*
_output_shapes

:2*
dtype02
ReadVariableOpw
ReadVariableOp_1ReadVariableOplstm_cell_20_56079*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter¥
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_20_56081lstm_cell_20_56083lstm_cell_20_56085*
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
while_body_56096*
condR
while_cond_56095*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm¥
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime·
AssignVariableOpAssignVariableOplstm_cell_20_56077while:output:4^ReadVariableOp%^lstm_cell_20/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp½
AssignVariableOp_1AssignVariableOplstm_cell_20_56079while:output:5^ReadVariableOp_1%^lstm_cell_20/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22

IdentityÉ
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1%^lstm_cell_20/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:ÿÿÿÿÿÿÿÿÿ2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12L
$lstm_cell_20/StatefulPartitionedCall$lstm_cell_20/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
ü.
º
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_60908

inputs
states_0
states_11
matmul_readvariableop_resource:	2È5
"matmul_1_readvariableop_1_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp¢MatMul_1/ReadVariableOp_1¢mul_2/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMulu
MatMul_1/ReadVariableOpReadVariableOpstates_0*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOp_1
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
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
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2Ì
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
±0
Ô
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_56397

inputs
states:2
states_1:21
matmul_readvariableop_resource:	2È5
"matmul_1_readvariableop_1_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp¢MatMul_1/ReadVariableOp_1¢mul_2/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:2*
dtype02
MatMul_1/ReadVariableOp
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOp_1
MatMul_1MatMulMatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2Ì
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
ÔY
Ë
while_body_59822
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_20_matmul_readvariableop_resource_0:	2ÈH
5while_lstm_cell_20_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_20_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_20_matmul_readvariableop_resource:	2ÈF
3while_lstm_cell_20_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_20_biasadd_readvariableop_resource:	È¢)while/lstm_cell_20/BiasAdd/ReadVariableOp¢(while/lstm_cell_20/MatMul/ReadVariableOp¢*while/lstm_cell_20/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_20/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_20_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02*
(while/lstm_cell_20/MatMul/ReadVariableOpÎ
while/lstm_cell_20/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/MatMulÏ
*while/lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_20_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_20/MatMul_1/ReadVariableOp·
while/lstm_cell_20/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_20/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/MatMul_1¯
while/lstm_cell_20/addAddV2#while/lstm_cell_20/MatMul:product:0%while/lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/addÈ
)while/lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_20_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_20/BiasAdd/ReadVariableOp¼
while/lstm_cell_20/BiasAddBiasAddwhile/lstm_cell_20/add:z:01while/lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/BiasAdd
"while/lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_20/split/split_dimç
while/lstm_cell_20/splitSplit+while/lstm_cell_20/split/split_dim:output:0#while/lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_20/splity
while/lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const}
while/lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_1¦
while/lstm_cell_20/MulMul!while/lstm_cell_20/split:output:0!while/lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul§
while/lstm_cell_20/Add_1AddV2while/lstm_cell_20/Mul:z:0#while/lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_1
*while/lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_20/clip_by_value/Minimum/yÛ
(while/lstm_cell_20/clip_by_value/MinimumMinimumwhile/lstm_cell_20/Add_1:z:03while/lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(while/lstm_cell_20/clip_by_value/Minimum
"while/lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_20/clip_by_value/yÓ
 while/lstm_cell_20/clip_by_valueMaximum,while/lstm_cell_20/clip_by_value/Minimum:z:0+while/lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 while/lstm_cell_20/clip_by_value}
while/lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const_2}
while/lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_3¬
while/lstm_cell_20/Mul_1Mul!while/lstm_cell_20/split:output:1#while/lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul_1©
while/lstm_cell_20/Add_2AddV2while/lstm_cell_20/Mul_1:z:0#while/lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_2¡
,while/lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_20/clip_by_value_1/Minimum/yá
*while/lstm_cell_20/clip_by_value_1/MinimumMinimumwhile/lstm_cell_20/Add_2:z:05while/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_20/clip_by_value_1/Minimum
$while/lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_20/clip_by_value_1/yÛ
"while/lstm_cell_20/clip_by_value_1Maximum.while/lstm_cell_20/clip_by_value_1/Minimum:z:0-while/lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_20/clip_by_value_1¡
while/lstm_cell_20/mul_2Mul&while/lstm_cell_20/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_2
while/lstm_cell_20/TanhTanh!while/lstm_cell_20/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_20/Tanh§
while/lstm_cell_20/mul_3Mul$while/lstm_cell_20/clip_by_value:z:0while/lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_3¢
while/lstm_cell_20/add_3AddV2while/lstm_cell_20/mul_2:z:0while/lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_20/add_3}
while/lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const_4}
while/lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_5¬
while/lstm_cell_20/Mul_4Mul!while/lstm_cell_20/split:output:3#while/lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul_4©
while/lstm_cell_20/Add_4AddV2while/lstm_cell_20/Mul_4:z:0#while/lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_4¡
,while/lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_20/clip_by_value_2/Minimum/yá
*while/lstm_cell_20/clip_by_value_2/MinimumMinimumwhile/lstm_cell_20/Add_4:z:05while/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_20/clip_by_value_2/Minimum
$while/lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_20/clip_by_value_2/yÛ
"while/lstm_cell_20/clip_by_value_2Maximum.while/lstm_cell_20/clip_by_value_2/Minimum:z:0-while/lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_20/clip_by_value_2
while/lstm_cell_20/Tanh_1Tanhwhile/lstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Tanh_1«
while/lstm_cell_20/mul_5Mul&while/lstm_cell_20/clip_by_value_2:z:0while/lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_20/mul_5:z:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identitywhile/lstm_cell_20/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_20/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_20/BiasAdd/ReadVariableOp)^while/lstm_cell_20/MatMul/ReadVariableOp+^while/lstm_cell_20/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_20_biasadd_readvariableop_resource4while_lstm_cell_20_biasadd_readvariableop_resource_0"l
3while_lstm_cell_20_matmul_1_readvariableop_resource5while_lstm_cell_20_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_20_matmul_readvariableop_resource3while_lstm_cell_20_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_20/BiasAdd/ReadVariableOp)while/lstm_cell_20/BiasAdd/ReadVariableOp2T
(while/lstm_cell_20/MatMul/ReadVariableOp(while/lstm_cell_20/MatMul/ReadVariableOp2X
*while/lstm_cell_20/MatMul_1/ReadVariableOp*while/lstm_cell_20/MatMul_1/ReadVariableOp: 
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
ï,

G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_56306

inputs

states
states_11
matmul_readvariableop_resource:	2È3
 matmul_1_readvariableop_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2
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
ô.
¸
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60838

inputs

states
states_11
matmul_readvariableop_resource:	È5
"matmul_1_readvariableop_1_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp¢MatMul_1/ReadVariableOp_1¢mul_2/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMuls
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOp_1
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
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
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2Ì
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
å$
Ø
while_body_55320
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
while_lstm_cell_21_55397_0:	È-
while_lstm_cell_21_55399_0:	2È)
while_lstm_cell_21_55401_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
while_lstm_cell_21_55397:	È+
while_lstm_cell_21_55399:	2È'
while_lstm_cell_21_55401:	È¢*while/lstm_cell_21/StatefulPartitionedCallÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÂ
*while/lstm_cell_21/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_21_55397_0while_lstm_cell_21_55399_0while_lstm_cell_21_55401_0*
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_553962,
*while/lstm_cell_21/StatefulPartitionedCall÷
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder3while/lstm_cell_21/StatefulPartitionedCall:output:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identity3while/lstm_cell_21/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identity3while/lstm_cell_21/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5

while/NoOpNoOp+^while/lstm_cell_21/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"6
while_lstm_cell_21_55397while_lstm_cell_21_55397_0"6
while_lstm_cell_21_55399while_lstm_cell_21_55399_0"6
while_lstm_cell_21_55401while_lstm_cell_21_55401_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2X
*while/lstm_cell_21/StatefulPartitionedCall*while/lstm_cell_21/StatefulPartitionedCall: 
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
æ
õ
,__inference_lstm_cell_20_layer_call_fn_61179

inputs
states_0
states_1
unknown:	2È
	unknown_0:	2È
	unknown_1:	È
identity

identity_1

identity_2¢StatefulPartitionedCall
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_611662
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
ï

(__inference_dense_10_layer_call_fn_61272

inputs
unknown:2
	unknown_0:
identity¢StatefulPartitionedCalló
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *L
fGRE
C__inference_dense_10_layer_call_and_return_conditional_losses_567992
StatefulPartitionedCall{
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ2: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs


Ü
lstm_21_while_cond_58085,
(lstm_21_while_lstm_21_while_loop_counter2
.lstm_21_while_lstm_21_while_maximum_iterations
lstm_21_while_placeholder
lstm_21_while_placeholder_1
lstm_21_while_placeholder_2
lstm_21_while_placeholder_3,
(lstm_21_while_less_lstm_21_strided_sliceC
?lstm_21_while_lstm_21_while_cond_58085___redundant_placeholder0C
?lstm_21_while_lstm_21_while_cond_58085___redundant_placeholder1C
?lstm_21_while_lstm_21_while_cond_58085___redundant_placeholder2C
?lstm_21_while_lstm_21_while_cond_58085___redundant_placeholder3
lstm_21_while_identity

lstm_21/while/LessLesslstm_21_while_placeholder(lstm_21_while_less_lstm_21_strided_slice*
T0*
_output_shapes
: 2
lstm_21/while/Lessu
lstm_21/while/IdentityIdentitylstm_21/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_21/while/Identity"9
lstm_21_while_identitylstm_21/while/Identity:output:0*(
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
¨
¼
while_cond_57677
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_57677___redundant_placeholder03
/while_while_cond_57677___redundant_placeholder13
/while_while_cond_57677___redundant_placeholder23
/while_while_cond_57677___redundant_placeholder3
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
®
Â
-__inference_sequential_10_layer_call_fn_57916
lstm_21_input
unknown:	È
	unknown_0:2
	unknown_1:	2È
	unknown_2:	È
	unknown_3:2
	unknown_4:	2È
	unknown_5:2
	unknown_6:	2È
	unknown_7:	È
	unknown_8:2
	unknown_9:2

unknown_10:
identity¢StatefulPartitionedCallù
StatefulPartitionedCallStatefulPartitionedCalllstm_21_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
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
GPU 2J 8 *Q
fLRJ
H__inference_sequential_10_layer_call_and_return_conditional_losses_578602
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
StatefulPartitionedCallStatefulPartitionedCall:Q M
"
_output_shapes
:

'
_user_specified_namelstm_21_input
¥

ô
C__inference_dense_10_layer_call_and_return_conditional_losses_56799

inputs0
matmul_readvariableop_resource:2-
biasadd_readvariableop_resource:
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:2*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
MatMul
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOp
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2	
BiasAddk
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2

Identity
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ2: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
í,
»
__inference__traced_save_61343
file_prefix:
6savev2_lstm_21_lstm_cell_21_kernel_read_readvariableopD
@savev2_lstm_21_lstm_cell_21_recurrent_kernel_read_readvariableop8
4savev2_lstm_21_lstm_cell_21_bias_read_readvariableop:
6savev2_lstm_20_lstm_cell_20_kernel_read_readvariableopD
@savev2_lstm_20_lstm_cell_20_recurrent_kernel_read_readvariableop8
4savev2_lstm_20_lstm_cell_20_bias_read_readvariableop9
5savev2_time_distributed_10_kernel_read_readvariableop7
3savev2_time_distributed_10_bias_read_readvariableop/
+savev2_lstm_21_variable_read_readvariableop1
-savev2_lstm_21_variable_1_read_readvariableop/
+savev2_lstm_20_variable_read_readvariableop1
-savev2_lstm_20_variable_1_read_readvariableop$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop&
"savev2_total_1_read_readvariableop&
"savev2_count_1_read_readvariableop
savev2_const

identity_1¢MergeV2Checkpoints
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
Const_1
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
ShardedFilename/shard¦
ShardedFilenameShardedFilenameStringJoin:output:0ShardedFilename/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: 2
ShardedFilename
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*«
value¡BB0trainable_variables/0/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/1/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/2/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/3/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/4/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/5/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/6/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/7/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
SaveV2/tensor_namesª
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*5
value,B*B B B B B B B B B B B B B B B B B 2
SaveV2/shape_and_slicesÚ
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:06savev2_lstm_21_lstm_cell_21_kernel_read_readvariableop@savev2_lstm_21_lstm_cell_21_recurrent_kernel_read_readvariableop4savev2_lstm_21_lstm_cell_21_bias_read_readvariableop6savev2_lstm_20_lstm_cell_20_kernel_read_readvariableop@savev2_lstm_20_lstm_cell_20_recurrent_kernel_read_readvariableop4savev2_lstm_20_lstm_cell_20_bias_read_readvariableop5savev2_time_distributed_10_kernel_read_readvariableop3savev2_time_distributed_10_bias_read_readvariableop+savev2_lstm_21_variable_read_readvariableop-savev2_lstm_21_variable_1_read_readvariableop+savev2_lstm_20_variable_read_readvariableop-savev2_lstm_20_variable_1_read_readvariableop savev2_total_read_readvariableop savev2_count_read_readvariableop"savev2_total_1_read_readvariableop"savev2_count_1_read_readvariableopsavev2_const"/device:CPU:0*
_output_shapes
 *
dtypes
22
SaveV2º
&MergeV2Checkpoints/checkpoint_prefixesPackShardedFilename:filename:0^SaveV2"/device:CPU:0*
N*
T0*
_output_shapes
:2(
&MergeV2Checkpoints/checkpoint_prefixes¡
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

identity_1Identity_1:output:0*
_input_shapes
~: :	È:	2È:È:	2È:	2È:È:2::2:2:2:2: : : : : 2(
MergeV2CheckpointsMergeV2Checkpoints:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:%!

_output_shapes
:	È:%!

_output_shapes
:	2È:!

_output_shapes	
:È:%!

_output_shapes
:	2È:%!

_output_shapes
:	2È:!

_output_shapes	
:È:$ 

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
o
¶
B__inference_lstm_21_layer_call_and_return_conditional_losses_59155
inputs_0>
+lstm_cell_21_matmul_readvariableop_resource:	È?
-lstm_cell_21_matmul_1_readvariableop_resource:2B
/lstm_cell_21_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_21_biasadd_readvariableop_resource:	È<
*lstm_cell_21_mul_2_readvariableop_resource:2
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_21/BiasAdd/ReadVariableOp¢"lstm_cell_21/MatMul/ReadVariableOp¢$lstm_cell_21/MatMul_1/ReadVariableOp¢&lstm_cell_21/MatMul_1/ReadVariableOp_1¢!lstm_cell_21/mul_2/ReadVariableOp¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_21/MatMul/ReadVariableOpReadVariableOp+lstm_cell_21_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02$
"lstm_cell_21/MatMul/ReadVariableOp¤
lstm_cell_21/MatMulMatMulstrided_slice_1:output:0*lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/MatMulº
$lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$lstm_cell_21/MatMul_1/ReadVariableOpÁ
&lstm_cell_21/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_21_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_21/MatMul_1/ReadVariableOp_1À
lstm_cell_21/MatMul_1MatMul,lstm_cell_21/MatMul_1/ReadVariableOp:value:0.lstm_cell_21/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/MatMul_1
lstm_cell_21/addAddV2lstm_cell_21/MatMul:product:0lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_cell_21/add´
#lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_21_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_21/BiasAdd/ReadVariableOp¤
lstm_cell_21/BiasAddBiasAddlstm_cell_21/add:z:0+lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/BiasAdd~
lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_21/split/split_dimÏ
lstm_cell_21/splitSplit%lstm_cell_21/split/split_dim:output:0lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_21/splitm
lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Constq
lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_1
lstm_cell_21/MulMullstm_cell_21/split:output:0lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul
lstm_cell_21/Add_1AddV2lstm_cell_21/Mul:z:0lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_1
$lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_21/clip_by_value/Minimum/yÃ
"lstm_cell_21/clip_by_value/MinimumMinimumlstm_cell_21/Add_1:z:0-lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22$
"lstm_cell_21/clip_by_value/Minimum
lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_21/clip_by_value/y»
lstm_cell_21/clip_by_valueMaximum&lstm_cell_21/clip_by_value/Minimum:z:0%lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_valueq
lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Const_2q
lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_3
lstm_cell_21/Mul_1Mullstm_cell_21/split:output:1lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul_1
lstm_cell_21/Add_2AddV2lstm_cell_21/Mul_1:z:0lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_2
&lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_21/clip_by_value_1/Minimum/yÉ
$lstm_cell_21/clip_by_value_1/MinimumMinimumlstm_cell_21/Add_2:z:0/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_21/clip_by_value_1/Minimum
lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_21/clip_by_value_1/yÃ
lstm_cell_21/clip_by_value_1Maximum(lstm_cell_21/clip_by_value_1/Minimum:z:0'lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_value_1±
!lstm_cell_21/mul_2/ReadVariableOpReadVariableOp*lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02#
!lstm_cell_21/mul_2/ReadVariableOp¥
lstm_cell_21/mul_2Mul lstm_cell_21/clip_by_value_1:z:0)lstm_cell_21/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_2t
lstm_cell_21/TanhTanhlstm_cell_21/split:output:2*
T0*
_output_shapes

:22
lstm_cell_21/Tanh
lstm_cell_21/mul_3Mullstm_cell_21/clip_by_value:z:0lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_3
lstm_cell_21/add_3AddV2lstm_cell_21/mul_2:z:0lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_21/add_3q
lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Const_4q
lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_5
lstm_cell_21/Mul_4Mullstm_cell_21/split:output:3lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul_4
lstm_cell_21/Add_4AddV2lstm_cell_21/Mul_4:z:0lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_4
&lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_21/clip_by_value_2/Minimum/yÉ
$lstm_cell_21/clip_by_value_2/MinimumMinimumlstm_cell_21/Add_4:z:0/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_21/clip_by_value_2/Minimum
lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_21/clip_by_value_2/yÃ
lstm_cell_21/clip_by_value_2Maximum(lstm_cell_21/clip_by_value_2/Minimum:z:0'lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_value_2s
lstm_cell_21/Tanh_1Tanhlstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_21/Tanh_1
lstm_cell_21/mul_5Mul lstm_cell_21/clip_by_value_2:z:0lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
time
ReadVariableOpReadVariableOp-lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterõ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_21_matmul_readvariableop_resource/lstm_cell_21_matmul_1_readvariableop_1_resource,lstm_cell_21_biasadd_readvariableop_resource*
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
while_body_59050*
condR
while_cond_59049*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm¥
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_21_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_21/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_21_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_21/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_21/BiasAdd/ReadVariableOp#^lstm_cell_21/MatMul/ReadVariableOp%^lstm_cell_21/MatMul_1/ReadVariableOp'^lstm_cell_21/MatMul_1/ReadVariableOp_1"^lstm_cell_21/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:ÿÿÿÿÿÿÿÿÿ: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_21/BiasAdd/ReadVariableOp#lstm_cell_21/BiasAdd/ReadVariableOp2H
"lstm_cell_21/MatMul/ReadVariableOp"lstm_cell_21/MatMul/ReadVariableOp2L
$lstm_cell_21/MatMul_1/ReadVariableOp$lstm_cell_21/MatMul_1/ReadVariableOp2P
&lstm_cell_21/MatMul_1/ReadVariableOp_1&lstm_cell_21/MatMul_1/ReadVariableOp_12F
!lstm_cell_21/mul_2/ReadVariableOp!lstm_cell_21/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/0
ß
÷
H__inference_sequential_10_layer_call_and_return_conditional_losses_57321

inputs 
lstm_21_57099:	È
lstm_21_57101:2 
lstm_21_57103:	2È
lstm_21_57105:	È
lstm_21_57107:2 
lstm_20_57288:	2È
lstm_20_57290:2 
lstm_20_57292:	2È
lstm_20_57294:	È
lstm_20_57296:2+
time_distributed_10_57313:2'
time_distributed_10_57315:
identity¢lstm_20/StatefulPartitionedCall¢lstm_21/StatefulPartitionedCall¢+time_distributed_10/StatefulPartitionedCall¸
lstm_21/StatefulPartitionedCallStatefulPartitionedCallinputslstm_21_57099lstm_21_57101lstm_21_57103lstm_21_57105lstm_21_57107*
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
GPU 2J 8 *K
fFRD
B__inference_lstm_21_layer_call_and_return_conditional_losses_570982!
lstm_21/StatefulPartitionedCallÚ
lstm_20/StatefulPartitionedCallStatefulPartitionedCall(lstm_21/StatefulPartitionedCall:output:0lstm_20_57288lstm_20_57290lstm_20_57292lstm_20_57294lstm_20_57296*
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
GPU 2J 8 *K
fFRD
B__inference_lstm_20_layer_call_and_return_conditional_losses_572872!
lstm_20/StatefulPartitionedCallå
+time_distributed_10/StatefulPartitionedCallStatefulPartitionedCall(lstm_20/StatefulPartitionedCall:output:0time_distributed_10_57313time_distributed_10_57315*
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
GPU 2J 8 *W
fRRP
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_573122-
+time_distributed_10/StatefulPartitionedCall
!time_distributed_10/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2#
!time_distributed_10/Reshape/shapeÄ
time_distributed_10/ReshapeReshape(lstm_20/StatefulPartitionedCall:output:0*time_distributed_10/Reshape/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_10/Reshape
IdentityIdentity4time_distributed_10/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

IdentityÀ
NoOpNoOp ^lstm_20/StatefulPartitionedCall ^lstm_21/StatefulPartitionedCall,^time_distributed_10/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 2B
lstm_20/StatefulPartitionedCalllstm_20/StatefulPartitionedCall2B
lstm_21/StatefulPartitionedCalllstm_21/StatefulPartitionedCall2Z
+time_distributed_10/StatefulPartitionedCall+time_distributed_10/StatefulPartitionedCall:J F
"
_output_shapes
:

 
_user_specified_nameinputs
ø9

B__inference_lstm_21_layer_call_and_return_conditional_losses_55759

inputs$
lstm_cell_21_55671:2$
lstm_cell_21_55673:2%
lstm_cell_21_55675:	È%
lstm_cell_21_55677:	2È!
lstm_cell_21_55679:	È
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢$lstm_cell_21/StatefulPartitionedCall¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1
$lstm_cell_21/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_21_55671lstm_cell_21_55673lstm_cell_21_55675lstm_cell_21_55677lstm_cell_21_55679*
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_556212&
$lstm_cell_21/StatefulPartitionedCall
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
times
ReadVariableOpReadVariableOplstm_cell_21_55671*
_output_shapes

:2*
dtype02
ReadVariableOpw
ReadVariableOp_1ReadVariableOplstm_cell_21_55673*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter¥
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_21_55675lstm_cell_21_55677lstm_cell_21_55679*
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
while_body_55690*
condR
while_cond_55689*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm¥
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime·
AssignVariableOpAssignVariableOplstm_cell_21_55671while:output:4^ReadVariableOp%^lstm_cell_21/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp½
AssignVariableOp_1AssignVariableOplstm_cell_21_55673while:output:5^ReadVariableOp_1%^lstm_cell_21/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22

IdentityÉ
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1%^lstm_cell_21/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:ÿÿÿÿÿÿÿÿÿ: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12L
$lstm_cell_21/StatefulPartitionedCall$lstm_cell_21/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs
å$
Ø
while_body_55690
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
while_lstm_cell_21_55714_0:	È-
while_lstm_cell_21_55716_0:	2È)
while_lstm_cell_21_55718_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
while_lstm_cell_21_55714:	È+
while_lstm_cell_21_55716:	2È'
while_lstm_cell_21_55718:	È¢*while/lstm_cell_21/StatefulPartitionedCallÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÂ
*while/lstm_cell_21/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_21_55714_0while_lstm_cell_21_55716_0while_lstm_cell_21_55718_0*
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_555302,
*while/lstm_cell_21/StatefulPartitionedCall÷
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder3while/lstm_cell_21/StatefulPartitionedCall:output:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identity3while/lstm_cell_21/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identity3while/lstm_cell_21/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5

while/NoOpNoOp+^while/lstm_cell_21/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"6
while_lstm_cell_21_55714while_lstm_cell_21_55714_0"6
while_lstm_cell_21_55716while_lstm_cell_21_55716_0"6
while_lstm_cell_21_55718while_lstm_cell_21_55718_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2X
*while/lstm_cell_21/StatefulPartitionedCall*while/lstm_cell_21/StatefulPartitionedCall: 
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
o
¶
B__inference_lstm_21_layer_call_and_return_conditional_losses_58977
inputs_0>
+lstm_cell_21_matmul_readvariableop_resource:	È?
-lstm_cell_21_matmul_1_readvariableop_resource:2B
/lstm_cell_21_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_21_biasadd_readvariableop_resource:	È<
*lstm_cell_21_mul_2_readvariableop_resource:2
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_21/BiasAdd/ReadVariableOp¢"lstm_cell_21/MatMul/ReadVariableOp¢$lstm_cell_21/MatMul_1/ReadVariableOp¢&lstm_cell_21/MatMul_1/ReadVariableOp_1¢!lstm_cell_21/mul_2/ReadVariableOp¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_21/MatMul/ReadVariableOpReadVariableOp+lstm_cell_21_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02$
"lstm_cell_21/MatMul/ReadVariableOp¤
lstm_cell_21/MatMulMatMulstrided_slice_1:output:0*lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/MatMulº
$lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$lstm_cell_21/MatMul_1/ReadVariableOpÁ
&lstm_cell_21/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_21_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_21/MatMul_1/ReadVariableOp_1À
lstm_cell_21/MatMul_1MatMul,lstm_cell_21/MatMul_1/ReadVariableOp:value:0.lstm_cell_21/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/MatMul_1
lstm_cell_21/addAddV2lstm_cell_21/MatMul:product:0lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_cell_21/add´
#lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_21_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_21/BiasAdd/ReadVariableOp¤
lstm_cell_21/BiasAddBiasAddlstm_cell_21/add:z:0+lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/BiasAdd~
lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_21/split/split_dimÏ
lstm_cell_21/splitSplit%lstm_cell_21/split/split_dim:output:0lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_21/splitm
lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Constq
lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_1
lstm_cell_21/MulMullstm_cell_21/split:output:0lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul
lstm_cell_21/Add_1AddV2lstm_cell_21/Mul:z:0lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_1
$lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_21/clip_by_value/Minimum/yÃ
"lstm_cell_21/clip_by_value/MinimumMinimumlstm_cell_21/Add_1:z:0-lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22$
"lstm_cell_21/clip_by_value/Minimum
lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_21/clip_by_value/y»
lstm_cell_21/clip_by_valueMaximum&lstm_cell_21/clip_by_value/Minimum:z:0%lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_valueq
lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Const_2q
lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_3
lstm_cell_21/Mul_1Mullstm_cell_21/split:output:1lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul_1
lstm_cell_21/Add_2AddV2lstm_cell_21/Mul_1:z:0lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_2
&lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_21/clip_by_value_1/Minimum/yÉ
$lstm_cell_21/clip_by_value_1/MinimumMinimumlstm_cell_21/Add_2:z:0/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_21/clip_by_value_1/Minimum
lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_21/clip_by_value_1/yÃ
lstm_cell_21/clip_by_value_1Maximum(lstm_cell_21/clip_by_value_1/Minimum:z:0'lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_value_1±
!lstm_cell_21/mul_2/ReadVariableOpReadVariableOp*lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02#
!lstm_cell_21/mul_2/ReadVariableOp¥
lstm_cell_21/mul_2Mul lstm_cell_21/clip_by_value_1:z:0)lstm_cell_21/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_2t
lstm_cell_21/TanhTanhlstm_cell_21/split:output:2*
T0*
_output_shapes

:22
lstm_cell_21/Tanh
lstm_cell_21/mul_3Mullstm_cell_21/clip_by_value:z:0lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_3
lstm_cell_21/add_3AddV2lstm_cell_21/mul_2:z:0lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_21/add_3q
lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Const_4q
lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_5
lstm_cell_21/Mul_4Mullstm_cell_21/split:output:3lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul_4
lstm_cell_21/Add_4AddV2lstm_cell_21/Mul_4:z:0lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_4
&lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_21/clip_by_value_2/Minimum/yÉ
$lstm_cell_21/clip_by_value_2/MinimumMinimumlstm_cell_21/Add_4:z:0/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_21/clip_by_value_2/Minimum
lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_21/clip_by_value_2/yÃ
lstm_cell_21/clip_by_value_2Maximum(lstm_cell_21/clip_by_value_2/Minimum:z:0'lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_value_2s
lstm_cell_21/Tanh_1Tanhlstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_21/Tanh_1
lstm_cell_21/mul_5Mul lstm_cell_21/clip_by_value_2:z:0lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
time
ReadVariableOpReadVariableOp-lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterõ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_21_matmul_readvariableop_resource/lstm_cell_21_matmul_1_readvariableop_1_resource,lstm_cell_21_biasadd_readvariableop_resource*
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
while_body_58872*
condR
while_cond_58871*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm¥
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_21_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_21/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_21_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_21/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_21/BiasAdd/ReadVariableOp#^lstm_cell_21/MatMul/ReadVariableOp%^lstm_cell_21/MatMul_1/ReadVariableOp'^lstm_cell_21/MatMul_1/ReadVariableOp_1"^lstm_cell_21/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:ÿÿÿÿÿÿÿÿÿ: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_21/BiasAdd/ReadVariableOp#lstm_cell_21/BiasAdd/ReadVariableOp2H
"lstm_cell_21/MatMul/ReadVariableOp"lstm_cell_21/MatMul/ReadVariableOp2L
$lstm_cell_21/MatMul_1/ReadVariableOp$lstm_cell_21/MatMul_1/ReadVariableOp2P
&lstm_cell_21/MatMul_1/ReadVariableOp_1&lstm_cell_21/MatMul_1/ReadVariableOp_12F
!lstm_cell_21/mul_2/ReadVariableOp!lstm_cell_21/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/0
ùn
´
B__inference_lstm_20_layer_call_and_return_conditional_losses_60105

inputs>
+lstm_cell_20_matmul_readvariableop_resource:	2È?
-lstm_cell_20_matmul_1_readvariableop_resource:2B
/lstm_cell_20_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_20_biasadd_readvariableop_resource:	È<
*lstm_cell_20_mul_2_readvariableop_resource:2
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_20/BiasAdd/ReadVariableOp¢"lstm_cell_20/MatMul/ReadVariableOp¢$lstm_cell_20/MatMul_1/ReadVariableOp¢&lstm_cell_20/MatMul_1/ReadVariableOp_1¢!lstm_cell_20/mul_2/ReadVariableOp¢whileu
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_20/MatMul/ReadVariableOpReadVariableOp+lstm_cell_20_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02$
"lstm_cell_20/MatMul/ReadVariableOp¤
lstm_cell_20/MatMulMatMulstrided_slice_1:output:0*lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/MatMulº
$lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$lstm_cell_20/MatMul_1/ReadVariableOpÁ
&lstm_cell_20/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_20_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_20/MatMul_1/ReadVariableOp_1À
lstm_cell_20/MatMul_1MatMul,lstm_cell_20/MatMul_1/ReadVariableOp:value:0.lstm_cell_20/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/MatMul_1
lstm_cell_20/addAddV2lstm_cell_20/MatMul:product:0lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_cell_20/add´
#lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_20_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_20/BiasAdd/ReadVariableOp¤
lstm_cell_20/BiasAddBiasAddlstm_cell_20/add:z:0+lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/BiasAdd~
lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_20/split/split_dimÏ
lstm_cell_20/splitSplit%lstm_cell_20/split/split_dim:output:0lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_20/splitm
lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Constq
lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_1
lstm_cell_20/MulMullstm_cell_20/split:output:0lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul
lstm_cell_20/Add_1AddV2lstm_cell_20/Mul:z:0lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_1
$lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_20/clip_by_value/Minimum/yÃ
"lstm_cell_20/clip_by_value/MinimumMinimumlstm_cell_20/Add_1:z:0-lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22$
"lstm_cell_20/clip_by_value/Minimum
lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_20/clip_by_value/y»
lstm_cell_20/clip_by_valueMaximum&lstm_cell_20/clip_by_value/Minimum:z:0%lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_valueq
lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Const_2q
lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_3
lstm_cell_20/Mul_1Mullstm_cell_20/split:output:1lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul_1
lstm_cell_20/Add_2AddV2lstm_cell_20/Mul_1:z:0lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_2
&lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_20/clip_by_value_1/Minimum/yÉ
$lstm_cell_20/clip_by_value_1/MinimumMinimumlstm_cell_20/Add_2:z:0/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_20/clip_by_value_1/Minimum
lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_20/clip_by_value_1/yÃ
lstm_cell_20/clip_by_value_1Maximum(lstm_cell_20/clip_by_value_1/Minimum:z:0'lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_value_1±
!lstm_cell_20/mul_2/ReadVariableOpReadVariableOp*lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02#
!lstm_cell_20/mul_2/ReadVariableOp¥
lstm_cell_20/mul_2Mul lstm_cell_20/clip_by_value_1:z:0)lstm_cell_20/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_2t
lstm_cell_20/TanhTanhlstm_cell_20/split:output:2*
T0*
_output_shapes

:22
lstm_cell_20/Tanh
lstm_cell_20/mul_3Mullstm_cell_20/clip_by_value:z:0lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_3
lstm_cell_20/add_3AddV2lstm_cell_20/mul_2:z:0lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_20/add_3q
lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Const_4q
lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_5
lstm_cell_20/Mul_4Mullstm_cell_20/split:output:3lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul_4
lstm_cell_20/Add_4AddV2lstm_cell_20/Mul_4:z:0lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_4
&lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_20/clip_by_value_2/Minimum/yÉ
$lstm_cell_20/clip_by_value_2/MinimumMinimumlstm_cell_20/Add_4:z:0/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_20/clip_by_value_2/Minimum
lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_20/clip_by_value_2/yÃ
lstm_cell_20/clip_by_value_2Maximum(lstm_cell_20/clip_by_value_2/Minimum:z:0'lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_value_2s
lstm_cell_20/Tanh_1Tanhlstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_20/Tanh_1
lstm_cell_20/mul_5Mul lstm_cell_20/clip_by_value_2:z:0lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
time
ReadVariableOpReadVariableOp-lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterõ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_20_matmul_readvariableop_resource/lstm_cell_20_matmul_1_readvariableop_1_resource,lstm_cell_20_biasadd_readvariableop_resource*
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
while_body_60000*
condR
while_cond_59999*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm
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
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_20_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_20/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_20_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_20/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_20/BiasAdd/ReadVariableOp#^lstm_cell_20/MatMul/ReadVariableOp%^lstm_cell_20/MatMul_1/ReadVariableOp'^lstm_cell_20/MatMul_1/ReadVariableOp_1"^lstm_cell_20/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_20/BiasAdd/ReadVariableOp#lstm_cell_20/BiasAdd/ReadVariableOp2H
"lstm_cell_20/MatMul/ReadVariableOp"lstm_cell_20/MatMul/ReadVariableOp2L
$lstm_cell_20/MatMul_1/ReadVariableOp$lstm_cell_20/MatMul_1/ReadVariableOp2P
&lstm_cell_20/MatMul_1/ReadVariableOp_1&lstm_cell_20/MatMul_1/ReadVariableOp_12F
!lstm_cell_20/mul_2/ReadVariableOp!lstm_cell_20/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
È
õ
,__inference_lstm_cell_20_layer_call_fn_61105

inputs
states_0
states_1
unknown:	2È
	unknown_0:	2È
	unknown_1:	È
identity

identity_1

identity_2¢StatefulPartitionedCall§
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_563062
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
ø9

B__inference_lstm_20_layer_call_and_return_conditional_losses_56535

inputs$
lstm_cell_20_56447:2$
lstm_cell_20_56449:2%
lstm_cell_20_56451:	2È%
lstm_cell_20_56453:	2È!
lstm_cell_20_56455:	È
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢$lstm_cell_20/StatefulPartitionedCall¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1
$lstm_cell_20/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_20_56447lstm_cell_20_56449lstm_cell_20_56451lstm_cell_20_56453lstm_cell_20_56455*
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_563972&
$lstm_cell_20/StatefulPartitionedCall
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
times
ReadVariableOpReadVariableOplstm_cell_20_56447*
_output_shapes

:2*
dtype02
ReadVariableOpw
ReadVariableOp_1ReadVariableOplstm_cell_20_56449*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter¥
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_20_56451lstm_cell_20_56453lstm_cell_20_56455*
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
while_body_56466*
condR
while_cond_56465*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm¥
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime·
AssignVariableOpAssignVariableOplstm_cell_20_56447while:output:4^ReadVariableOp%^lstm_cell_20/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp½
AssignVariableOp_1AssignVariableOplstm_cell_20_56449while:output:5^ReadVariableOp_1%^lstm_cell_20/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22

IdentityÉ
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1%^lstm_cell_20/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:ÿÿÿÿÿÿÿÿÿ2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12L
$lstm_cell_20/StatefulPartitionedCall$lstm_cell_20/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
ô.
¸
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_61240

inputs

states
states_11
matmul_readvariableop_resource:	2È5
"matmul_1_readvariableop_1_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp¢MatMul_1/ReadVariableOp_1¢mul_2/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMuls
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOp_1
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
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
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2Ì
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
·×
¾
 __inference__wrapped_model_55223
lstm_21_inputT
Asequential_10_lstm_21_lstm_cell_21_matmul_readvariableop_resource:	ÈU
Csequential_10_lstm_21_lstm_cell_21_matmul_1_readvariableop_resource:2X
Esequential_10_lstm_21_lstm_cell_21_matmul_1_readvariableop_1_resource:	2ÈQ
Bsequential_10_lstm_21_lstm_cell_21_biasadd_readvariableop_resource:	ÈR
@sequential_10_lstm_21_lstm_cell_21_mul_2_readvariableop_resource:2T
Asequential_10_lstm_20_lstm_cell_20_matmul_readvariableop_resource:	2ÈU
Csequential_10_lstm_20_lstm_cell_20_matmul_1_readvariableop_resource:2X
Esequential_10_lstm_20_lstm_cell_20_matmul_1_readvariableop_1_resource:	2ÈQ
Bsequential_10_lstm_20_lstm_cell_20_biasadd_readvariableop_resource:	ÈR
@sequential_10_lstm_20_lstm_cell_20_mul_2_readvariableop_resource:2[
Isequential_10_time_distributed_10_dense_10_matmul_readvariableop_resource:2X
Jsequential_10_time_distributed_10_dense_10_biasadd_readvariableop_resource:
identity¢&sequential_10/lstm_20/AssignVariableOp¢(sequential_10/lstm_20/AssignVariableOp_1¢$sequential_10/lstm_20/ReadVariableOp¢&sequential_10/lstm_20/ReadVariableOp_1¢9sequential_10/lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp¢8sequential_10/lstm_20/lstm_cell_20/MatMul/ReadVariableOp¢:sequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp¢<sequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1¢7sequential_10/lstm_20/lstm_cell_20/mul_2/ReadVariableOp¢sequential_10/lstm_20/while¢&sequential_10/lstm_21/AssignVariableOp¢(sequential_10/lstm_21/AssignVariableOp_1¢$sequential_10/lstm_21/ReadVariableOp¢&sequential_10/lstm_21/ReadVariableOp_1¢9sequential_10/lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp¢8sequential_10/lstm_21/lstm_cell_21/MatMul/ReadVariableOp¢:sequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp¢<sequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1¢7sequential_10/lstm_21/lstm_cell_21/mul_2/ReadVariableOp¢sequential_10/lstm_21/while¢Asequential_10/time_distributed_10/dense_10/BiasAdd/ReadVariableOp¢@sequential_10/time_distributed_10/dense_10/MatMul/ReadVariableOp¡
$sequential_10/lstm_21/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2&
$sequential_10/lstm_21/transpose/permº
sequential_10/lstm_21/transpose	Transposelstm_21_input-sequential_10/lstm_21/transpose/perm:output:0*
T0*"
_output_shapes
:
2!
sequential_10/lstm_21/transpose
sequential_10/lstm_21/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
         2
sequential_10/lstm_21/Shape 
)sequential_10/lstm_21/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2+
)sequential_10/lstm_21/strided_slice/stack¤
+sequential_10/lstm_21/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_10/lstm_21/strided_slice/stack_1¤
+sequential_10/lstm_21/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_10/lstm_21/strided_slice/stack_2æ
#sequential_10/lstm_21/strided_sliceStridedSlice$sequential_10/lstm_21/Shape:output:02sequential_10/lstm_21/strided_slice/stack:output:04sequential_10/lstm_21/strided_slice/stack_1:output:04sequential_10/lstm_21/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2%
#sequential_10/lstm_21/strided_slice±
1sequential_10/lstm_21/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ23
1sequential_10/lstm_21/TensorArrayV2/element_shape
#sequential_10/lstm_21/TensorArrayV2TensorListReserve:sequential_10/lstm_21/TensorArrayV2/element_shape:output:0,sequential_10/lstm_21/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02%
#sequential_10/lstm_21/TensorArrayV2ë
Ksequential_10/lstm_21/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      2M
Ksequential_10/lstm_21/TensorArrayUnstack/TensorListFromTensor/element_shapeÐ
=sequential_10/lstm_21/TensorArrayUnstack/TensorListFromTensorTensorListFromTensor#sequential_10/lstm_21/transpose:y:0Tsequential_10/lstm_21/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02?
=sequential_10/lstm_21/TensorArrayUnstack/TensorListFromTensor¤
+sequential_10/lstm_21/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2-
+sequential_10/lstm_21/strided_slice_1/stack¨
-sequential_10/lstm_21/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2/
-sequential_10/lstm_21/strided_slice_1/stack_1¨
-sequential_10/lstm_21/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2/
-sequential_10/lstm_21/strided_slice_1/stack_2÷
%sequential_10/lstm_21/strided_slice_1StridedSlice#sequential_10/lstm_21/transpose:y:04sequential_10/lstm_21/strided_slice_1/stack:output:06sequential_10/lstm_21/strided_slice_1/stack_1:output:06sequential_10/lstm_21/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2'
%sequential_10/lstm_21/strided_slice_1÷
8sequential_10/lstm_21/lstm_cell_21/MatMul/ReadVariableOpReadVariableOpAsequential_10_lstm_21_lstm_cell_21_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02:
8sequential_10/lstm_21/lstm_cell_21/MatMul/ReadVariableOpü
)sequential_10/lstm_21/lstm_cell_21/MatMulMatMul.sequential_10/lstm_21/strided_slice_1:output:0@sequential_10/lstm_21/lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2+
)sequential_10/lstm_21/lstm_cell_21/MatMulü
:sequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOpCsequential_10_lstm_21_lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02<
:sequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp
<sequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1ReadVariableOpEsequential_10_lstm_21_lstm_cell_21_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02>
<sequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1
+sequential_10/lstm_21/lstm_cell_21/MatMul_1MatMulBsequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp:value:0Dsequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2-
+sequential_10/lstm_21/lstm_cell_21/MatMul_1ï
&sequential_10/lstm_21/lstm_cell_21/addAddV23sequential_10/lstm_21/lstm_cell_21/MatMul:product:05sequential_10/lstm_21/lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2(
&sequential_10/lstm_21/lstm_cell_21/addö
9sequential_10/lstm_21/lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOpBsequential_10_lstm_21_lstm_cell_21_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02;
9sequential_10/lstm_21/lstm_cell_21/BiasAdd/ReadVariableOpü
*sequential_10/lstm_21/lstm_cell_21/BiasAddBiasAdd*sequential_10/lstm_21/lstm_cell_21/add:z:0Asequential_10/lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2,
*sequential_10/lstm_21/lstm_cell_21/BiasAddª
2sequential_10/lstm_21/lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :24
2sequential_10/lstm_21/lstm_cell_21/split/split_dim§
(sequential_10/lstm_21/lstm_cell_21/splitSplit;sequential_10/lstm_21/lstm_cell_21/split/split_dim:output:03sequential_10/lstm_21/lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2*
(sequential_10/lstm_21/lstm_cell_21/split
(sequential_10/lstm_21/lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2*
(sequential_10/lstm_21/lstm_cell_21/Const
*sequential_10/lstm_21/lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2,
*sequential_10/lstm_21/lstm_cell_21/Const_1æ
&sequential_10/lstm_21/lstm_cell_21/MulMul1sequential_10/lstm_21/lstm_cell_21/split:output:01sequential_10/lstm_21/lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22(
&sequential_10/lstm_21/lstm_cell_21/Mulç
(sequential_10/lstm_21/lstm_cell_21/Add_1AddV2*sequential_10/lstm_21/lstm_cell_21/Mul:z:03sequential_10/lstm_21/lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_21/lstm_cell_21/Add_1½
:sequential_10/lstm_21/lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2<
:sequential_10/lstm_21/lstm_cell_21/clip_by_value/Minimum/y
8sequential_10/lstm_21/lstm_cell_21/clip_by_value/MinimumMinimum,sequential_10/lstm_21/lstm_cell_21/Add_1:z:0Csequential_10/lstm_21/lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22:
8sequential_10/lstm_21/lstm_cell_21/clip_by_value/Minimum­
2sequential_10/lstm_21/lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    24
2sequential_10/lstm_21/lstm_cell_21/clip_by_value/y
0sequential_10/lstm_21/lstm_cell_21/clip_by_valueMaximum<sequential_10/lstm_21/lstm_cell_21/clip_by_value/Minimum:z:0;sequential_10/lstm_21/lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:222
0sequential_10/lstm_21/lstm_cell_21/clip_by_value
*sequential_10/lstm_21/lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2,
*sequential_10/lstm_21/lstm_cell_21/Const_2
*sequential_10/lstm_21/lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2,
*sequential_10/lstm_21/lstm_cell_21/Const_3ì
(sequential_10/lstm_21/lstm_cell_21/Mul_1Mul1sequential_10/lstm_21/lstm_cell_21/split:output:13sequential_10/lstm_21/lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_21/lstm_cell_21/Mul_1é
(sequential_10/lstm_21/lstm_cell_21/Add_2AddV2,sequential_10/lstm_21/lstm_cell_21/Mul_1:z:03sequential_10/lstm_21/lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_21/lstm_cell_21/Add_2Á
<sequential_10/lstm_21/lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2>
<sequential_10/lstm_21/lstm_cell_21/clip_by_value_1/Minimum/y¡
:sequential_10/lstm_21/lstm_cell_21/clip_by_value_1/MinimumMinimum,sequential_10/lstm_21/lstm_cell_21/Add_2:z:0Esequential_10/lstm_21/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22<
:sequential_10/lstm_21/lstm_cell_21/clip_by_value_1/Minimum±
4sequential_10/lstm_21/lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    26
4sequential_10/lstm_21/lstm_cell_21/clip_by_value_1/y
2sequential_10/lstm_21/lstm_cell_21/clip_by_value_1Maximum>sequential_10/lstm_21/lstm_cell_21/clip_by_value_1/Minimum:z:0=sequential_10/lstm_21/lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:224
2sequential_10/lstm_21/lstm_cell_21/clip_by_value_1ó
7sequential_10/lstm_21/lstm_cell_21/mul_2/ReadVariableOpReadVariableOp@sequential_10_lstm_21_lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype029
7sequential_10/lstm_21/lstm_cell_21/mul_2/ReadVariableOpý
(sequential_10/lstm_21/lstm_cell_21/mul_2Mul6sequential_10/lstm_21/lstm_cell_21/clip_by_value_1:z:0?sequential_10/lstm_21/lstm_cell_21/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_21/lstm_cell_21/mul_2¶
'sequential_10/lstm_21/lstm_cell_21/TanhTanh1sequential_10/lstm_21/lstm_cell_21/split:output:2*
T0*
_output_shapes

:22)
'sequential_10/lstm_21/lstm_cell_21/Tanhç
(sequential_10/lstm_21/lstm_cell_21/mul_3Mul4sequential_10/lstm_21/lstm_cell_21/clip_by_value:z:0+sequential_10/lstm_21/lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_21/lstm_cell_21/mul_3â
(sequential_10/lstm_21/lstm_cell_21/add_3AddV2,sequential_10/lstm_21/lstm_cell_21/mul_2:z:0,sequential_10/lstm_21/lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_21/lstm_cell_21/add_3
*sequential_10/lstm_21/lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2,
*sequential_10/lstm_21/lstm_cell_21/Const_4
*sequential_10/lstm_21/lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2,
*sequential_10/lstm_21/lstm_cell_21/Const_5ì
(sequential_10/lstm_21/lstm_cell_21/Mul_4Mul1sequential_10/lstm_21/lstm_cell_21/split:output:33sequential_10/lstm_21/lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_21/lstm_cell_21/Mul_4é
(sequential_10/lstm_21/lstm_cell_21/Add_4AddV2,sequential_10/lstm_21/lstm_cell_21/Mul_4:z:03sequential_10/lstm_21/lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_21/lstm_cell_21/Add_4Á
<sequential_10/lstm_21/lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2>
<sequential_10/lstm_21/lstm_cell_21/clip_by_value_2/Minimum/y¡
:sequential_10/lstm_21/lstm_cell_21/clip_by_value_2/MinimumMinimum,sequential_10/lstm_21/lstm_cell_21/Add_4:z:0Esequential_10/lstm_21/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22<
:sequential_10/lstm_21/lstm_cell_21/clip_by_value_2/Minimum±
4sequential_10/lstm_21/lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    26
4sequential_10/lstm_21/lstm_cell_21/clip_by_value_2/y
2sequential_10/lstm_21/lstm_cell_21/clip_by_value_2Maximum>sequential_10/lstm_21/lstm_cell_21/clip_by_value_2/Minimum:z:0=sequential_10/lstm_21/lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:224
2sequential_10/lstm_21/lstm_cell_21/clip_by_value_2µ
)sequential_10/lstm_21/lstm_cell_21/Tanh_1Tanh,sequential_10/lstm_21/lstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22+
)sequential_10/lstm_21/lstm_cell_21/Tanh_1ë
(sequential_10/lstm_21/lstm_cell_21/mul_5Mul6sequential_10/lstm_21/lstm_cell_21/clip_by_value_2:z:0-sequential_10/lstm_21/lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_21/lstm_cell_21/mul_5»
3sequential_10/lstm_21/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   25
3sequential_10/lstm_21/TensorArrayV2_1/element_shape
%sequential_10/lstm_21/TensorArrayV2_1TensorListReserve<sequential_10/lstm_21/TensorArrayV2_1/element_shape:output:0,sequential_10/lstm_21/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02'
%sequential_10/lstm_21/TensorArrayV2_1z
sequential_10/lstm_21/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential_10/lstm_21/timeÐ
$sequential_10/lstm_21/ReadVariableOpReadVariableOpCsequential_10_lstm_21_lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$sequential_10/lstm_21/ReadVariableOpÑ
&sequential_10/lstm_21/ReadVariableOp_1ReadVariableOp@sequential_10_lstm_21_lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02(
&sequential_10/lstm_21/ReadVariableOp_1«
.sequential_10/lstm_21/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ20
.sequential_10/lstm_21/while/maximum_iterations
(sequential_10/lstm_21/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2*
(sequential_10/lstm_21/while/loop_counter¿
sequential_10/lstm_21/whileWhile1sequential_10/lstm_21/while/loop_counter:output:07sequential_10/lstm_21/while/maximum_iterations:output:0#sequential_10/lstm_21/time:output:0.sequential_10/lstm_21/TensorArrayV2_1:handle:0,sequential_10/lstm_21/ReadVariableOp:value:0.sequential_10/lstm_21/ReadVariableOp_1:value:0,sequential_10/lstm_21/strided_slice:output:0Msequential_10/lstm_21/TensorArrayUnstack/TensorListFromTensor:output_handle:0Asequential_10_lstm_21_lstm_cell_21_matmul_readvariableop_resourceEsequential_10_lstm_21_lstm_cell_21_matmul_1_readvariableop_1_resourceBsequential_10_lstm_21_lstm_cell_21_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *2
body*R(
&sequential_10_lstm_21_while_body_54932*2
cond*R(
&sequential_10_lstm_21_while_cond_54931*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
sequential_10/lstm_21/whileá
Fsequential_10/lstm_21/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2H
Fsequential_10/lstm_21/TensorArrayV2Stack/TensorListStack/element_shape·
8sequential_10/lstm_21/TensorArrayV2Stack/TensorListStackTensorListStack$sequential_10/lstm_21/while:output:3Osequential_10/lstm_21/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02:
8sequential_10/lstm_21/TensorArrayV2Stack/TensorListStack­
+sequential_10/lstm_21/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2-
+sequential_10/lstm_21/strided_slice_2/stack¨
-sequential_10/lstm_21/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2/
-sequential_10/lstm_21/strided_slice_2/stack_1¨
-sequential_10/lstm_21/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2/
-sequential_10/lstm_21/strided_slice_2/stack_2
%sequential_10/lstm_21/strided_slice_2StridedSliceAsequential_10/lstm_21/TensorArrayV2Stack/TensorListStack:tensor:04sequential_10/lstm_21/strided_slice_2/stack:output:06sequential_10/lstm_21/strided_slice_2/stack_1:output:06sequential_10/lstm_21/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2'
%sequential_10/lstm_21/strided_slice_2¥
&sequential_10/lstm_21/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2(
&sequential_10/lstm_21/transpose_1/permô
!sequential_10/lstm_21/transpose_1	TransposeAsequential_10/lstm_21/TensorArrayV2Stack/TensorListStack:tensor:0/sequential_10/lstm_21/transpose_1/perm:output:0*
T0*"
_output_shapes
:
22#
!sequential_10/lstm_21/transpose_1
sequential_10/lstm_21/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential_10/lstm_21/runtimeÖ
&sequential_10/lstm_21/AssignVariableOpAssignVariableOpCsequential_10_lstm_21_lstm_cell_21_matmul_1_readvariableop_resource$sequential_10/lstm_21/while:output:4%^sequential_10/lstm_21/ReadVariableOp;^sequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02(
&sequential_10/lstm_21/AssignVariableOpÖ
(sequential_10/lstm_21/AssignVariableOp_1AssignVariableOp@sequential_10_lstm_21_lstm_cell_21_mul_2_readvariableop_resource$sequential_10/lstm_21/while:output:5'^sequential_10/lstm_21/ReadVariableOp_18^sequential_10/lstm_21/lstm_cell_21/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02*
(sequential_10/lstm_21/AssignVariableOp_1¡
$sequential_10/lstm_20/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2&
$sequential_10/lstm_20/transpose/permÒ
sequential_10/lstm_20/transpose	Transpose%sequential_10/lstm_21/transpose_1:y:0-sequential_10/lstm_20/transpose/perm:output:0*
T0*"
_output_shapes
:
22!
sequential_10/lstm_20/transpose
sequential_10/lstm_20/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
      2   2
sequential_10/lstm_20/Shape 
)sequential_10/lstm_20/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2+
)sequential_10/lstm_20/strided_slice/stack¤
+sequential_10/lstm_20/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_10/lstm_20/strided_slice/stack_1¤
+sequential_10/lstm_20/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_10/lstm_20/strided_slice/stack_2æ
#sequential_10/lstm_20/strided_sliceStridedSlice$sequential_10/lstm_20/Shape:output:02sequential_10/lstm_20/strided_slice/stack:output:04sequential_10/lstm_20/strided_slice/stack_1:output:04sequential_10/lstm_20/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2%
#sequential_10/lstm_20/strided_slice±
1sequential_10/lstm_20/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ23
1sequential_10/lstm_20/TensorArrayV2/element_shape
#sequential_10/lstm_20/TensorArrayV2TensorListReserve:sequential_10/lstm_20/TensorArrayV2/element_shape:output:0,sequential_10/lstm_20/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02%
#sequential_10/lstm_20/TensorArrayV2ë
Ksequential_10/lstm_20/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2M
Ksequential_10/lstm_20/TensorArrayUnstack/TensorListFromTensor/element_shapeÐ
=sequential_10/lstm_20/TensorArrayUnstack/TensorListFromTensorTensorListFromTensor#sequential_10/lstm_20/transpose:y:0Tsequential_10/lstm_20/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02?
=sequential_10/lstm_20/TensorArrayUnstack/TensorListFromTensor¤
+sequential_10/lstm_20/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2-
+sequential_10/lstm_20/strided_slice_1/stack¨
-sequential_10/lstm_20/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2/
-sequential_10/lstm_20/strided_slice_1/stack_1¨
-sequential_10/lstm_20/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2/
-sequential_10/lstm_20/strided_slice_1/stack_2÷
%sequential_10/lstm_20/strided_slice_1StridedSlice#sequential_10/lstm_20/transpose:y:04sequential_10/lstm_20/strided_slice_1/stack:output:06sequential_10/lstm_20/strided_slice_1/stack_1:output:06sequential_10/lstm_20/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2'
%sequential_10/lstm_20/strided_slice_1÷
8sequential_10/lstm_20/lstm_cell_20/MatMul/ReadVariableOpReadVariableOpAsequential_10_lstm_20_lstm_cell_20_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02:
8sequential_10/lstm_20/lstm_cell_20/MatMul/ReadVariableOpü
)sequential_10/lstm_20/lstm_cell_20/MatMulMatMul.sequential_10/lstm_20/strided_slice_1:output:0@sequential_10/lstm_20/lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2+
)sequential_10/lstm_20/lstm_cell_20/MatMulü
:sequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOpCsequential_10_lstm_20_lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02<
:sequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp
<sequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1ReadVariableOpEsequential_10_lstm_20_lstm_cell_20_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02>
<sequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1
+sequential_10/lstm_20/lstm_cell_20/MatMul_1MatMulBsequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp:value:0Dsequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2-
+sequential_10/lstm_20/lstm_cell_20/MatMul_1ï
&sequential_10/lstm_20/lstm_cell_20/addAddV23sequential_10/lstm_20/lstm_cell_20/MatMul:product:05sequential_10/lstm_20/lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2(
&sequential_10/lstm_20/lstm_cell_20/addö
9sequential_10/lstm_20/lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOpBsequential_10_lstm_20_lstm_cell_20_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02;
9sequential_10/lstm_20/lstm_cell_20/BiasAdd/ReadVariableOpü
*sequential_10/lstm_20/lstm_cell_20/BiasAddBiasAdd*sequential_10/lstm_20/lstm_cell_20/add:z:0Asequential_10/lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2,
*sequential_10/lstm_20/lstm_cell_20/BiasAddª
2sequential_10/lstm_20/lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :24
2sequential_10/lstm_20/lstm_cell_20/split/split_dim§
(sequential_10/lstm_20/lstm_cell_20/splitSplit;sequential_10/lstm_20/lstm_cell_20/split/split_dim:output:03sequential_10/lstm_20/lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2*
(sequential_10/lstm_20/lstm_cell_20/split
(sequential_10/lstm_20/lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2*
(sequential_10/lstm_20/lstm_cell_20/Const
*sequential_10/lstm_20/lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2,
*sequential_10/lstm_20/lstm_cell_20/Const_1æ
&sequential_10/lstm_20/lstm_cell_20/MulMul1sequential_10/lstm_20/lstm_cell_20/split:output:01sequential_10/lstm_20/lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22(
&sequential_10/lstm_20/lstm_cell_20/Mulç
(sequential_10/lstm_20/lstm_cell_20/Add_1AddV2*sequential_10/lstm_20/lstm_cell_20/Mul:z:03sequential_10/lstm_20/lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_20/lstm_cell_20/Add_1½
:sequential_10/lstm_20/lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2<
:sequential_10/lstm_20/lstm_cell_20/clip_by_value/Minimum/y
8sequential_10/lstm_20/lstm_cell_20/clip_by_value/MinimumMinimum,sequential_10/lstm_20/lstm_cell_20/Add_1:z:0Csequential_10/lstm_20/lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22:
8sequential_10/lstm_20/lstm_cell_20/clip_by_value/Minimum­
2sequential_10/lstm_20/lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    24
2sequential_10/lstm_20/lstm_cell_20/clip_by_value/y
0sequential_10/lstm_20/lstm_cell_20/clip_by_valueMaximum<sequential_10/lstm_20/lstm_cell_20/clip_by_value/Minimum:z:0;sequential_10/lstm_20/lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:222
0sequential_10/lstm_20/lstm_cell_20/clip_by_value
*sequential_10/lstm_20/lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2,
*sequential_10/lstm_20/lstm_cell_20/Const_2
*sequential_10/lstm_20/lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2,
*sequential_10/lstm_20/lstm_cell_20/Const_3ì
(sequential_10/lstm_20/lstm_cell_20/Mul_1Mul1sequential_10/lstm_20/lstm_cell_20/split:output:13sequential_10/lstm_20/lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_20/lstm_cell_20/Mul_1é
(sequential_10/lstm_20/lstm_cell_20/Add_2AddV2,sequential_10/lstm_20/lstm_cell_20/Mul_1:z:03sequential_10/lstm_20/lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_20/lstm_cell_20/Add_2Á
<sequential_10/lstm_20/lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2>
<sequential_10/lstm_20/lstm_cell_20/clip_by_value_1/Minimum/y¡
:sequential_10/lstm_20/lstm_cell_20/clip_by_value_1/MinimumMinimum,sequential_10/lstm_20/lstm_cell_20/Add_2:z:0Esequential_10/lstm_20/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22<
:sequential_10/lstm_20/lstm_cell_20/clip_by_value_1/Minimum±
4sequential_10/lstm_20/lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    26
4sequential_10/lstm_20/lstm_cell_20/clip_by_value_1/y
2sequential_10/lstm_20/lstm_cell_20/clip_by_value_1Maximum>sequential_10/lstm_20/lstm_cell_20/clip_by_value_1/Minimum:z:0=sequential_10/lstm_20/lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:224
2sequential_10/lstm_20/lstm_cell_20/clip_by_value_1ó
7sequential_10/lstm_20/lstm_cell_20/mul_2/ReadVariableOpReadVariableOp@sequential_10_lstm_20_lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype029
7sequential_10/lstm_20/lstm_cell_20/mul_2/ReadVariableOpý
(sequential_10/lstm_20/lstm_cell_20/mul_2Mul6sequential_10/lstm_20/lstm_cell_20/clip_by_value_1:z:0?sequential_10/lstm_20/lstm_cell_20/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_20/lstm_cell_20/mul_2¶
'sequential_10/lstm_20/lstm_cell_20/TanhTanh1sequential_10/lstm_20/lstm_cell_20/split:output:2*
T0*
_output_shapes

:22)
'sequential_10/lstm_20/lstm_cell_20/Tanhç
(sequential_10/lstm_20/lstm_cell_20/mul_3Mul4sequential_10/lstm_20/lstm_cell_20/clip_by_value:z:0+sequential_10/lstm_20/lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_20/lstm_cell_20/mul_3â
(sequential_10/lstm_20/lstm_cell_20/add_3AddV2,sequential_10/lstm_20/lstm_cell_20/mul_2:z:0,sequential_10/lstm_20/lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_20/lstm_cell_20/add_3
*sequential_10/lstm_20/lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2,
*sequential_10/lstm_20/lstm_cell_20/Const_4
*sequential_10/lstm_20/lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2,
*sequential_10/lstm_20/lstm_cell_20/Const_5ì
(sequential_10/lstm_20/lstm_cell_20/Mul_4Mul1sequential_10/lstm_20/lstm_cell_20/split:output:33sequential_10/lstm_20/lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_20/lstm_cell_20/Mul_4é
(sequential_10/lstm_20/lstm_cell_20/Add_4AddV2,sequential_10/lstm_20/lstm_cell_20/Mul_4:z:03sequential_10/lstm_20/lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_20/lstm_cell_20/Add_4Á
<sequential_10/lstm_20/lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2>
<sequential_10/lstm_20/lstm_cell_20/clip_by_value_2/Minimum/y¡
:sequential_10/lstm_20/lstm_cell_20/clip_by_value_2/MinimumMinimum,sequential_10/lstm_20/lstm_cell_20/Add_4:z:0Esequential_10/lstm_20/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22<
:sequential_10/lstm_20/lstm_cell_20/clip_by_value_2/Minimum±
4sequential_10/lstm_20/lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    26
4sequential_10/lstm_20/lstm_cell_20/clip_by_value_2/y
2sequential_10/lstm_20/lstm_cell_20/clip_by_value_2Maximum>sequential_10/lstm_20/lstm_cell_20/clip_by_value_2/Minimum:z:0=sequential_10/lstm_20/lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:224
2sequential_10/lstm_20/lstm_cell_20/clip_by_value_2µ
)sequential_10/lstm_20/lstm_cell_20/Tanh_1Tanh,sequential_10/lstm_20/lstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22+
)sequential_10/lstm_20/lstm_cell_20/Tanh_1ë
(sequential_10/lstm_20/lstm_cell_20/mul_5Mul6sequential_10/lstm_20/lstm_cell_20/clip_by_value_2:z:0-sequential_10/lstm_20/lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22*
(sequential_10/lstm_20/lstm_cell_20/mul_5»
3sequential_10/lstm_20/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   25
3sequential_10/lstm_20/TensorArrayV2_1/element_shape
%sequential_10/lstm_20/TensorArrayV2_1TensorListReserve<sequential_10/lstm_20/TensorArrayV2_1/element_shape:output:0,sequential_10/lstm_20/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02'
%sequential_10/lstm_20/TensorArrayV2_1z
sequential_10/lstm_20/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential_10/lstm_20/timeÐ
$sequential_10/lstm_20/ReadVariableOpReadVariableOpCsequential_10_lstm_20_lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$sequential_10/lstm_20/ReadVariableOpÑ
&sequential_10/lstm_20/ReadVariableOp_1ReadVariableOp@sequential_10_lstm_20_lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02(
&sequential_10/lstm_20/ReadVariableOp_1«
.sequential_10/lstm_20/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ20
.sequential_10/lstm_20/while/maximum_iterations
(sequential_10/lstm_20/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2*
(sequential_10/lstm_20/while/loop_counter¿
sequential_10/lstm_20/whileWhile1sequential_10/lstm_20/while/loop_counter:output:07sequential_10/lstm_20/while/maximum_iterations:output:0#sequential_10/lstm_20/time:output:0.sequential_10/lstm_20/TensorArrayV2_1:handle:0,sequential_10/lstm_20/ReadVariableOp:value:0.sequential_10/lstm_20/ReadVariableOp_1:value:0,sequential_10/lstm_20/strided_slice:output:0Msequential_10/lstm_20/TensorArrayUnstack/TensorListFromTensor:output_handle:0Asequential_10_lstm_20_lstm_cell_20_matmul_readvariableop_resourceEsequential_10_lstm_20_lstm_cell_20_matmul_1_readvariableop_1_resourceBsequential_10_lstm_20_lstm_cell_20_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *2
body*R(
&sequential_10_lstm_20_while_body_55106*2
cond*R(
&sequential_10_lstm_20_while_cond_55105*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
sequential_10/lstm_20/whileá
Fsequential_10/lstm_20/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2H
Fsequential_10/lstm_20/TensorArrayV2Stack/TensorListStack/element_shape·
8sequential_10/lstm_20/TensorArrayV2Stack/TensorListStackTensorListStack$sequential_10/lstm_20/while:output:3Osequential_10/lstm_20/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02:
8sequential_10/lstm_20/TensorArrayV2Stack/TensorListStack­
+sequential_10/lstm_20/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2-
+sequential_10/lstm_20/strided_slice_2/stack¨
-sequential_10/lstm_20/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2/
-sequential_10/lstm_20/strided_slice_2/stack_1¨
-sequential_10/lstm_20/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2/
-sequential_10/lstm_20/strided_slice_2/stack_2
%sequential_10/lstm_20/strided_slice_2StridedSliceAsequential_10/lstm_20/TensorArrayV2Stack/TensorListStack:tensor:04sequential_10/lstm_20/strided_slice_2/stack:output:06sequential_10/lstm_20/strided_slice_2/stack_1:output:06sequential_10/lstm_20/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2'
%sequential_10/lstm_20/strided_slice_2¥
&sequential_10/lstm_20/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2(
&sequential_10/lstm_20/transpose_1/permô
!sequential_10/lstm_20/transpose_1	TransposeAsequential_10/lstm_20/TensorArrayV2Stack/TensorListStack:tensor:0/sequential_10/lstm_20/transpose_1/perm:output:0*
T0*"
_output_shapes
:
22#
!sequential_10/lstm_20/transpose_1
sequential_10/lstm_20/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential_10/lstm_20/runtimeÖ
&sequential_10/lstm_20/AssignVariableOpAssignVariableOpCsequential_10_lstm_20_lstm_cell_20_matmul_1_readvariableop_resource$sequential_10/lstm_20/while:output:4%^sequential_10/lstm_20/ReadVariableOp;^sequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02(
&sequential_10/lstm_20/AssignVariableOpÖ
(sequential_10/lstm_20/AssignVariableOp_1AssignVariableOp@sequential_10_lstm_20_lstm_cell_20_mul_2_readvariableop_resource$sequential_10/lstm_20/while:output:5'^sequential_10/lstm_20/ReadVariableOp_18^sequential_10/lstm_20/lstm_cell_20/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02*
(sequential_10/lstm_20/AssignVariableOp_1³
/sequential_10/time_distributed_10/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   21
/sequential_10/time_distributed_10/Reshape/shapeë
)sequential_10/time_distributed_10/ReshapeReshape%sequential_10/lstm_20/transpose_1:y:08sequential_10/time_distributed_10/Reshape/shape:output:0*
T0*
_output_shapes

:
22+
)sequential_10/time_distributed_10/Reshape
@sequential_10/time_distributed_10/dense_10/MatMul/ReadVariableOpReadVariableOpIsequential_10_time_distributed_10_dense_10_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02B
@sequential_10/time_distributed_10/dense_10/MatMul/ReadVariableOp
1sequential_10/time_distributed_10/dense_10/MatMulMatMul2sequential_10/time_distributed_10/Reshape:output:0Hsequential_10/time_distributed_10/dense_10/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
23
1sequential_10/time_distributed_10/dense_10/MatMul
Asequential_10/time_distributed_10/dense_10/BiasAdd/ReadVariableOpReadVariableOpJsequential_10_time_distributed_10_dense_10_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02C
Asequential_10/time_distributed_10/dense_10/BiasAdd/ReadVariableOp¤
2sequential_10/time_distributed_10/dense_10/BiasAddBiasAdd;sequential_10/time_distributed_10/dense_10/MatMul:product:0Isequential_10/time_distributed_10/dense_10/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
24
2sequential_10/time_distributed_10/dense_10/BiasAdd»
1sequential_10/time_distributed_10/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      23
1sequential_10/time_distributed_10/Reshape_1/shape
+sequential_10/time_distributed_10/Reshape_1Reshape;sequential_10/time_distributed_10/dense_10/BiasAdd:output:0:sequential_10/time_distributed_10/Reshape_1/shape:output:0*
T0*"
_output_shapes
:
2-
+sequential_10/time_distributed_10/Reshape_1·
1sequential_10/time_distributed_10/Reshape_2/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   23
1sequential_10/time_distributed_10/Reshape_2/shapeñ
+sequential_10/time_distributed_10/Reshape_2Reshape%sequential_10/lstm_20/transpose_1:y:0:sequential_10/time_distributed_10/Reshape_2/shape:output:0*
T0*
_output_shapes

:
22-
+sequential_10/time_distributed_10/Reshape_2
IdentityIdentity4sequential_10/time_distributed_10/Reshape_1:output:0^NoOp*
T0*"
_output_shapes
:
2

Identity³	
NoOpNoOp'^sequential_10/lstm_20/AssignVariableOp)^sequential_10/lstm_20/AssignVariableOp_1%^sequential_10/lstm_20/ReadVariableOp'^sequential_10/lstm_20/ReadVariableOp_1:^sequential_10/lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp9^sequential_10/lstm_20/lstm_cell_20/MatMul/ReadVariableOp;^sequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp=^sequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_18^sequential_10/lstm_20/lstm_cell_20/mul_2/ReadVariableOp^sequential_10/lstm_20/while'^sequential_10/lstm_21/AssignVariableOp)^sequential_10/lstm_21/AssignVariableOp_1%^sequential_10/lstm_21/ReadVariableOp'^sequential_10/lstm_21/ReadVariableOp_1:^sequential_10/lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp9^sequential_10/lstm_21/lstm_cell_21/MatMul/ReadVariableOp;^sequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp=^sequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_18^sequential_10/lstm_21/lstm_cell_21/mul_2/ReadVariableOp^sequential_10/lstm_21/whileB^sequential_10/time_distributed_10/dense_10/BiasAdd/ReadVariableOpA^sequential_10/time_distributed_10/dense_10/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 2P
&sequential_10/lstm_20/AssignVariableOp&sequential_10/lstm_20/AssignVariableOp2T
(sequential_10/lstm_20/AssignVariableOp_1(sequential_10/lstm_20/AssignVariableOp_12L
$sequential_10/lstm_20/ReadVariableOp$sequential_10/lstm_20/ReadVariableOp2P
&sequential_10/lstm_20/ReadVariableOp_1&sequential_10/lstm_20/ReadVariableOp_12v
9sequential_10/lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp9sequential_10/lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp2t
8sequential_10/lstm_20/lstm_cell_20/MatMul/ReadVariableOp8sequential_10/lstm_20/lstm_cell_20/MatMul/ReadVariableOp2x
:sequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp:sequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp2|
<sequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1<sequential_10/lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_12r
7sequential_10/lstm_20/lstm_cell_20/mul_2/ReadVariableOp7sequential_10/lstm_20/lstm_cell_20/mul_2/ReadVariableOp2:
sequential_10/lstm_20/whilesequential_10/lstm_20/while2P
&sequential_10/lstm_21/AssignVariableOp&sequential_10/lstm_21/AssignVariableOp2T
(sequential_10/lstm_21/AssignVariableOp_1(sequential_10/lstm_21/AssignVariableOp_12L
$sequential_10/lstm_21/ReadVariableOp$sequential_10/lstm_21/ReadVariableOp2P
&sequential_10/lstm_21/ReadVariableOp_1&sequential_10/lstm_21/ReadVariableOp_12v
9sequential_10/lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp9sequential_10/lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp2t
8sequential_10/lstm_21/lstm_cell_21/MatMul/ReadVariableOp8sequential_10/lstm_21/lstm_cell_21/MatMul/ReadVariableOp2x
:sequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp:sequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp2|
<sequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1<sequential_10/lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_12r
7sequential_10/lstm_21/lstm_cell_21/mul_2/ReadVariableOp7sequential_10/lstm_21/lstm_cell_21/mul_2/ReadVariableOp2:
sequential_10/lstm_21/whilesequential_10/lstm_21/while2
Asequential_10/time_distributed_10/dense_10/BiasAdd/ReadVariableOpAsequential_10/time_distributed_10/dense_10/BiasAdd/ReadVariableOp2
@sequential_10/time_distributed_10/dense_10/MatMul/ReadVariableOp@sequential_10/time_distributed_10/dense_10/MatMul/ReadVariableOp:Q M
"
_output_shapes
:

'
_user_specified_namelstm_21_input
ÔY
Ë
while_body_57182
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_20_matmul_readvariableop_resource_0:	2ÈH
5while_lstm_cell_20_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_20_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_20_matmul_readvariableop_resource:	2ÈF
3while_lstm_cell_20_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_20_biasadd_readvariableop_resource:	È¢)while/lstm_cell_20/BiasAdd/ReadVariableOp¢(while/lstm_cell_20/MatMul/ReadVariableOp¢*while/lstm_cell_20/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_20/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_20_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02*
(while/lstm_cell_20/MatMul/ReadVariableOpÎ
while/lstm_cell_20/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/MatMulÏ
*while/lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_20_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_20/MatMul_1/ReadVariableOp·
while/lstm_cell_20/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_20/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/MatMul_1¯
while/lstm_cell_20/addAddV2#while/lstm_cell_20/MatMul:product:0%while/lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/addÈ
)while/lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_20_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_20/BiasAdd/ReadVariableOp¼
while/lstm_cell_20/BiasAddBiasAddwhile/lstm_cell_20/add:z:01while/lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/BiasAdd
"while/lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_20/split/split_dimç
while/lstm_cell_20/splitSplit+while/lstm_cell_20/split/split_dim:output:0#while/lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_20/splity
while/lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const}
while/lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_1¦
while/lstm_cell_20/MulMul!while/lstm_cell_20/split:output:0!while/lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul§
while/lstm_cell_20/Add_1AddV2while/lstm_cell_20/Mul:z:0#while/lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_1
*while/lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_20/clip_by_value/Minimum/yÛ
(while/lstm_cell_20/clip_by_value/MinimumMinimumwhile/lstm_cell_20/Add_1:z:03while/lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(while/lstm_cell_20/clip_by_value/Minimum
"while/lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_20/clip_by_value/yÓ
 while/lstm_cell_20/clip_by_valueMaximum,while/lstm_cell_20/clip_by_value/Minimum:z:0+while/lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 while/lstm_cell_20/clip_by_value}
while/lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const_2}
while/lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_3¬
while/lstm_cell_20/Mul_1Mul!while/lstm_cell_20/split:output:1#while/lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul_1©
while/lstm_cell_20/Add_2AddV2while/lstm_cell_20/Mul_1:z:0#while/lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_2¡
,while/lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_20/clip_by_value_1/Minimum/yá
*while/lstm_cell_20/clip_by_value_1/MinimumMinimumwhile/lstm_cell_20/Add_2:z:05while/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_20/clip_by_value_1/Minimum
$while/lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_20/clip_by_value_1/yÛ
"while/lstm_cell_20/clip_by_value_1Maximum.while/lstm_cell_20/clip_by_value_1/Minimum:z:0-while/lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_20/clip_by_value_1¡
while/lstm_cell_20/mul_2Mul&while/lstm_cell_20/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_2
while/lstm_cell_20/TanhTanh!while/lstm_cell_20/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_20/Tanh§
while/lstm_cell_20/mul_3Mul$while/lstm_cell_20/clip_by_value:z:0while/lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_3¢
while/lstm_cell_20/add_3AddV2while/lstm_cell_20/mul_2:z:0while/lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_20/add_3}
while/lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const_4}
while/lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_5¬
while/lstm_cell_20/Mul_4Mul!while/lstm_cell_20/split:output:3#while/lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul_4©
while/lstm_cell_20/Add_4AddV2while/lstm_cell_20/Mul_4:z:0#while/lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_4¡
,while/lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_20/clip_by_value_2/Minimum/yá
*while/lstm_cell_20/clip_by_value_2/MinimumMinimumwhile/lstm_cell_20/Add_4:z:05while/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_20/clip_by_value_2/Minimum
$while/lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_20/clip_by_value_2/yÛ
"while/lstm_cell_20/clip_by_value_2Maximum.while/lstm_cell_20/clip_by_value_2/Minimum:z:0-while/lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_20/clip_by_value_2
while/lstm_cell_20/Tanh_1Tanhwhile/lstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Tanh_1«
while/lstm_cell_20/mul_5Mul&while/lstm_cell_20/clip_by_value_2:z:0while/lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_20/mul_5:z:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identitywhile/lstm_cell_20/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_20/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_20/BiasAdd/ReadVariableOp)^while/lstm_cell_20/MatMul/ReadVariableOp+^while/lstm_cell_20/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_20_biasadd_readvariableop_resource4while_lstm_cell_20_biasadd_readvariableop_resource_0"l
3while_lstm_cell_20_matmul_1_readvariableop_resource5while_lstm_cell_20_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_20_matmul_readvariableop_resource3while_lstm_cell_20_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_20/BiasAdd/ReadVariableOp)while/lstm_cell_20/BiasAdd/ReadVariableOp2T
(while/lstm_cell_20/MatMul/ReadVariableOp(while/lstm_cell_20/MatMul/ReadVariableOp2X
*while/lstm_cell_20/MatMul_1/ReadVariableOp*while/lstm_cell_20/MatMul_1/ReadVariableOp: 
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
ô
þ
H__inference_sequential_10_layer_call_and_return_conditional_losses_57982
lstm_21_input 
lstm_21_57952:	È
lstm_21_57954:2 
lstm_21_57956:	2È
lstm_21_57958:	È
lstm_21_57960:2 
lstm_20_57963:	2È
lstm_20_57965:2 
lstm_20_57967:	2È
lstm_20_57969:	È
lstm_20_57971:2+
time_distributed_10_57974:2'
time_distributed_10_57976:
identity¢lstm_20/StatefulPartitionedCall¢lstm_21/StatefulPartitionedCall¢+time_distributed_10/StatefulPartitionedCall¿
lstm_21/StatefulPartitionedCallStatefulPartitionedCalllstm_21_inputlstm_21_57952lstm_21_57954lstm_21_57956lstm_21_57958lstm_21_57960*
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
GPU 2J 8 *K
fFRD
B__inference_lstm_21_layer_call_and_return_conditional_losses_577832!
lstm_21/StatefulPartitionedCallÚ
lstm_20/StatefulPartitionedCallStatefulPartitionedCall(lstm_21/StatefulPartitionedCall:output:0lstm_20_57963lstm_20_57965lstm_20_57967lstm_20_57969lstm_20_57971*
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
GPU 2J 8 *K
fFRD
B__inference_lstm_20_layer_call_and_return_conditional_losses_575752!
lstm_20/StatefulPartitionedCallå
+time_distributed_10/StatefulPartitionedCallStatefulPartitionedCall(lstm_20/StatefulPartitionedCall:output:0time_distributed_10_57974time_distributed_10_57976*
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
GPU 2J 8 *W
fRRP
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_573732-
+time_distributed_10/StatefulPartitionedCall
!time_distributed_10/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2#
!time_distributed_10/Reshape/shapeÄ
time_distributed_10/ReshapeReshape(lstm_20/StatefulPartitionedCall:output:0*time_distributed_10/Reshape/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_10/Reshape
IdentityIdentity4time_distributed_10/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

IdentityÀ
NoOpNoOp ^lstm_20/StatefulPartitionedCall ^lstm_21/StatefulPartitionedCall,^time_distributed_10/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 2B
lstm_20/StatefulPartitionedCalllstm_20/StatefulPartitionedCall2B
lstm_21/StatefulPartitionedCalllstm_21/StatefulPartitionedCall2Z
+time_distributed_10/StatefulPartitionedCall+time_distributed_10/StatefulPartitionedCall:Q M
"
_output_shapes
:

'
_user_specified_namelstm_21_input
±0
Ô
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_55621

inputs
states:2
states_1:21
matmul_readvariableop_resource:	È5
"matmul_1_readvariableop_1_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp¢MatMul_1/ReadVariableOp_1¢mul_2/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:2*
dtype02
MatMul_1/ReadVariableOp
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOp_1
MatMul_1MatMulMatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2Ì
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
¨
¼
while_cond_59227
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_59227___redundant_placeholder03
/while_while_cond_59227___redundant_placeholder13
/while_while_cond_59227___redundant_placeholder23
/while_while_cond_59227___redundant_placeholder3
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
È
õ
,__inference_lstm_cell_20_layer_call_fn_61088

inputs
states_0
states_1
unknown:	2È
	unknown_0:	2È
	unknown_1:	È
identity

identity_1

identity_2¢StatefulPartitionedCall§
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_561722
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
÷,

G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_61014

inputs
states_0
states_11
matmul_readvariableop_resource:	2È3
 matmul_1_readvariableop_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2
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
±0
Ô
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_56076

inputs
states:2
states_1:21
matmul_readvariableop_resource:	2È5
"matmul_1_readvariableop_1_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp¢MatMul_1/ReadVariableOp_1¢mul_2/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:2*
dtype02
MatMul_1/ReadVariableOp
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOp_1
MatMul_1MatMulMatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2Ì
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
º
 
3__inference_time_distributed_10_layer_call_fn_60422

inputs
unknown:2
	unknown_0:
identity¢StatefulPartitionedCall
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *W
fRRP
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_568102
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2: : 22
StatefulPartitionedCallStatefulPartitionedCall:\ X
4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
æ
õ
,__inference_lstm_cell_21_layer_call_fn_60777

inputs
states_0
states_1
unknown:	È
	unknown_0:	2È
	unknown_1:	È
identity

identity_1

identity_2¢StatefulPartitionedCall
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_607642
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
ùn
´
B__inference_lstm_21_layer_call_and_return_conditional_losses_59511

inputs>
+lstm_cell_21_matmul_readvariableop_resource:	È?
-lstm_cell_21_matmul_1_readvariableop_resource:2B
/lstm_cell_21_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_21_biasadd_readvariableop_resource:	È<
*lstm_cell_21_mul_2_readvariableop_resource:2
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_21/BiasAdd/ReadVariableOp¢"lstm_cell_21/MatMul/ReadVariableOp¢$lstm_cell_21/MatMul_1/ReadVariableOp¢&lstm_cell_21/MatMul_1/ReadVariableOp_1¢!lstm_cell_21/mul_2/ReadVariableOp¢whileu
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_21/MatMul/ReadVariableOpReadVariableOp+lstm_cell_21_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02$
"lstm_cell_21/MatMul/ReadVariableOp¤
lstm_cell_21/MatMulMatMulstrided_slice_1:output:0*lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/MatMulº
$lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$lstm_cell_21/MatMul_1/ReadVariableOpÁ
&lstm_cell_21/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_21_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_21/MatMul_1/ReadVariableOp_1À
lstm_cell_21/MatMul_1MatMul,lstm_cell_21/MatMul_1/ReadVariableOp:value:0.lstm_cell_21/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/MatMul_1
lstm_cell_21/addAddV2lstm_cell_21/MatMul:product:0lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_cell_21/add´
#lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_21_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_21/BiasAdd/ReadVariableOp¤
lstm_cell_21/BiasAddBiasAddlstm_cell_21/add:z:0+lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/BiasAdd~
lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_21/split/split_dimÏ
lstm_cell_21/splitSplit%lstm_cell_21/split/split_dim:output:0lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_21/splitm
lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Constq
lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_1
lstm_cell_21/MulMullstm_cell_21/split:output:0lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul
lstm_cell_21/Add_1AddV2lstm_cell_21/Mul:z:0lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_1
$lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_21/clip_by_value/Minimum/yÃ
"lstm_cell_21/clip_by_value/MinimumMinimumlstm_cell_21/Add_1:z:0-lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22$
"lstm_cell_21/clip_by_value/Minimum
lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_21/clip_by_value/y»
lstm_cell_21/clip_by_valueMaximum&lstm_cell_21/clip_by_value/Minimum:z:0%lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_valueq
lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Const_2q
lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_3
lstm_cell_21/Mul_1Mullstm_cell_21/split:output:1lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul_1
lstm_cell_21/Add_2AddV2lstm_cell_21/Mul_1:z:0lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_2
&lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_21/clip_by_value_1/Minimum/yÉ
$lstm_cell_21/clip_by_value_1/MinimumMinimumlstm_cell_21/Add_2:z:0/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_21/clip_by_value_1/Minimum
lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_21/clip_by_value_1/yÃ
lstm_cell_21/clip_by_value_1Maximum(lstm_cell_21/clip_by_value_1/Minimum:z:0'lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_value_1±
!lstm_cell_21/mul_2/ReadVariableOpReadVariableOp*lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02#
!lstm_cell_21/mul_2/ReadVariableOp¥
lstm_cell_21/mul_2Mul lstm_cell_21/clip_by_value_1:z:0)lstm_cell_21/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_2t
lstm_cell_21/TanhTanhlstm_cell_21/split:output:2*
T0*
_output_shapes

:22
lstm_cell_21/Tanh
lstm_cell_21/mul_3Mullstm_cell_21/clip_by_value:z:0lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_3
lstm_cell_21/add_3AddV2lstm_cell_21/mul_2:z:0lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_21/add_3q
lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Const_4q
lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_5
lstm_cell_21/Mul_4Mullstm_cell_21/split:output:3lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul_4
lstm_cell_21/Add_4AddV2lstm_cell_21/Mul_4:z:0lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_4
&lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_21/clip_by_value_2/Minimum/yÉ
$lstm_cell_21/clip_by_value_2/MinimumMinimumlstm_cell_21/Add_4:z:0/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_21/clip_by_value_2/Minimum
lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_21/clip_by_value_2/yÃ
lstm_cell_21/clip_by_value_2Maximum(lstm_cell_21/clip_by_value_2/Minimum:z:0'lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_value_2s
lstm_cell_21/Tanh_1Tanhlstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_21/Tanh_1
lstm_cell_21/mul_5Mul lstm_cell_21/clip_by_value_2:z:0lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
time
ReadVariableOpReadVariableOp-lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterõ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_21_matmul_readvariableop_resource/lstm_cell_21_matmul_1_readvariableop_1_resource,lstm_cell_21_biasadd_readvariableop_resource*
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
while_body_59406*
condR
while_cond_59405*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm
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
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_21_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_21/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_21_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_21/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_21/BiasAdd/ReadVariableOp#^lstm_cell_21/MatMul/ReadVariableOp%^lstm_cell_21/MatMul_1/ReadVariableOp'^lstm_cell_21/MatMul_1/ReadVariableOp_1"^lstm_cell_21/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_21/BiasAdd/ReadVariableOp#lstm_cell_21/BiasAdd/ReadVariableOp2H
"lstm_cell_21/MatMul/ReadVariableOp"lstm_cell_21/MatMul/ReadVariableOp2L
$lstm_cell_21/MatMul_1/ReadVariableOp$lstm_cell_21/MatMul_1/ReadVariableOp2P
&lstm_cell_21/MatMul_1/ReadVariableOp_1&lstm_cell_21/MatMul_1/ReadVariableOp_12F
!lstm_cell_21/mul_2/ReadVariableOp!lstm_cell_21/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:

 
_user_specified_nameinputs
Þ
î
'__inference_lstm_20_layer_call_fn_60343

inputs
unknown:	2È
	unknown_0:2
	unknown_1:	2È
	unknown_2:	È
	unknown_3:2
identity¢StatefulPartitionedCall
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
GPU 2J 8 *K
fFRD
B__inference_lstm_20_layer_call_and_return_conditional_losses_575752
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
ùn
´
B__inference_lstm_21_layer_call_and_return_conditional_losses_59333

inputs>
+lstm_cell_21_matmul_readvariableop_resource:	È?
-lstm_cell_21_matmul_1_readvariableop_resource:2B
/lstm_cell_21_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_21_biasadd_readvariableop_resource:	È<
*lstm_cell_21_mul_2_readvariableop_resource:2
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_21/BiasAdd/ReadVariableOp¢"lstm_cell_21/MatMul/ReadVariableOp¢$lstm_cell_21/MatMul_1/ReadVariableOp¢&lstm_cell_21/MatMul_1/ReadVariableOp_1¢!lstm_cell_21/mul_2/ReadVariableOp¢whileu
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_21/MatMul/ReadVariableOpReadVariableOp+lstm_cell_21_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02$
"lstm_cell_21/MatMul/ReadVariableOp¤
lstm_cell_21/MatMulMatMulstrided_slice_1:output:0*lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/MatMulº
$lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$lstm_cell_21/MatMul_1/ReadVariableOpÁ
&lstm_cell_21/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_21_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_21/MatMul_1/ReadVariableOp_1À
lstm_cell_21/MatMul_1MatMul,lstm_cell_21/MatMul_1/ReadVariableOp:value:0.lstm_cell_21/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/MatMul_1
lstm_cell_21/addAddV2lstm_cell_21/MatMul:product:0lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_cell_21/add´
#lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_21_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_21/BiasAdd/ReadVariableOp¤
lstm_cell_21/BiasAddBiasAddlstm_cell_21/add:z:0+lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/BiasAdd~
lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_21/split/split_dimÏ
lstm_cell_21/splitSplit%lstm_cell_21/split/split_dim:output:0lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_21/splitm
lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Constq
lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_1
lstm_cell_21/MulMullstm_cell_21/split:output:0lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul
lstm_cell_21/Add_1AddV2lstm_cell_21/Mul:z:0lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_1
$lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_21/clip_by_value/Minimum/yÃ
"lstm_cell_21/clip_by_value/MinimumMinimumlstm_cell_21/Add_1:z:0-lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22$
"lstm_cell_21/clip_by_value/Minimum
lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_21/clip_by_value/y»
lstm_cell_21/clip_by_valueMaximum&lstm_cell_21/clip_by_value/Minimum:z:0%lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_valueq
lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Const_2q
lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_3
lstm_cell_21/Mul_1Mullstm_cell_21/split:output:1lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul_1
lstm_cell_21/Add_2AddV2lstm_cell_21/Mul_1:z:0lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_2
&lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_21/clip_by_value_1/Minimum/yÉ
$lstm_cell_21/clip_by_value_1/MinimumMinimumlstm_cell_21/Add_2:z:0/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_21/clip_by_value_1/Minimum
lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_21/clip_by_value_1/yÃ
lstm_cell_21/clip_by_value_1Maximum(lstm_cell_21/clip_by_value_1/Minimum:z:0'lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_value_1±
!lstm_cell_21/mul_2/ReadVariableOpReadVariableOp*lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02#
!lstm_cell_21/mul_2/ReadVariableOp¥
lstm_cell_21/mul_2Mul lstm_cell_21/clip_by_value_1:z:0)lstm_cell_21/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_2t
lstm_cell_21/TanhTanhlstm_cell_21/split:output:2*
T0*
_output_shapes

:22
lstm_cell_21/Tanh
lstm_cell_21/mul_3Mullstm_cell_21/clip_by_value:z:0lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_3
lstm_cell_21/add_3AddV2lstm_cell_21/mul_2:z:0lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_21/add_3q
lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Const_4q
lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_5
lstm_cell_21/Mul_4Mullstm_cell_21/split:output:3lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul_4
lstm_cell_21/Add_4AddV2lstm_cell_21/Mul_4:z:0lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_4
&lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_21/clip_by_value_2/Minimum/yÉ
$lstm_cell_21/clip_by_value_2/MinimumMinimumlstm_cell_21/Add_4:z:0/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_21/clip_by_value_2/Minimum
lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_21/clip_by_value_2/yÃ
lstm_cell_21/clip_by_value_2Maximum(lstm_cell_21/clip_by_value_2/Minimum:z:0'lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_value_2s
lstm_cell_21/Tanh_1Tanhlstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_21/Tanh_1
lstm_cell_21/mul_5Mul lstm_cell_21/clip_by_value_2:z:0lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
time
ReadVariableOpReadVariableOp-lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterõ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_21_matmul_readvariableop_resource/lstm_cell_21_matmul_1_readvariableop_1_resource,lstm_cell_21_biasadd_readvariableop_resource*
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
while_body_59228*
condR
while_cond_59227*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm
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
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_21_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_21/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_21_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_21/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_21/BiasAdd/ReadVariableOp#^lstm_cell_21/MatMul/ReadVariableOp%^lstm_cell_21/MatMul_1/ReadVariableOp'^lstm_cell_21/MatMul_1/ReadVariableOp_1"^lstm_cell_21/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_21/BiasAdd/ReadVariableOp#lstm_cell_21/BiasAdd/ReadVariableOp2H
"lstm_cell_21/MatMul/ReadVariableOp"lstm_cell_21/MatMul/ReadVariableOp2L
$lstm_cell_21/MatMul_1/ReadVariableOp$lstm_cell_21/MatMul_1/ReadVariableOp2P
&lstm_cell_21/MatMul_1/ReadVariableOp_1&lstm_cell_21/MatMul_1/ReadVariableOp_12F
!lstm_cell_21/mul_2/ReadVariableOp!lstm_cell_21/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:

 
_user_specified_nameinputs
ÔY
Ë
while_body_58872
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_21_matmul_readvariableop_resource_0:	ÈH
5while_lstm_cell_21_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_21_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_21_matmul_readvariableop_resource:	ÈF
3while_lstm_cell_21_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_21_biasadd_readvariableop_resource:	È¢)while/lstm_cell_21/BiasAdd/ReadVariableOp¢(while/lstm_cell_21/MatMul/ReadVariableOp¢*while/lstm_cell_21/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_21/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_21_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02*
(while/lstm_cell_21/MatMul/ReadVariableOpÎ
while/lstm_cell_21/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/MatMulÏ
*while/lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_21_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_21/MatMul_1/ReadVariableOp·
while/lstm_cell_21/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_21/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/MatMul_1¯
while/lstm_cell_21/addAddV2#while/lstm_cell_21/MatMul:product:0%while/lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/addÈ
)while/lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_21_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_21/BiasAdd/ReadVariableOp¼
while/lstm_cell_21/BiasAddBiasAddwhile/lstm_cell_21/add:z:01while/lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/BiasAdd
"while/lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_21/split/split_dimç
while/lstm_cell_21/splitSplit+while/lstm_cell_21/split/split_dim:output:0#while/lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_21/splity
while/lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const}
while/lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_1¦
while/lstm_cell_21/MulMul!while/lstm_cell_21/split:output:0!while/lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul§
while/lstm_cell_21/Add_1AddV2while/lstm_cell_21/Mul:z:0#while/lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_1
*while/lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_21/clip_by_value/Minimum/yÛ
(while/lstm_cell_21/clip_by_value/MinimumMinimumwhile/lstm_cell_21/Add_1:z:03while/lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(while/lstm_cell_21/clip_by_value/Minimum
"while/lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_21/clip_by_value/yÓ
 while/lstm_cell_21/clip_by_valueMaximum,while/lstm_cell_21/clip_by_value/Minimum:z:0+while/lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 while/lstm_cell_21/clip_by_value}
while/lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const_2}
while/lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_3¬
while/lstm_cell_21/Mul_1Mul!while/lstm_cell_21/split:output:1#while/lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul_1©
while/lstm_cell_21/Add_2AddV2while/lstm_cell_21/Mul_1:z:0#while/lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_2¡
,while/lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_21/clip_by_value_1/Minimum/yá
*while/lstm_cell_21/clip_by_value_1/MinimumMinimumwhile/lstm_cell_21/Add_2:z:05while/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_21/clip_by_value_1/Minimum
$while/lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_21/clip_by_value_1/yÛ
"while/lstm_cell_21/clip_by_value_1Maximum.while/lstm_cell_21/clip_by_value_1/Minimum:z:0-while/lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_21/clip_by_value_1¡
while/lstm_cell_21/mul_2Mul&while/lstm_cell_21/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_2
while/lstm_cell_21/TanhTanh!while/lstm_cell_21/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_21/Tanh§
while/lstm_cell_21/mul_3Mul$while/lstm_cell_21/clip_by_value:z:0while/lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_3¢
while/lstm_cell_21/add_3AddV2while/lstm_cell_21/mul_2:z:0while/lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_21/add_3}
while/lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const_4}
while/lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_5¬
while/lstm_cell_21/Mul_4Mul!while/lstm_cell_21/split:output:3#while/lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul_4©
while/lstm_cell_21/Add_4AddV2while/lstm_cell_21/Mul_4:z:0#while/lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_4¡
,while/lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_21/clip_by_value_2/Minimum/yá
*while/lstm_cell_21/clip_by_value_2/MinimumMinimumwhile/lstm_cell_21/Add_4:z:05while/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_21/clip_by_value_2/Minimum
$while/lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_21/clip_by_value_2/yÛ
"while/lstm_cell_21/clip_by_value_2Maximum.while/lstm_cell_21/clip_by_value_2/Minimum:z:0-while/lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_21/clip_by_value_2
while/lstm_cell_21/Tanh_1Tanhwhile/lstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Tanh_1«
while/lstm_cell_21/mul_5Mul&while/lstm_cell_21/clip_by_value_2:z:0while/lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_21/mul_5:z:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identitywhile/lstm_cell_21/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_21/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_21/BiasAdd/ReadVariableOp)^while/lstm_cell_21/MatMul/ReadVariableOp+^while/lstm_cell_21/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_21_biasadd_readvariableop_resource4while_lstm_cell_21_biasadd_readvariableop_resource_0"l
3while_lstm_cell_21_matmul_1_readvariableop_resource5while_lstm_cell_21_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_21_matmul_readvariableop_resource3while_lstm_cell_21_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_21/BiasAdd/ReadVariableOp)while/lstm_cell_21/BiasAdd/ReadVariableOp2T
(while/lstm_cell_21/MatMul/ReadVariableOp(while/lstm_cell_21/MatMul/ReadVariableOp2X
*while/lstm_cell_21/MatMul_1/ReadVariableOp*while/lstm_cell_21/MatMul_1/ReadVariableOp: 
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
ÔY
Ë
while_body_59228
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_21_matmul_readvariableop_resource_0:	ÈH
5while_lstm_cell_21_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_21_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_21_matmul_readvariableop_resource:	ÈF
3while_lstm_cell_21_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_21_biasadd_readvariableop_resource:	È¢)while/lstm_cell_21/BiasAdd/ReadVariableOp¢(while/lstm_cell_21/MatMul/ReadVariableOp¢*while/lstm_cell_21/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_21/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_21_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02*
(while/lstm_cell_21/MatMul/ReadVariableOpÎ
while/lstm_cell_21/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/MatMulÏ
*while/lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_21_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_21/MatMul_1/ReadVariableOp·
while/lstm_cell_21/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_21/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/MatMul_1¯
while/lstm_cell_21/addAddV2#while/lstm_cell_21/MatMul:product:0%while/lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/addÈ
)while/lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_21_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_21/BiasAdd/ReadVariableOp¼
while/lstm_cell_21/BiasAddBiasAddwhile/lstm_cell_21/add:z:01while/lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/BiasAdd
"while/lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_21/split/split_dimç
while/lstm_cell_21/splitSplit+while/lstm_cell_21/split/split_dim:output:0#while/lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_21/splity
while/lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const}
while/lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_1¦
while/lstm_cell_21/MulMul!while/lstm_cell_21/split:output:0!while/lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul§
while/lstm_cell_21/Add_1AddV2while/lstm_cell_21/Mul:z:0#while/lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_1
*while/lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_21/clip_by_value/Minimum/yÛ
(while/lstm_cell_21/clip_by_value/MinimumMinimumwhile/lstm_cell_21/Add_1:z:03while/lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(while/lstm_cell_21/clip_by_value/Minimum
"while/lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_21/clip_by_value/yÓ
 while/lstm_cell_21/clip_by_valueMaximum,while/lstm_cell_21/clip_by_value/Minimum:z:0+while/lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 while/lstm_cell_21/clip_by_value}
while/lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const_2}
while/lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_3¬
while/lstm_cell_21/Mul_1Mul!while/lstm_cell_21/split:output:1#while/lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul_1©
while/lstm_cell_21/Add_2AddV2while/lstm_cell_21/Mul_1:z:0#while/lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_2¡
,while/lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_21/clip_by_value_1/Minimum/yá
*while/lstm_cell_21/clip_by_value_1/MinimumMinimumwhile/lstm_cell_21/Add_2:z:05while/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_21/clip_by_value_1/Minimum
$while/lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_21/clip_by_value_1/yÛ
"while/lstm_cell_21/clip_by_value_1Maximum.while/lstm_cell_21/clip_by_value_1/Minimum:z:0-while/lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_21/clip_by_value_1¡
while/lstm_cell_21/mul_2Mul&while/lstm_cell_21/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_2
while/lstm_cell_21/TanhTanh!while/lstm_cell_21/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_21/Tanh§
while/lstm_cell_21/mul_3Mul$while/lstm_cell_21/clip_by_value:z:0while/lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_3¢
while/lstm_cell_21/add_3AddV2while/lstm_cell_21/mul_2:z:0while/lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_21/add_3}
while/lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const_4}
while/lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_5¬
while/lstm_cell_21/Mul_4Mul!while/lstm_cell_21/split:output:3#while/lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul_4©
while/lstm_cell_21/Add_4AddV2while/lstm_cell_21/Mul_4:z:0#while/lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_4¡
,while/lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_21/clip_by_value_2/Minimum/yá
*while/lstm_cell_21/clip_by_value_2/MinimumMinimumwhile/lstm_cell_21/Add_4:z:05while/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_21/clip_by_value_2/Minimum
$while/lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_21/clip_by_value_2/yÛ
"while/lstm_cell_21/clip_by_value_2Maximum.while/lstm_cell_21/clip_by_value_2/Minimum:z:0-while/lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_21/clip_by_value_2
while/lstm_cell_21/Tanh_1Tanhwhile/lstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Tanh_1«
while/lstm_cell_21/mul_5Mul&while/lstm_cell_21/clip_by_value_2:z:0while/lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_21/mul_5:z:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identitywhile/lstm_cell_21/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_21/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_21/BiasAdd/ReadVariableOp)^while/lstm_cell_21/MatMul/ReadVariableOp+^while/lstm_cell_21/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_21_biasadd_readvariableop_resource4while_lstm_cell_21_biasadd_readvariableop_resource_0"l
3while_lstm_cell_21_matmul_1_readvariableop_resource5while_lstm_cell_21_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_21_matmul_readvariableop_resource3while_lstm_cell_21_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_21/BiasAdd/ReadVariableOp)while/lstm_cell_21/BiasAdd/ReadVariableOp2T
(while/lstm_cell_21/MatMul/ReadVariableOp(while/lstm_cell_21/MatMul/ReadVariableOp2X
*while/lstm_cell_21/MatMul_1/ReadVariableOp*while/lstm_cell_21/MatMul_1/ReadVariableOp: 
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
ÔY
Ë
while_body_59050
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_21_matmul_readvariableop_resource_0:	ÈH
5while_lstm_cell_21_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_21_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_21_matmul_readvariableop_resource:	ÈF
3while_lstm_cell_21_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_21_biasadd_readvariableop_resource:	È¢)while/lstm_cell_21/BiasAdd/ReadVariableOp¢(while/lstm_cell_21/MatMul/ReadVariableOp¢*while/lstm_cell_21/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_21/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_21_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02*
(while/lstm_cell_21/MatMul/ReadVariableOpÎ
while/lstm_cell_21/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/MatMulÏ
*while/lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_21_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_21/MatMul_1/ReadVariableOp·
while/lstm_cell_21/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_21/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/MatMul_1¯
while/lstm_cell_21/addAddV2#while/lstm_cell_21/MatMul:product:0%while/lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/addÈ
)while/lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_21_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_21/BiasAdd/ReadVariableOp¼
while/lstm_cell_21/BiasAddBiasAddwhile/lstm_cell_21/add:z:01while/lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/BiasAdd
"while/lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_21/split/split_dimç
while/lstm_cell_21/splitSplit+while/lstm_cell_21/split/split_dim:output:0#while/lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_21/splity
while/lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const}
while/lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_1¦
while/lstm_cell_21/MulMul!while/lstm_cell_21/split:output:0!while/lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul§
while/lstm_cell_21/Add_1AddV2while/lstm_cell_21/Mul:z:0#while/lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_1
*while/lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_21/clip_by_value/Minimum/yÛ
(while/lstm_cell_21/clip_by_value/MinimumMinimumwhile/lstm_cell_21/Add_1:z:03while/lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(while/lstm_cell_21/clip_by_value/Minimum
"while/lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_21/clip_by_value/yÓ
 while/lstm_cell_21/clip_by_valueMaximum,while/lstm_cell_21/clip_by_value/Minimum:z:0+while/lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 while/lstm_cell_21/clip_by_value}
while/lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const_2}
while/lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_3¬
while/lstm_cell_21/Mul_1Mul!while/lstm_cell_21/split:output:1#while/lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul_1©
while/lstm_cell_21/Add_2AddV2while/lstm_cell_21/Mul_1:z:0#while/lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_2¡
,while/lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_21/clip_by_value_1/Minimum/yá
*while/lstm_cell_21/clip_by_value_1/MinimumMinimumwhile/lstm_cell_21/Add_2:z:05while/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_21/clip_by_value_1/Minimum
$while/lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_21/clip_by_value_1/yÛ
"while/lstm_cell_21/clip_by_value_1Maximum.while/lstm_cell_21/clip_by_value_1/Minimum:z:0-while/lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_21/clip_by_value_1¡
while/lstm_cell_21/mul_2Mul&while/lstm_cell_21/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_2
while/lstm_cell_21/TanhTanh!while/lstm_cell_21/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_21/Tanh§
while/lstm_cell_21/mul_3Mul$while/lstm_cell_21/clip_by_value:z:0while/lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_3¢
while/lstm_cell_21/add_3AddV2while/lstm_cell_21/mul_2:z:0while/lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_21/add_3}
while/lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const_4}
while/lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_5¬
while/lstm_cell_21/Mul_4Mul!while/lstm_cell_21/split:output:3#while/lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul_4©
while/lstm_cell_21/Add_4AddV2while/lstm_cell_21/Mul_4:z:0#while/lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_4¡
,while/lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_21/clip_by_value_2/Minimum/yá
*while/lstm_cell_21/clip_by_value_2/MinimumMinimumwhile/lstm_cell_21/Add_4:z:05while/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_21/clip_by_value_2/Minimum
$while/lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_21/clip_by_value_2/yÛ
"while/lstm_cell_21/clip_by_value_2Maximum.while/lstm_cell_21/clip_by_value_2/Minimum:z:0-while/lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_21/clip_by_value_2
while/lstm_cell_21/Tanh_1Tanhwhile/lstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Tanh_1«
while/lstm_cell_21/mul_5Mul&while/lstm_cell_21/clip_by_value_2:z:0while/lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_21/mul_5:z:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identitywhile/lstm_cell_21/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_21/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_21/BiasAdd/ReadVariableOp)^while/lstm_cell_21/MatMul/ReadVariableOp+^while/lstm_cell_21/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_21_biasadd_readvariableop_resource4while_lstm_cell_21_biasadd_readvariableop_resource_0"l
3while_lstm_cell_21_matmul_1_readvariableop_resource5while_lstm_cell_21_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_21_matmul_readvariableop_resource3while_lstm_cell_21_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_21/BiasAdd/ReadVariableOp)while/lstm_cell_21/BiasAdd/ReadVariableOp2T
(while/lstm_cell_21/MatMul/ReadVariableOp(while/lstm_cell_21/MatMul/ReadVariableOp2X
*while/lstm_cell_21/MatMul_1/ReadVariableOp*while/lstm_cell_21/MatMul_1/ReadVariableOp: 
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

»
-__inference_sequential_10_layer_call_fn_58799

inputs
unknown:	È
	unknown_0:2
	unknown_1:	2È
	unknown_2:	È
	unknown_3:2
	unknown_4:	2È
	unknown_5:2
	unknown_6:	2È
	unknown_7:	È
	unknown_8:2
	unknown_9:2

unknown_10:
identity¢StatefulPartitionedCallò
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
GPU 2J 8 *Q
fLRJ
H__inference_sequential_10_layer_call_and_return_conditional_losses_578602
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
÷,

G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60559

inputs
states_0
states_11
matmul_readvariableop_resource:	È3
 matmul_1_readvariableop_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2
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
i
Ë

lstm_21_while_body_58086,
(lstm_21_while_lstm_21_while_loop_counter2
.lstm_21_while_lstm_21_while_maximum_iterations
lstm_21_while_placeholder
lstm_21_while_placeholder_1
lstm_21_while_placeholder_2
lstm_21_while_placeholder_3)
%lstm_21_while_lstm_21_strided_slice_0g
clstm_21_while_tensorarrayv2read_tensorlistgetitem_lstm_21_tensorarrayunstack_tensorlistfromtensor_0N
;lstm_21_while_lstm_cell_21_matmul_readvariableop_resource_0:	ÈP
=lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource_0:	2ÈK
<lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource_0:	È
lstm_21_while_identity
lstm_21_while_identity_1
lstm_21_while_identity_2
lstm_21_while_identity_3
lstm_21_while_identity_4
lstm_21_while_identity_5'
#lstm_21_while_lstm_21_strided_slicee
alstm_21_while_tensorarrayv2read_tensorlistgetitem_lstm_21_tensorarrayunstack_tensorlistfromtensorL
9lstm_21_while_lstm_cell_21_matmul_readvariableop_resource:	ÈN
;lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource:	2ÈI
:lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource:	È¢1lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp¢0lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp¢2lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOpÓ
?lstm_21/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      2A
?lstm_21/while/TensorArrayV2Read/TensorListGetItem/element_shapeú
1lstm_21/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemclstm_21_while_tensorarrayv2read_tensorlistgetitem_lstm_21_tensorarrayunstack_tensorlistfromtensor_0lstm_21_while_placeholderHlstm_21/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype023
1lstm_21/while/TensorArrayV2Read/TensorListGetItemá
0lstm_21/while/lstm_cell_21/MatMul/ReadVariableOpReadVariableOp;lstm_21_while_lstm_cell_21_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype022
0lstm_21/while/lstm_cell_21/MatMul/ReadVariableOpî
!lstm_21/while/lstm_cell_21/MatMulMatMul8lstm_21/while/TensorArrayV2Read/TensorListGetItem:item:08lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2#
!lstm_21/while/lstm_cell_21/MatMulç
2lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp=lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype024
2lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp×
#lstm_21/while/lstm_cell_21/MatMul_1MatMullstm_21_while_placeholder_2:lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2%
#lstm_21/while/lstm_cell_21/MatMul_1Ï
lstm_21/while/lstm_cell_21/addAddV2+lstm_21/while/lstm_cell_21/MatMul:product:0-lstm_21/while/lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2 
lstm_21/while/lstm_cell_21/addà
1lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp<lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype023
1lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOpÜ
"lstm_21/while/lstm_cell_21/BiasAddBiasAdd"lstm_21/while/lstm_cell_21/add:z:09lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2$
"lstm_21/while/lstm_cell_21/BiasAdd
*lstm_21/while/lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2,
*lstm_21/while/lstm_cell_21/split/split_dim
 lstm_21/while/lstm_cell_21/splitSplit3lstm_21/while/lstm_cell_21/split/split_dim:output:0+lstm_21/while/lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2"
 lstm_21/while/lstm_cell_21/split
 lstm_21/while/lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2"
 lstm_21/while/lstm_cell_21/Const
"lstm_21/while/lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_21/while/lstm_cell_21/Const_1Æ
lstm_21/while/lstm_cell_21/MulMul)lstm_21/while/lstm_cell_21/split:output:0)lstm_21/while/lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22 
lstm_21/while/lstm_cell_21/MulÇ
 lstm_21/while/lstm_cell_21/Add_1AddV2"lstm_21/while/lstm_cell_21/Mul:z:0+lstm_21/while/lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/Add_1­
2lstm_21/while/lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?24
2lstm_21/while/lstm_cell_21/clip_by_value/Minimum/yû
0lstm_21/while/lstm_cell_21/clip_by_value/MinimumMinimum$lstm_21/while/lstm_cell_21/Add_1:z:0;lstm_21/while/lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222
0lstm_21/while/lstm_cell_21/clip_by_value/Minimum
*lstm_21/while/lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_21/while/lstm_cell_21/clip_by_value/yó
(lstm_21/while/lstm_cell_21/clip_by_valueMaximum4lstm_21/while/lstm_cell_21/clip_by_value/Minimum:z:03lstm_21/while/lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22*
(lstm_21/while/lstm_cell_21/clip_by_value
"lstm_21/while/lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_21/while/lstm_cell_21/Const_2
"lstm_21/while/lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_21/while/lstm_cell_21/Const_3Ì
 lstm_21/while/lstm_cell_21/Mul_1Mul)lstm_21/while/lstm_cell_21/split:output:1+lstm_21/while/lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/Mul_1É
 lstm_21/while/lstm_cell_21/Add_2AddV2$lstm_21/while/lstm_cell_21/Mul_1:z:0+lstm_21/while/lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/Add_2±
4lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum/y
2lstm_21/while/lstm_cell_21/clip_by_value_1/MinimumMinimum$lstm_21/while/lstm_cell_21/Add_2:z:0=lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:224
2lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum¡
,lstm_21/while/lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_21/while/lstm_cell_21/clip_by_value_1/yû
*lstm_21/while/lstm_cell_21/clip_by_value_1Maximum6lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum:z:05lstm_21/while/lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22,
*lstm_21/while/lstm_cell_21/clip_by_value_1Á
 lstm_21/while/lstm_cell_21/mul_2Mul.lstm_21/while/lstm_cell_21/clip_by_value_1:z:0lstm_21_while_placeholder_3*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/mul_2
lstm_21/while/lstm_cell_21/TanhTanh)lstm_21/while/lstm_cell_21/split:output:2*
T0*
_output_shapes

:22!
lstm_21/while/lstm_cell_21/TanhÇ
 lstm_21/while/lstm_cell_21/mul_3Mul,lstm_21/while/lstm_cell_21/clip_by_value:z:0#lstm_21/while/lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/mul_3Â
 lstm_21/while/lstm_cell_21/add_3AddV2$lstm_21/while/lstm_cell_21/mul_2:z:0$lstm_21/while/lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/add_3
"lstm_21/while/lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_21/while/lstm_cell_21/Const_4
"lstm_21/while/lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_21/while/lstm_cell_21/Const_5Ì
 lstm_21/while/lstm_cell_21/Mul_4Mul)lstm_21/while/lstm_cell_21/split:output:3+lstm_21/while/lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/Mul_4É
 lstm_21/while/lstm_cell_21/Add_4AddV2$lstm_21/while/lstm_cell_21/Mul_4:z:0+lstm_21/while/lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/Add_4±
4lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum/y
2lstm_21/while/lstm_cell_21/clip_by_value_2/MinimumMinimum$lstm_21/while/lstm_cell_21/Add_4:z:0=lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:224
2lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum¡
,lstm_21/while/lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_21/while/lstm_cell_21/clip_by_value_2/yû
*lstm_21/while/lstm_cell_21/clip_by_value_2Maximum6lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum:z:05lstm_21/while/lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22,
*lstm_21/while/lstm_cell_21/clip_by_value_2
!lstm_21/while/lstm_cell_21/Tanh_1Tanh$lstm_21/while/lstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22#
!lstm_21/while/lstm_cell_21/Tanh_1Ë
 lstm_21/while/lstm_cell_21/mul_5Mul.lstm_21/while/lstm_cell_21/clip_by_value_2:z:0%lstm_21/while/lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/mul_5
2lstm_21/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_21_while_placeholder_1lstm_21_while_placeholder$lstm_21/while/lstm_cell_21/mul_5:z:0*
_output_shapes
: *
element_dtype024
2lstm_21/while/TensorArrayV2Write/TensorListSetIteml
lstm_21/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_21/while/add/y
lstm_21/while/addAddV2lstm_21_while_placeholderlstm_21/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_21/while/addp
lstm_21/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_21/while/add_1/y
lstm_21/while/add_1AddV2(lstm_21_while_lstm_21_while_loop_counterlstm_21/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_21/while/add_1
lstm_21/while/IdentityIdentitylstm_21/while/add_1:z:0^lstm_21/while/NoOp*
T0*
_output_shapes
: 2
lstm_21/while/Identity¦
lstm_21/while/Identity_1Identity.lstm_21_while_lstm_21_while_maximum_iterations^lstm_21/while/NoOp*
T0*
_output_shapes
: 2
lstm_21/while/Identity_1
lstm_21/while/Identity_2Identitylstm_21/while/add:z:0^lstm_21/while/NoOp*
T0*
_output_shapes
: 2
lstm_21/while/Identity_2º
lstm_21/while/Identity_3IdentityBlstm_21/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_21/while/NoOp*
T0*
_output_shapes
: 2
lstm_21/while/Identity_3¤
lstm_21/while/Identity_4Identity$lstm_21/while/lstm_cell_21/mul_5:z:0^lstm_21/while/NoOp*
T0*
_output_shapes

:22
lstm_21/while/Identity_4¤
lstm_21/while/Identity_5Identity$lstm_21/while/lstm_cell_21/add_3:z:0^lstm_21/while/NoOp*
T0*
_output_shapes

:22
lstm_21/while/Identity_5
lstm_21/while/NoOpNoOp2^lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp1^lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp3^lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_21/while/NoOp"9
lstm_21_while_identitylstm_21/while/Identity:output:0"=
lstm_21_while_identity_1!lstm_21/while/Identity_1:output:0"=
lstm_21_while_identity_2!lstm_21/while/Identity_2:output:0"=
lstm_21_while_identity_3!lstm_21/while/Identity_3:output:0"=
lstm_21_while_identity_4!lstm_21/while/Identity_4:output:0"=
lstm_21_while_identity_5!lstm_21/while/Identity_5:output:0"L
#lstm_21_while_lstm_21_strided_slice%lstm_21_while_lstm_21_strided_slice_0"z
:lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource<lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource_0"|
;lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource=lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource_0"x
9lstm_21_while_lstm_cell_21_matmul_readvariableop_resource;lstm_21_while_lstm_cell_21_matmul_readvariableop_resource_0"È
alstm_21_while_tensorarrayv2read_tensorlistgetitem_lstm_21_tensorarrayunstack_tensorlistfromtensorclstm_21_while_tensorarrayv2read_tensorlistgetitem_lstm_21_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2f
1lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp1lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp2d
0lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp0lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp2h
2lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp2lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp: 
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


H__inference_sequential_10_layer_call_and_return_conditional_losses_58377

inputsF
3lstm_21_lstm_cell_21_matmul_readvariableop_resource:	ÈG
5lstm_21_lstm_cell_21_matmul_1_readvariableop_resource:2J
7lstm_21_lstm_cell_21_matmul_1_readvariableop_1_resource:	2ÈC
4lstm_21_lstm_cell_21_biasadd_readvariableop_resource:	ÈD
2lstm_21_lstm_cell_21_mul_2_readvariableop_resource:2F
3lstm_20_lstm_cell_20_matmul_readvariableop_resource:	2ÈG
5lstm_20_lstm_cell_20_matmul_1_readvariableop_resource:2J
7lstm_20_lstm_cell_20_matmul_1_readvariableop_1_resource:	2ÈC
4lstm_20_lstm_cell_20_biasadd_readvariableop_resource:	ÈD
2lstm_20_lstm_cell_20_mul_2_readvariableop_resource:2M
;time_distributed_10_dense_10_matmul_readvariableop_resource:2J
<time_distributed_10_dense_10_biasadd_readvariableop_resource:
identity¢lstm_20/AssignVariableOp¢lstm_20/AssignVariableOp_1¢lstm_20/ReadVariableOp¢lstm_20/ReadVariableOp_1¢+lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp¢*lstm_20/lstm_cell_20/MatMul/ReadVariableOp¢,lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp¢.lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1¢)lstm_20/lstm_cell_20/mul_2/ReadVariableOp¢lstm_20/while¢lstm_21/AssignVariableOp¢lstm_21/AssignVariableOp_1¢lstm_21/ReadVariableOp¢lstm_21/ReadVariableOp_1¢+lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp¢*lstm_21/lstm_cell_21/MatMul/ReadVariableOp¢,lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp¢.lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1¢)lstm_21/lstm_cell_21/mul_2/ReadVariableOp¢lstm_21/while¢3time_distributed_10/dense_10/BiasAdd/ReadVariableOp¢2time_distributed_10/dense_10/MatMul/ReadVariableOp
lstm_21/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_21/transpose/perm
lstm_21/transpose	Transposeinputslstm_21/transpose/perm:output:0*
T0*"
_output_shapes
:
2
lstm_21/transposes
lstm_21/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
         2
lstm_21/Shape
lstm_21/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_21/strided_slice/stack
lstm_21/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_21/strided_slice/stack_1
lstm_21/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_21/strided_slice/stack_2
lstm_21/strided_sliceStridedSlicelstm_21/Shape:output:0$lstm_21/strided_slice/stack:output:0&lstm_21/strided_slice/stack_1:output:0&lstm_21/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_21/strided_slice
#lstm_21/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2%
#lstm_21/TensorArrayV2/element_shapeÐ
lstm_21/TensorArrayV2TensorListReserve,lstm_21/TensorArrayV2/element_shape:output:0lstm_21/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_21/TensorArrayV2Ï
=lstm_21/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      2?
=lstm_21/TensorArrayUnstack/TensorListFromTensor/element_shape
/lstm_21/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_21/transpose:y:0Flstm_21/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type021
/lstm_21/TensorArrayUnstack/TensorListFromTensor
lstm_21/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_21/strided_slice_1/stack
lstm_21/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_21/strided_slice_1/stack_1
lstm_21/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_21/strided_slice_1/stack_2£
lstm_21/strided_slice_1StridedSlicelstm_21/transpose:y:0&lstm_21/strided_slice_1/stack:output:0(lstm_21/strided_slice_1/stack_1:output:0(lstm_21/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
lstm_21/strided_slice_1Í
*lstm_21/lstm_cell_21/MatMul/ReadVariableOpReadVariableOp3lstm_21_lstm_cell_21_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02,
*lstm_21/lstm_cell_21/MatMul/ReadVariableOpÄ
lstm_21/lstm_cell_21/MatMulMatMul lstm_21/strided_slice_1:output:02lstm_21/lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_21/lstm_cell_21/MatMulÒ
,lstm_21/lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp5lstm_21_lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02.
,lstm_21/lstm_cell_21/MatMul_1/ReadVariableOpÙ
.lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1ReadVariableOp7lstm_21_lstm_cell_21_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype020
.lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1à
lstm_21/lstm_cell_21/MatMul_1MatMul4lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp:value:06lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_21/lstm_cell_21/MatMul_1·
lstm_21/lstm_cell_21/addAddV2%lstm_21/lstm_cell_21/MatMul:product:0'lstm_21/lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_21/lstm_cell_21/addÌ
+lstm_21/lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp4lstm_21_lstm_cell_21_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02-
+lstm_21/lstm_cell_21/BiasAdd/ReadVariableOpÄ
lstm_21/lstm_cell_21/BiasAddBiasAddlstm_21/lstm_cell_21/add:z:03lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_21/lstm_cell_21/BiasAdd
$lstm_21/lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2&
$lstm_21/lstm_cell_21/split/split_dimï
lstm_21/lstm_cell_21/splitSplit-lstm_21/lstm_cell_21/split/split_dim:output:0%lstm_21/lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_21/lstm_cell_21/split}
lstm_21/lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_21/lstm_cell_21/Const
lstm_21/lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_21/lstm_cell_21/Const_1®
lstm_21/lstm_cell_21/MulMul#lstm_21/lstm_cell_21/split:output:0#lstm_21/lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Mul¯
lstm_21/lstm_cell_21/Add_1AddV2lstm_21/lstm_cell_21/Mul:z:0%lstm_21/lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Add_1¡
,lstm_21/lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,lstm_21/lstm_cell_21/clip_by_value/Minimum/yã
*lstm_21/lstm_cell_21/clip_by_value/MinimumMinimumlstm_21/lstm_cell_21/Add_1:z:05lstm_21/lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22,
*lstm_21/lstm_cell_21/clip_by_value/Minimum
$lstm_21/lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_21/lstm_cell_21/clip_by_value/yÛ
"lstm_21/lstm_cell_21/clip_by_valueMaximum.lstm_21/lstm_cell_21/clip_by_value/Minimum:z:0-lstm_21/lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22$
"lstm_21/lstm_cell_21/clip_by_value
lstm_21/lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_21/lstm_cell_21/Const_2
lstm_21/lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_21/lstm_cell_21/Const_3´
lstm_21/lstm_cell_21/Mul_1Mul#lstm_21/lstm_cell_21/split:output:1%lstm_21/lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Mul_1±
lstm_21/lstm_cell_21/Add_2AddV2lstm_21/lstm_cell_21/Mul_1:z:0%lstm_21/lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Add_2¥
.lstm_21/lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_21/lstm_cell_21/clip_by_value_1/Minimum/yé
,lstm_21/lstm_cell_21/clip_by_value_1/MinimumMinimumlstm_21/lstm_cell_21/Add_2:z:07lstm_21/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22.
,lstm_21/lstm_cell_21/clip_by_value_1/Minimum
&lstm_21/lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_21/lstm_cell_21/clip_by_value_1/yã
$lstm_21/lstm_cell_21/clip_by_value_1Maximum0lstm_21/lstm_cell_21/clip_by_value_1/Minimum:z:0/lstm_21/lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22&
$lstm_21/lstm_cell_21/clip_by_value_1É
)lstm_21/lstm_cell_21/mul_2/ReadVariableOpReadVariableOp2lstm_21_lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02+
)lstm_21/lstm_cell_21/mul_2/ReadVariableOpÅ
lstm_21/lstm_cell_21/mul_2Mul(lstm_21/lstm_cell_21/clip_by_value_1:z:01lstm_21/lstm_cell_21/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/mul_2
lstm_21/lstm_cell_21/TanhTanh#lstm_21/lstm_cell_21/split:output:2*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Tanh¯
lstm_21/lstm_cell_21/mul_3Mul&lstm_21/lstm_cell_21/clip_by_value:z:0lstm_21/lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/mul_3ª
lstm_21/lstm_cell_21/add_3AddV2lstm_21/lstm_cell_21/mul_2:z:0lstm_21/lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/add_3
lstm_21/lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_21/lstm_cell_21/Const_4
lstm_21/lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_21/lstm_cell_21/Const_5´
lstm_21/lstm_cell_21/Mul_4Mul#lstm_21/lstm_cell_21/split:output:3%lstm_21/lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Mul_4±
lstm_21/lstm_cell_21/Add_4AddV2lstm_21/lstm_cell_21/Mul_4:z:0%lstm_21/lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Add_4¥
.lstm_21/lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_21/lstm_cell_21/clip_by_value_2/Minimum/yé
,lstm_21/lstm_cell_21/clip_by_value_2/MinimumMinimumlstm_21/lstm_cell_21/Add_4:z:07lstm_21/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22.
,lstm_21/lstm_cell_21/clip_by_value_2/Minimum
&lstm_21/lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_21/lstm_cell_21/clip_by_value_2/yã
$lstm_21/lstm_cell_21/clip_by_value_2Maximum0lstm_21/lstm_cell_21/clip_by_value_2/Minimum:z:0/lstm_21/lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22&
$lstm_21/lstm_cell_21/clip_by_value_2
lstm_21/lstm_cell_21/Tanh_1Tanhlstm_21/lstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Tanh_1³
lstm_21/lstm_cell_21/mul_5Mul(lstm_21/lstm_cell_21/clip_by_value_2:z:0lstm_21/lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/mul_5
%lstm_21/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2'
%lstm_21/TensorArrayV2_1/element_shapeÖ
lstm_21/TensorArrayV2_1TensorListReserve.lstm_21/TensorArrayV2_1/element_shape:output:0lstm_21/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_21/TensorArrayV2_1^
lstm_21/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_21/time¦
lstm_21/ReadVariableOpReadVariableOp5lstm_21_lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_21/ReadVariableOp§
lstm_21/ReadVariableOp_1ReadVariableOp2lstm_21_lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_21/ReadVariableOp_1
 lstm_21/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2"
 lstm_21/while/maximum_iterationsz
lstm_21/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_21/while/loop_counterí
lstm_21/whileWhile#lstm_21/while/loop_counter:output:0)lstm_21/while/maximum_iterations:output:0lstm_21/time:output:0 lstm_21/TensorArrayV2_1:handle:0lstm_21/ReadVariableOp:value:0 lstm_21/ReadVariableOp_1:value:0lstm_21/strided_slice:output:0?lstm_21/TensorArrayUnstack/TensorListFromTensor:output_handle:03lstm_21_lstm_cell_21_matmul_readvariableop_resource7lstm_21_lstm_cell_21_matmul_1_readvariableop_1_resource4lstm_21_lstm_cell_21_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *$
bodyR
lstm_21_while_body_58086*$
condR
lstm_21_while_cond_58085*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
lstm_21/whileÅ
8lstm_21/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2:
8lstm_21/TensorArrayV2Stack/TensorListStack/element_shapeÿ
*lstm_21/TensorArrayV2Stack/TensorListStackTensorListStacklstm_21/while:output:3Alstm_21/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02,
*lstm_21/TensorArrayV2Stack/TensorListStack
lstm_21/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
lstm_21/strided_slice_2/stack
lstm_21/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2!
lstm_21/strided_slice_2/stack_1
lstm_21/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_21/strided_slice_2/stack_2Á
lstm_21/strided_slice_2StridedSlice3lstm_21/TensorArrayV2Stack/TensorListStack:tensor:0&lstm_21/strided_slice_2/stack:output:0(lstm_21/strided_slice_2/stack_1:output:0(lstm_21/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_21/strided_slice_2
lstm_21/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_21/transpose_1/perm¼
lstm_21/transpose_1	Transpose3lstm_21/TensorArrayV2Stack/TensorListStack:tensor:0!lstm_21/transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
lstm_21/transpose_1v
lstm_21/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_21/runtime
lstm_21/AssignVariableOpAssignVariableOp5lstm_21_lstm_cell_21_matmul_1_readvariableop_resourcelstm_21/while:output:4^lstm_21/ReadVariableOp-^lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_21/AssignVariableOp
lstm_21/AssignVariableOp_1AssignVariableOp2lstm_21_lstm_cell_21_mul_2_readvariableop_resourcelstm_21/while:output:5^lstm_21/ReadVariableOp_1*^lstm_21/lstm_cell_21/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_21/AssignVariableOp_1
lstm_20/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_20/transpose/perm
lstm_20/transpose	Transposelstm_21/transpose_1:y:0lstm_20/transpose/perm:output:0*
T0*"
_output_shapes
:
22
lstm_20/transposes
lstm_20/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
      2   2
lstm_20/Shape
lstm_20/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_20/strided_slice/stack
lstm_20/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_20/strided_slice/stack_1
lstm_20/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_20/strided_slice/stack_2
lstm_20/strided_sliceStridedSlicelstm_20/Shape:output:0$lstm_20/strided_slice/stack:output:0&lstm_20/strided_slice/stack_1:output:0&lstm_20/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_20/strided_slice
#lstm_20/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2%
#lstm_20/TensorArrayV2/element_shapeÐ
lstm_20/TensorArrayV2TensorListReserve,lstm_20/TensorArrayV2/element_shape:output:0lstm_20/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_20/TensorArrayV2Ï
=lstm_20/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2?
=lstm_20/TensorArrayUnstack/TensorListFromTensor/element_shape
/lstm_20/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_20/transpose:y:0Flstm_20/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type021
/lstm_20/TensorArrayUnstack/TensorListFromTensor
lstm_20/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_20/strided_slice_1/stack
lstm_20/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_20/strided_slice_1/stack_1
lstm_20/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_20/strided_slice_1/stack_2£
lstm_20/strided_slice_1StridedSlicelstm_20/transpose:y:0&lstm_20/strided_slice_1/stack:output:0(lstm_20/strided_slice_1/stack_1:output:0(lstm_20/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_20/strided_slice_1Í
*lstm_20/lstm_cell_20/MatMul/ReadVariableOpReadVariableOp3lstm_20_lstm_cell_20_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02,
*lstm_20/lstm_cell_20/MatMul/ReadVariableOpÄ
lstm_20/lstm_cell_20/MatMulMatMul lstm_20/strided_slice_1:output:02lstm_20/lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_20/lstm_cell_20/MatMulÒ
,lstm_20/lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp5lstm_20_lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02.
,lstm_20/lstm_cell_20/MatMul_1/ReadVariableOpÙ
.lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1ReadVariableOp7lstm_20_lstm_cell_20_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype020
.lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1à
lstm_20/lstm_cell_20/MatMul_1MatMul4lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp:value:06lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_20/lstm_cell_20/MatMul_1·
lstm_20/lstm_cell_20/addAddV2%lstm_20/lstm_cell_20/MatMul:product:0'lstm_20/lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_20/lstm_cell_20/addÌ
+lstm_20/lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp4lstm_20_lstm_cell_20_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02-
+lstm_20/lstm_cell_20/BiasAdd/ReadVariableOpÄ
lstm_20/lstm_cell_20/BiasAddBiasAddlstm_20/lstm_cell_20/add:z:03lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_20/lstm_cell_20/BiasAdd
$lstm_20/lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2&
$lstm_20/lstm_cell_20/split/split_dimï
lstm_20/lstm_cell_20/splitSplit-lstm_20/lstm_cell_20/split/split_dim:output:0%lstm_20/lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_20/lstm_cell_20/split}
lstm_20/lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_20/lstm_cell_20/Const
lstm_20/lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_20/lstm_cell_20/Const_1®
lstm_20/lstm_cell_20/MulMul#lstm_20/lstm_cell_20/split:output:0#lstm_20/lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Mul¯
lstm_20/lstm_cell_20/Add_1AddV2lstm_20/lstm_cell_20/Mul:z:0%lstm_20/lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Add_1¡
,lstm_20/lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,lstm_20/lstm_cell_20/clip_by_value/Minimum/yã
*lstm_20/lstm_cell_20/clip_by_value/MinimumMinimumlstm_20/lstm_cell_20/Add_1:z:05lstm_20/lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22,
*lstm_20/lstm_cell_20/clip_by_value/Minimum
$lstm_20/lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_20/lstm_cell_20/clip_by_value/yÛ
"lstm_20/lstm_cell_20/clip_by_valueMaximum.lstm_20/lstm_cell_20/clip_by_value/Minimum:z:0-lstm_20/lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22$
"lstm_20/lstm_cell_20/clip_by_value
lstm_20/lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_20/lstm_cell_20/Const_2
lstm_20/lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_20/lstm_cell_20/Const_3´
lstm_20/lstm_cell_20/Mul_1Mul#lstm_20/lstm_cell_20/split:output:1%lstm_20/lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Mul_1±
lstm_20/lstm_cell_20/Add_2AddV2lstm_20/lstm_cell_20/Mul_1:z:0%lstm_20/lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Add_2¥
.lstm_20/lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_20/lstm_cell_20/clip_by_value_1/Minimum/yé
,lstm_20/lstm_cell_20/clip_by_value_1/MinimumMinimumlstm_20/lstm_cell_20/Add_2:z:07lstm_20/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22.
,lstm_20/lstm_cell_20/clip_by_value_1/Minimum
&lstm_20/lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_20/lstm_cell_20/clip_by_value_1/yã
$lstm_20/lstm_cell_20/clip_by_value_1Maximum0lstm_20/lstm_cell_20/clip_by_value_1/Minimum:z:0/lstm_20/lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22&
$lstm_20/lstm_cell_20/clip_by_value_1É
)lstm_20/lstm_cell_20/mul_2/ReadVariableOpReadVariableOp2lstm_20_lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02+
)lstm_20/lstm_cell_20/mul_2/ReadVariableOpÅ
lstm_20/lstm_cell_20/mul_2Mul(lstm_20/lstm_cell_20/clip_by_value_1:z:01lstm_20/lstm_cell_20/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/mul_2
lstm_20/lstm_cell_20/TanhTanh#lstm_20/lstm_cell_20/split:output:2*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Tanh¯
lstm_20/lstm_cell_20/mul_3Mul&lstm_20/lstm_cell_20/clip_by_value:z:0lstm_20/lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/mul_3ª
lstm_20/lstm_cell_20/add_3AddV2lstm_20/lstm_cell_20/mul_2:z:0lstm_20/lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/add_3
lstm_20/lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_20/lstm_cell_20/Const_4
lstm_20/lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_20/lstm_cell_20/Const_5´
lstm_20/lstm_cell_20/Mul_4Mul#lstm_20/lstm_cell_20/split:output:3%lstm_20/lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Mul_4±
lstm_20/lstm_cell_20/Add_4AddV2lstm_20/lstm_cell_20/Mul_4:z:0%lstm_20/lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Add_4¥
.lstm_20/lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_20/lstm_cell_20/clip_by_value_2/Minimum/yé
,lstm_20/lstm_cell_20/clip_by_value_2/MinimumMinimumlstm_20/lstm_cell_20/Add_4:z:07lstm_20/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22.
,lstm_20/lstm_cell_20/clip_by_value_2/Minimum
&lstm_20/lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_20/lstm_cell_20/clip_by_value_2/yã
$lstm_20/lstm_cell_20/clip_by_value_2Maximum0lstm_20/lstm_cell_20/clip_by_value_2/Minimum:z:0/lstm_20/lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22&
$lstm_20/lstm_cell_20/clip_by_value_2
lstm_20/lstm_cell_20/Tanh_1Tanhlstm_20/lstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Tanh_1³
lstm_20/lstm_cell_20/mul_5Mul(lstm_20/lstm_cell_20/clip_by_value_2:z:0lstm_20/lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/mul_5
%lstm_20/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2'
%lstm_20/TensorArrayV2_1/element_shapeÖ
lstm_20/TensorArrayV2_1TensorListReserve.lstm_20/TensorArrayV2_1/element_shape:output:0lstm_20/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_20/TensorArrayV2_1^
lstm_20/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_20/time¦
lstm_20/ReadVariableOpReadVariableOp5lstm_20_lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_20/ReadVariableOp§
lstm_20/ReadVariableOp_1ReadVariableOp2lstm_20_lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_20/ReadVariableOp_1
 lstm_20/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2"
 lstm_20/while/maximum_iterationsz
lstm_20/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_20/while/loop_counterí
lstm_20/whileWhile#lstm_20/while/loop_counter:output:0)lstm_20/while/maximum_iterations:output:0lstm_20/time:output:0 lstm_20/TensorArrayV2_1:handle:0lstm_20/ReadVariableOp:value:0 lstm_20/ReadVariableOp_1:value:0lstm_20/strided_slice:output:0?lstm_20/TensorArrayUnstack/TensorListFromTensor:output_handle:03lstm_20_lstm_cell_20_matmul_readvariableop_resource7lstm_20_lstm_cell_20_matmul_1_readvariableop_1_resource4lstm_20_lstm_cell_20_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *$
bodyR
lstm_20_while_body_58260*$
condR
lstm_20_while_cond_58259*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
lstm_20/whileÅ
8lstm_20/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2:
8lstm_20/TensorArrayV2Stack/TensorListStack/element_shapeÿ
*lstm_20/TensorArrayV2Stack/TensorListStackTensorListStacklstm_20/while:output:3Alstm_20/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02,
*lstm_20/TensorArrayV2Stack/TensorListStack
lstm_20/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
lstm_20/strided_slice_2/stack
lstm_20/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2!
lstm_20/strided_slice_2/stack_1
lstm_20/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_20/strided_slice_2/stack_2Á
lstm_20/strided_slice_2StridedSlice3lstm_20/TensorArrayV2Stack/TensorListStack:tensor:0&lstm_20/strided_slice_2/stack:output:0(lstm_20/strided_slice_2/stack_1:output:0(lstm_20/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_20/strided_slice_2
lstm_20/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_20/transpose_1/perm¼
lstm_20/transpose_1	Transpose3lstm_20/TensorArrayV2Stack/TensorListStack:tensor:0!lstm_20/transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
lstm_20/transpose_1v
lstm_20/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_20/runtime
lstm_20/AssignVariableOpAssignVariableOp5lstm_20_lstm_cell_20_matmul_1_readvariableop_resourcelstm_20/while:output:4^lstm_20/ReadVariableOp-^lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_20/AssignVariableOp
lstm_20/AssignVariableOp_1AssignVariableOp2lstm_20_lstm_cell_20_mul_2_readvariableop_resourcelstm_20/while:output:5^lstm_20/ReadVariableOp_1*^lstm_20/lstm_cell_20/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_20/AssignVariableOp_1
!time_distributed_10/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2#
!time_distributed_10/Reshape/shape³
time_distributed_10/ReshapeReshapelstm_20/transpose_1:y:0*time_distributed_10/Reshape/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_10/Reshapeä
2time_distributed_10/dense_10/MatMul/ReadVariableOpReadVariableOp;time_distributed_10_dense_10_matmul_readvariableop_resource*
_output_shapes

:2*
dtype024
2time_distributed_10/dense_10/MatMul/ReadVariableOpß
#time_distributed_10/dense_10/MatMulMatMul$time_distributed_10/Reshape:output:0:time_distributed_10/dense_10/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
2%
#time_distributed_10/dense_10/MatMulã
3time_distributed_10/dense_10/BiasAdd/ReadVariableOpReadVariableOp<time_distributed_10_dense_10_biasadd_readvariableop_resource*
_output_shapes
:*
dtype025
3time_distributed_10/dense_10/BiasAdd/ReadVariableOpì
$time_distributed_10/dense_10/BiasAddBiasAdd-time_distributed_10/dense_10/MatMul:product:0;time_distributed_10/dense_10/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
2&
$time_distributed_10/dense_10/BiasAdd
#time_distributed_10/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      2%
#time_distributed_10/Reshape_1/shapeÓ
time_distributed_10/Reshape_1Reshape-time_distributed_10/dense_10/BiasAdd:output:0,time_distributed_10/Reshape_1/shape:output:0*
T0*"
_output_shapes
:
2
time_distributed_10/Reshape_1
#time_distributed_10/Reshape_2/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2%
#time_distributed_10/Reshape_2/shape¹
time_distributed_10/Reshape_2Reshapelstm_20/transpose_1:y:0,time_distributed_10/Reshape_2/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_10/Reshape_2|
IdentityIdentity&time_distributed_10/Reshape_1:output:0^NoOp*
T0*"
_output_shapes
:
2

Identityÿ
NoOpNoOp^lstm_20/AssignVariableOp^lstm_20/AssignVariableOp_1^lstm_20/ReadVariableOp^lstm_20/ReadVariableOp_1,^lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp+^lstm_20/lstm_cell_20/MatMul/ReadVariableOp-^lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp/^lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1*^lstm_20/lstm_cell_20/mul_2/ReadVariableOp^lstm_20/while^lstm_21/AssignVariableOp^lstm_21/AssignVariableOp_1^lstm_21/ReadVariableOp^lstm_21/ReadVariableOp_1,^lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp+^lstm_21/lstm_cell_21/MatMul/ReadVariableOp-^lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp/^lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1*^lstm_21/lstm_cell_21/mul_2/ReadVariableOp^lstm_21/while4^time_distributed_10/dense_10/BiasAdd/ReadVariableOp3^time_distributed_10/dense_10/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 24
lstm_20/AssignVariableOplstm_20/AssignVariableOp28
lstm_20/AssignVariableOp_1lstm_20/AssignVariableOp_120
lstm_20/ReadVariableOplstm_20/ReadVariableOp24
lstm_20/ReadVariableOp_1lstm_20/ReadVariableOp_12Z
+lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp+lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp2X
*lstm_20/lstm_cell_20/MatMul/ReadVariableOp*lstm_20/lstm_cell_20/MatMul/ReadVariableOp2\
,lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp,lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp2`
.lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1.lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_12V
)lstm_20/lstm_cell_20/mul_2/ReadVariableOp)lstm_20/lstm_cell_20/mul_2/ReadVariableOp2
lstm_20/whilelstm_20/while24
lstm_21/AssignVariableOplstm_21/AssignVariableOp28
lstm_21/AssignVariableOp_1lstm_21/AssignVariableOp_120
lstm_21/ReadVariableOplstm_21/ReadVariableOp24
lstm_21/ReadVariableOp_1lstm_21/ReadVariableOp_12Z
+lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp+lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp2X
*lstm_21/lstm_cell_21/MatMul/ReadVariableOp*lstm_21/lstm_cell_21/MatMul/ReadVariableOp2\
,lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp,lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp2`
.lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1.lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_12V
)lstm_21/lstm_cell_21/mul_2/ReadVariableOp)lstm_21/lstm_cell_21/mul_2/ReadVariableOp2
lstm_21/whilelstm_21/while2j
3time_distributed_10/dense_10/BiasAdd/ReadVariableOp3time_distributed_10/dense_10/BiasAdd/ReadVariableOp2h
2time_distributed_10/dense_10/MatMul/ReadVariableOp2time_distributed_10/dense_10/MatMul/ReadVariableOp:J F
"
_output_shapes
:

 
_user_specified_nameinputs
ø9

B__inference_lstm_21_layer_call_and_return_conditional_losses_55442

inputs$
lstm_cell_21_55301:2$
lstm_cell_21_55303:2%
lstm_cell_21_55305:	È%
lstm_cell_21_55307:	2È!
lstm_cell_21_55309:	È
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢$lstm_cell_21/StatefulPartitionedCall¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1
$lstm_cell_21/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_21_55301lstm_cell_21_55303lstm_cell_21_55305lstm_cell_21_55307lstm_cell_21_55309*
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_553002&
$lstm_cell_21/StatefulPartitionedCall
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
times
ReadVariableOpReadVariableOplstm_cell_21_55301*
_output_shapes

:2*
dtype02
ReadVariableOpw
ReadVariableOp_1ReadVariableOplstm_cell_21_55303*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter¥
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_21_55305lstm_cell_21_55307lstm_cell_21_55309*
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
while_body_55320*
condR
while_cond_55319*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm¥
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime·
AssignVariableOpAssignVariableOplstm_cell_21_55301while:output:4^ReadVariableOp%^lstm_cell_21/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp½
AssignVariableOp_1AssignVariableOplstm_cell_21_55303while:output:5^ReadVariableOp_1%^lstm_cell_21/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22

IdentityÉ
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1%^lstm_cell_21/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:ÿÿÿÿÿÿÿÿÿ: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12L
$lstm_cell_21/StatefulPartitionedCall$lstm_cell_21/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs

»
-__inference_sequential_10_layer_call_fn_58770

inputs
unknown:	È
	unknown_0:2
	unknown_1:	2È
	unknown_2:	È
	unknown_3:2
	unknown_4:	2È
	unknown_5:2
	unknown_6:	2È
	unknown_7:	È
	unknown_8:2
	unknown_9:2

unknown_10:
identity¢StatefulPartitionedCallò
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
GPU 2J 8 *Q
fLRJ
H__inference_sequential_10_layer_call_and_return_conditional_losses_573212
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
ü.
º
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60669

inputs
states_0
states_11
matmul_readvariableop_resource:	È5
"matmul_1_readvariableop_1_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp¢MatMul_1/ReadVariableOp_1¢mul_2/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMulu
MatMul_1/ReadVariableOpReadVariableOpstates_0*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOp_1
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
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
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2Ì
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
Ë
£
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_60385

inputs9
'dense_10_matmul_readvariableop_resource:26
(dense_10_biasadd_readvariableop_resource:
identity¢dense_10/BiasAdd/ReadVariableOp¢dense_10/MatMul/ReadVariableOpD
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
strided_slice/stack_2â
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
valueB"ÿÿÿÿ2   2
Reshape/shapeo
ReshapeReshapeinputsReshape/shape:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22	
Reshape¨
dense_10/MatMul/ReadVariableOpReadVariableOp'dense_10_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02 
dense_10/MatMul/ReadVariableOp
dense_10/MatMulMatMulReshape:output:0&dense_10/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
dense_10/MatMul§
dense_10/BiasAdd/ReadVariableOpReadVariableOp(dense_10_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02!
dense_10/BiasAdd/ReadVariableOp¥
dense_10/BiasAddBiasAdddense_10/MatMul:product:0'dense_10/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
dense_10/BiasAddq
Reshape_1/shape/0Const*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
Reshape_1/shape/0h
Reshape_1/shape/2Const*
_output_shapes
: *
dtype0*
value	B :2
Reshape_1/shape/2¨
Reshape_1/shapePackReshape_1/shape/0:output:0strided_slice:output:0Reshape_1/shape/2:output:0*
N*
T0*
_output_shapes
:2
Reshape_1/shape
	Reshape_1Reshapedense_10/BiasAdd:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2

Identity
NoOpNoOp ^dense_10/BiasAdd/ReadVariableOp^dense_10/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2: : 2B
dense_10/BiasAdd/ReadVariableOpdense_10/BiasAdd/ReadVariableOp2@
dense_10/MatMul/ReadVariableOpdense_10/MatMul/ReadVariableOp:\ X
4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
¾
ô
&sequential_10_lstm_21_while_cond_54931H
Dsequential_10_lstm_21_while_sequential_10_lstm_21_while_loop_counterN
Jsequential_10_lstm_21_while_sequential_10_lstm_21_while_maximum_iterations+
'sequential_10_lstm_21_while_placeholder-
)sequential_10_lstm_21_while_placeholder_1-
)sequential_10_lstm_21_while_placeholder_2-
)sequential_10_lstm_21_while_placeholder_3H
Dsequential_10_lstm_21_while_less_sequential_10_lstm_21_strided_slice_
[sequential_10_lstm_21_while_sequential_10_lstm_21_while_cond_54931___redundant_placeholder0_
[sequential_10_lstm_21_while_sequential_10_lstm_21_while_cond_54931___redundant_placeholder1_
[sequential_10_lstm_21_while_sequential_10_lstm_21_while_cond_54931___redundant_placeholder2_
[sequential_10_lstm_21_while_sequential_10_lstm_21_while_cond_54931___redundant_placeholder3(
$sequential_10_lstm_21_while_identity
Ü
 sequential_10/lstm_21/while/LessLess'sequential_10_lstm_21_while_placeholderDsequential_10_lstm_21_while_less_sequential_10_lstm_21_strided_slice*
T0*
_output_shapes
: 2"
 sequential_10/lstm_21/while/Less
$sequential_10/lstm_21/while/IdentityIdentity$sequential_10/lstm_21/while/Less:z:0*
T0
*
_output_shapes
: 2&
$sequential_10/lstm_21/while/Identity"U
$sequential_10_lstm_21_while_identity-sequential_10/lstm_21/while/Identity:output:0*(
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
å$
Ø
while_body_56466
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
while_lstm_cell_20_56490_0:	2È-
while_lstm_cell_20_56492_0:	2È)
while_lstm_cell_20_56494_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
while_lstm_cell_20_56490:	2È+
while_lstm_cell_20_56492:	2È'
while_lstm_cell_20_56494:	È¢*while/lstm_cell_20/StatefulPartitionedCallÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÂ
*while/lstm_cell_20/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_20_56490_0while_lstm_cell_20_56492_0while_lstm_cell_20_56494_0*
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_563062,
*while/lstm_cell_20/StatefulPartitionedCall÷
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder3while/lstm_cell_20/StatefulPartitionedCall:output:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identity3while/lstm_cell_20/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identity3while/lstm_cell_20/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5

while/NoOpNoOp+^while/lstm_cell_20/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"6
while_lstm_cell_20_56490while_lstm_cell_20_56490_0"6
while_lstm_cell_20_56492while_lstm_cell_20_56492_0"6
while_lstm_cell_20_56494while_lstm_cell_20_56494_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2X
*while/lstm_cell_20/StatefulPartitionedCall*while/lstm_cell_20/StatefulPartitionedCall: 
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
¨
¼
while_cond_59405
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_59405___redundant_placeholder03
/while_while_cond_59405___redundant_placeholder13
/while_while_cond_59405___redundant_placeholder23
/while_while_cond_59405___redundant_placeholder3
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
¨
¼
while_cond_55319
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_55319___redundant_placeholder03
/while_while_cond_55319___redundant_placeholder13
/while_while_cond_55319___redundant_placeholder23
/while_while_cond_55319___redundant_placeholder3
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
ô.
¸
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60764

inputs

states
states_11
matmul_readvariableop_resource:	È5
"matmul_1_readvariableop_1_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp¢MatMul_1/ReadVariableOp_1¢mul_2/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMuls
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOp_1
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
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
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2Ì
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
ü.
º
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60506

inputs
states_0
states_11
matmul_readvariableop_resource:	È5
"matmul_1_readvariableop_1_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp¢MatMul_1/ReadVariableOp_1¢mul_2/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMulu
MatMul_1/ReadVariableOpReadVariableOpstates_0*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOp_1
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
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
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2Ì
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
º
 
3__inference_time_distributed_10_layer_call_fn_60431

inputs
unknown:2
	unknown_0:
identity¢StatefulPartitionedCall
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *W
fRRP
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_568582
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2: : 22
StatefulPartitionedCallStatefulPartitionedCall:\ X
4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs


Ü
lstm_20_while_cond_58623,
(lstm_20_while_lstm_20_while_loop_counter2
.lstm_20_while_lstm_20_while_maximum_iterations
lstm_20_while_placeholder
lstm_20_while_placeholder_1
lstm_20_while_placeholder_2
lstm_20_while_placeholder_3,
(lstm_20_while_less_lstm_20_strided_sliceC
?lstm_20_while_lstm_20_while_cond_58623___redundant_placeholder0C
?lstm_20_while_lstm_20_while_cond_58623___redundant_placeholder1C
?lstm_20_while_lstm_20_while_cond_58623___redundant_placeholder2C
?lstm_20_while_lstm_20_while_cond_58623___redundant_placeholder3
lstm_20_while_identity

lstm_20/while/LessLesslstm_20_while_placeholder(lstm_20_while_less_lstm_20_strided_slice*
T0*
_output_shapes
: 2
lstm_20/while/Lessu
lstm_20/while/IdentityIdentitylstm_20/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_20/while/Identity"9
lstm_20_while_identitylstm_20/while/Identity:output:0*(
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
	
ð
'__inference_lstm_21_layer_call_fn_59541
inputs_0
unknown:2
	unknown_0:2
	unknown_1:	È
	unknown_2:	2È
	unknown_3:	È
identity¢StatefulPartitionedCall
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_21_layer_call_and_return_conditional_losses_557592
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:ÿÿÿÿÿÿÿÿÿ: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/0
®
Â
-__inference_sequential_10_layer_call_fn_57348
lstm_21_input
unknown:	È
	unknown_0:2
	unknown_1:	2È
	unknown_2:	È
	unknown_3:2
	unknown_4:	2È
	unknown_5:2
	unknown_6:	2È
	unknown_7:	È
	unknown_8:2
	unknown_9:2

unknown_10:
identity¢StatefulPartitionedCallù
StatefulPartitionedCallStatefulPartitionedCalllstm_21_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
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
GPU 2J 8 *Q
fLRJ
H__inference_sequential_10_layer_call_and_return_conditional_losses_573212
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
StatefulPartitionedCallStatefulPartitionedCall:Q M
"
_output_shapes
:

'
_user_specified_namelstm_21_input
¨
¼
while_cond_59643
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_59643___redundant_placeholder03
/while_while_cond_59643___redundant_placeholder13
/while_while_cond_59643___redundant_placeholder23
/while_while_cond_59643___redundant_placeholder3
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
ÔY
Ë
while_body_60178
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_20_matmul_readvariableop_resource_0:	2ÈH
5while_lstm_cell_20_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_20_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_20_matmul_readvariableop_resource:	2ÈF
3while_lstm_cell_20_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_20_biasadd_readvariableop_resource:	È¢)while/lstm_cell_20/BiasAdd/ReadVariableOp¢(while/lstm_cell_20/MatMul/ReadVariableOp¢*while/lstm_cell_20/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_20/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_20_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02*
(while/lstm_cell_20/MatMul/ReadVariableOpÎ
while/lstm_cell_20/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/MatMulÏ
*while/lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_20_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_20/MatMul_1/ReadVariableOp·
while/lstm_cell_20/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_20/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/MatMul_1¯
while/lstm_cell_20/addAddV2#while/lstm_cell_20/MatMul:product:0%while/lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/addÈ
)while/lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_20_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_20/BiasAdd/ReadVariableOp¼
while/lstm_cell_20/BiasAddBiasAddwhile/lstm_cell_20/add:z:01while/lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/BiasAdd
"while/lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_20/split/split_dimç
while/lstm_cell_20/splitSplit+while/lstm_cell_20/split/split_dim:output:0#while/lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_20/splity
while/lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const}
while/lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_1¦
while/lstm_cell_20/MulMul!while/lstm_cell_20/split:output:0!while/lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul§
while/lstm_cell_20/Add_1AddV2while/lstm_cell_20/Mul:z:0#while/lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_1
*while/lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_20/clip_by_value/Minimum/yÛ
(while/lstm_cell_20/clip_by_value/MinimumMinimumwhile/lstm_cell_20/Add_1:z:03while/lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(while/lstm_cell_20/clip_by_value/Minimum
"while/lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_20/clip_by_value/yÓ
 while/lstm_cell_20/clip_by_valueMaximum,while/lstm_cell_20/clip_by_value/Minimum:z:0+while/lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 while/lstm_cell_20/clip_by_value}
while/lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const_2}
while/lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_3¬
while/lstm_cell_20/Mul_1Mul!while/lstm_cell_20/split:output:1#while/lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul_1©
while/lstm_cell_20/Add_2AddV2while/lstm_cell_20/Mul_1:z:0#while/lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_2¡
,while/lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_20/clip_by_value_1/Minimum/yá
*while/lstm_cell_20/clip_by_value_1/MinimumMinimumwhile/lstm_cell_20/Add_2:z:05while/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_20/clip_by_value_1/Minimum
$while/lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_20/clip_by_value_1/yÛ
"while/lstm_cell_20/clip_by_value_1Maximum.while/lstm_cell_20/clip_by_value_1/Minimum:z:0-while/lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_20/clip_by_value_1¡
while/lstm_cell_20/mul_2Mul&while/lstm_cell_20/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_2
while/lstm_cell_20/TanhTanh!while/lstm_cell_20/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_20/Tanh§
while/lstm_cell_20/mul_3Mul$while/lstm_cell_20/clip_by_value:z:0while/lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_3¢
while/lstm_cell_20/add_3AddV2while/lstm_cell_20/mul_2:z:0while/lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_20/add_3}
while/lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const_4}
while/lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_5¬
while/lstm_cell_20/Mul_4Mul!while/lstm_cell_20/split:output:3#while/lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul_4©
while/lstm_cell_20/Add_4AddV2while/lstm_cell_20/Mul_4:z:0#while/lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_4¡
,while/lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_20/clip_by_value_2/Minimum/yá
*while/lstm_cell_20/clip_by_value_2/MinimumMinimumwhile/lstm_cell_20/Add_4:z:05while/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_20/clip_by_value_2/Minimum
$while/lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_20/clip_by_value_2/yÛ
"while/lstm_cell_20/clip_by_value_2Maximum.while/lstm_cell_20/clip_by_value_2/Minimum:z:0-while/lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_20/clip_by_value_2
while/lstm_cell_20/Tanh_1Tanhwhile/lstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Tanh_1«
while/lstm_cell_20/mul_5Mul&while/lstm_cell_20/clip_by_value_2:z:0while/lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_20/mul_5:z:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identitywhile/lstm_cell_20/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_20/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_20/BiasAdd/ReadVariableOp)^while/lstm_cell_20/MatMul/ReadVariableOp+^while/lstm_cell_20/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_20_biasadd_readvariableop_resource4while_lstm_cell_20_biasadd_readvariableop_resource_0"l
3while_lstm_cell_20_matmul_1_readvariableop_resource5while_lstm_cell_20_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_20_matmul_readvariableop_resource3while_lstm_cell_20_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_20/BiasAdd/ReadVariableOp)while/lstm_cell_20/BiasAdd/ReadVariableOp2T
(while/lstm_cell_20/MatMul/ReadVariableOp(while/lstm_cell_20/MatMul/ReadVariableOp2X
*while/lstm_cell_20/MatMul_1/ReadVariableOp*while/lstm_cell_20/MatMul_1/ReadVariableOp: 
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
æ
õ
,__inference_lstm_cell_20_layer_call_fn_61253

inputs
states_0
states_1
unknown:	2È
	unknown_0:	2È
	unknown_1:	È
identity

identity_1

identity_2¢StatefulPartitionedCall
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_612402
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
ô
þ
H__inference_sequential_10_layer_call_and_return_conditional_losses_57949
lstm_21_input 
lstm_21_57919:	È
lstm_21_57921:2 
lstm_21_57923:	2È
lstm_21_57925:	È
lstm_21_57927:2 
lstm_20_57930:	2È
lstm_20_57932:2 
lstm_20_57934:	2È
lstm_20_57936:	È
lstm_20_57938:2+
time_distributed_10_57941:2'
time_distributed_10_57943:
identity¢lstm_20/StatefulPartitionedCall¢lstm_21/StatefulPartitionedCall¢+time_distributed_10/StatefulPartitionedCall¿
lstm_21/StatefulPartitionedCallStatefulPartitionedCalllstm_21_inputlstm_21_57919lstm_21_57921lstm_21_57923lstm_21_57925lstm_21_57927*
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
GPU 2J 8 *K
fFRD
B__inference_lstm_21_layer_call_and_return_conditional_losses_570982!
lstm_21/StatefulPartitionedCallÚ
lstm_20/StatefulPartitionedCallStatefulPartitionedCall(lstm_21/StatefulPartitionedCall:output:0lstm_20_57930lstm_20_57932lstm_20_57934lstm_20_57936lstm_20_57938*
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
GPU 2J 8 *K
fFRD
B__inference_lstm_20_layer_call_and_return_conditional_losses_572872!
lstm_20/StatefulPartitionedCallå
+time_distributed_10/StatefulPartitionedCallStatefulPartitionedCall(lstm_20/StatefulPartitionedCall:output:0time_distributed_10_57941time_distributed_10_57943*
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
GPU 2J 8 *W
fRRP
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_573122-
+time_distributed_10/StatefulPartitionedCall
!time_distributed_10/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2#
!time_distributed_10/Reshape/shapeÄ
time_distributed_10/ReshapeReshape(lstm_20/StatefulPartitionedCall:output:0*time_distributed_10/Reshape/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_10/Reshape
IdentityIdentity4time_distributed_10/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

IdentityÀ
NoOpNoOp ^lstm_20/StatefulPartitionedCall ^lstm_21/StatefulPartitionedCall,^time_distributed_10/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 2B
lstm_20/StatefulPartitionedCalllstm_20/StatefulPartitionedCall2B
lstm_21/StatefulPartitionedCalllstm_21/StatefulPartitionedCall2Z
+time_distributed_10/StatefulPartitionedCall+time_distributed_10/StatefulPartitionedCall:Q M
"
_output_shapes
:

'
_user_specified_namelstm_21_input
ÔY
Ë
while_body_57470
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_20_matmul_readvariableop_resource_0:	2ÈH
5while_lstm_cell_20_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_20_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_20_matmul_readvariableop_resource:	2ÈF
3while_lstm_cell_20_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_20_biasadd_readvariableop_resource:	È¢)while/lstm_cell_20/BiasAdd/ReadVariableOp¢(while/lstm_cell_20/MatMul/ReadVariableOp¢*while/lstm_cell_20/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_20/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_20_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02*
(while/lstm_cell_20/MatMul/ReadVariableOpÎ
while/lstm_cell_20/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/MatMulÏ
*while/lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_20_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_20/MatMul_1/ReadVariableOp·
while/lstm_cell_20/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_20/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/MatMul_1¯
while/lstm_cell_20/addAddV2#while/lstm_cell_20/MatMul:product:0%while/lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/addÈ
)while/lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_20_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_20/BiasAdd/ReadVariableOp¼
while/lstm_cell_20/BiasAddBiasAddwhile/lstm_cell_20/add:z:01while/lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/BiasAdd
"while/lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_20/split/split_dimç
while/lstm_cell_20/splitSplit+while/lstm_cell_20/split/split_dim:output:0#while/lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_20/splity
while/lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const}
while/lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_1¦
while/lstm_cell_20/MulMul!while/lstm_cell_20/split:output:0!while/lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul§
while/lstm_cell_20/Add_1AddV2while/lstm_cell_20/Mul:z:0#while/lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_1
*while/lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_20/clip_by_value/Minimum/yÛ
(while/lstm_cell_20/clip_by_value/MinimumMinimumwhile/lstm_cell_20/Add_1:z:03while/lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(while/lstm_cell_20/clip_by_value/Minimum
"while/lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_20/clip_by_value/yÓ
 while/lstm_cell_20/clip_by_valueMaximum,while/lstm_cell_20/clip_by_value/Minimum:z:0+while/lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 while/lstm_cell_20/clip_by_value}
while/lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const_2}
while/lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_3¬
while/lstm_cell_20/Mul_1Mul!while/lstm_cell_20/split:output:1#while/lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul_1©
while/lstm_cell_20/Add_2AddV2while/lstm_cell_20/Mul_1:z:0#while/lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_2¡
,while/lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_20/clip_by_value_1/Minimum/yá
*while/lstm_cell_20/clip_by_value_1/MinimumMinimumwhile/lstm_cell_20/Add_2:z:05while/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_20/clip_by_value_1/Minimum
$while/lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_20/clip_by_value_1/yÛ
"while/lstm_cell_20/clip_by_value_1Maximum.while/lstm_cell_20/clip_by_value_1/Minimum:z:0-while/lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_20/clip_by_value_1¡
while/lstm_cell_20/mul_2Mul&while/lstm_cell_20/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_2
while/lstm_cell_20/TanhTanh!while/lstm_cell_20/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_20/Tanh§
while/lstm_cell_20/mul_3Mul$while/lstm_cell_20/clip_by_value:z:0while/lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_3¢
while/lstm_cell_20/add_3AddV2while/lstm_cell_20/mul_2:z:0while/lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_20/add_3}
while/lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const_4}
while/lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_5¬
while/lstm_cell_20/Mul_4Mul!while/lstm_cell_20/split:output:3#while/lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul_4©
while/lstm_cell_20/Add_4AddV2while/lstm_cell_20/Mul_4:z:0#while/lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_4¡
,while/lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_20/clip_by_value_2/Minimum/yá
*while/lstm_cell_20/clip_by_value_2/MinimumMinimumwhile/lstm_cell_20/Add_4:z:05while/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_20/clip_by_value_2/Minimum
$while/lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_20/clip_by_value_2/yÛ
"while/lstm_cell_20/clip_by_value_2Maximum.while/lstm_cell_20/clip_by_value_2/Minimum:z:0-while/lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_20/clip_by_value_2
while/lstm_cell_20/Tanh_1Tanhwhile/lstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Tanh_1«
while/lstm_cell_20/mul_5Mul&while/lstm_cell_20/clip_by_value_2:z:0while/lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_20/mul_5:z:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identitywhile/lstm_cell_20/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_20/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_20/BiasAdd/ReadVariableOp)^while/lstm_cell_20/MatMul/ReadVariableOp+^while/lstm_cell_20/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_20_biasadd_readvariableop_resource4while_lstm_cell_20_biasadd_readvariableop_resource_0"l
3while_lstm_cell_20_matmul_1_readvariableop_resource5while_lstm_cell_20_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_20_matmul_readvariableop_resource3while_lstm_cell_20_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_20/BiasAdd/ReadVariableOp)while/lstm_cell_20/BiasAdd/ReadVariableOp2T
(while/lstm_cell_20/MatMul/ReadVariableOp(while/lstm_cell_20/MatMul/ReadVariableOp2X
*while/lstm_cell_20/MatMul_1/ReadVariableOp*while/lstm_cell_20/MatMul_1/ReadVariableOp: 
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
Ä
£
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_60399

inputs9
'dense_10_matmul_readvariableop_resource:26
(dense_10_biasadd_readvariableop_resource:
identity¢dense_10/BiasAdd/ReadVariableOp¢dense_10/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2
Reshape/shapef
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes

:
22	
Reshape¨
dense_10/MatMul/ReadVariableOpReadVariableOp'dense_10_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02 
dense_10/MatMul/ReadVariableOp
dense_10/MatMulMatMulReshape:output:0&dense_10/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_10/MatMul§
dense_10/BiasAdd/ReadVariableOpReadVariableOp(dense_10_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02!
dense_10/BiasAdd/ReadVariableOp
dense_10/BiasAddBiasAdddense_10/MatMul:product:0'dense_10/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_10/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      2
Reshape_1/shape
	Reshape_1Reshapedense_10/BiasAdd:output:0Reshape_1/shape:output:0*
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

Identity
NoOpNoOp ^dense_10/BiasAdd/ReadVariableOp^dense_10/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:
2: : 2B
dense_10/BiasAdd/ReadVariableOpdense_10/BiasAdd/ReadVariableOp2@
dense_10/MatMul/ReadVariableOpdense_10/MatMul/ReadVariableOp:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
ï,

G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_55396

inputs

states
states_11
matmul_readvariableop_resource:	È3
 matmul_1_readvariableop_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2
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
Þ
î
'__inference_lstm_20_layer_call_fn_60328

inputs
unknown:	2È
	unknown_0:2
	unknown_1:	2È
	unknown_2:	È
	unknown_3:2
identity¢StatefulPartitionedCall
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
GPU 2J 8 *K
fFRD
B__inference_lstm_20_layer_call_and_return_conditional_losses_572872
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
o
¶
B__inference_lstm_20_layer_call_and_return_conditional_losses_59749
inputs_0>
+lstm_cell_20_matmul_readvariableop_resource:	2È?
-lstm_cell_20_matmul_1_readvariableop_resource:2B
/lstm_cell_20_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_20_biasadd_readvariableop_resource:	È<
*lstm_cell_20_mul_2_readvariableop_resource:2
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_20/BiasAdd/ReadVariableOp¢"lstm_cell_20/MatMul/ReadVariableOp¢$lstm_cell_20/MatMul_1/ReadVariableOp¢&lstm_cell_20/MatMul_1/ReadVariableOp_1¢!lstm_cell_20/mul_2/ReadVariableOp¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_20/MatMul/ReadVariableOpReadVariableOp+lstm_cell_20_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02$
"lstm_cell_20/MatMul/ReadVariableOp¤
lstm_cell_20/MatMulMatMulstrided_slice_1:output:0*lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/MatMulº
$lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$lstm_cell_20/MatMul_1/ReadVariableOpÁ
&lstm_cell_20/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_20_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_20/MatMul_1/ReadVariableOp_1À
lstm_cell_20/MatMul_1MatMul,lstm_cell_20/MatMul_1/ReadVariableOp:value:0.lstm_cell_20/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/MatMul_1
lstm_cell_20/addAddV2lstm_cell_20/MatMul:product:0lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_cell_20/add´
#lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_20_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_20/BiasAdd/ReadVariableOp¤
lstm_cell_20/BiasAddBiasAddlstm_cell_20/add:z:0+lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/BiasAdd~
lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_20/split/split_dimÏ
lstm_cell_20/splitSplit%lstm_cell_20/split/split_dim:output:0lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_20/splitm
lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Constq
lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_1
lstm_cell_20/MulMullstm_cell_20/split:output:0lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul
lstm_cell_20/Add_1AddV2lstm_cell_20/Mul:z:0lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_1
$lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_20/clip_by_value/Minimum/yÃ
"lstm_cell_20/clip_by_value/MinimumMinimumlstm_cell_20/Add_1:z:0-lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22$
"lstm_cell_20/clip_by_value/Minimum
lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_20/clip_by_value/y»
lstm_cell_20/clip_by_valueMaximum&lstm_cell_20/clip_by_value/Minimum:z:0%lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_valueq
lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Const_2q
lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_3
lstm_cell_20/Mul_1Mullstm_cell_20/split:output:1lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul_1
lstm_cell_20/Add_2AddV2lstm_cell_20/Mul_1:z:0lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_2
&lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_20/clip_by_value_1/Minimum/yÉ
$lstm_cell_20/clip_by_value_1/MinimumMinimumlstm_cell_20/Add_2:z:0/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_20/clip_by_value_1/Minimum
lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_20/clip_by_value_1/yÃ
lstm_cell_20/clip_by_value_1Maximum(lstm_cell_20/clip_by_value_1/Minimum:z:0'lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_value_1±
!lstm_cell_20/mul_2/ReadVariableOpReadVariableOp*lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02#
!lstm_cell_20/mul_2/ReadVariableOp¥
lstm_cell_20/mul_2Mul lstm_cell_20/clip_by_value_1:z:0)lstm_cell_20/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_2t
lstm_cell_20/TanhTanhlstm_cell_20/split:output:2*
T0*
_output_shapes

:22
lstm_cell_20/Tanh
lstm_cell_20/mul_3Mullstm_cell_20/clip_by_value:z:0lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_3
lstm_cell_20/add_3AddV2lstm_cell_20/mul_2:z:0lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_20/add_3q
lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Const_4q
lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_5
lstm_cell_20/Mul_4Mullstm_cell_20/split:output:3lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul_4
lstm_cell_20/Add_4AddV2lstm_cell_20/Mul_4:z:0lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_4
&lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_20/clip_by_value_2/Minimum/yÉ
$lstm_cell_20/clip_by_value_2/MinimumMinimumlstm_cell_20/Add_4:z:0/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_20/clip_by_value_2/Minimum
lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_20/clip_by_value_2/yÃ
lstm_cell_20/clip_by_value_2Maximum(lstm_cell_20/clip_by_value_2/Minimum:z:0'lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_value_2s
lstm_cell_20/Tanh_1Tanhlstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_20/Tanh_1
lstm_cell_20/mul_5Mul lstm_cell_20/clip_by_value_2:z:0lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
time
ReadVariableOpReadVariableOp-lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterõ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_20_matmul_readvariableop_resource/lstm_cell_20_matmul_1_readvariableop_1_resource,lstm_cell_20_biasadd_readvariableop_resource*
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
while_body_59644*
condR
while_cond_59643*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm¥
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_20_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_20/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_20_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_20/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_20/BiasAdd/ReadVariableOp#^lstm_cell_20/MatMul/ReadVariableOp%^lstm_cell_20/MatMul_1/ReadVariableOp'^lstm_cell_20/MatMul_1/ReadVariableOp_1"^lstm_cell_20/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:ÿÿÿÿÿÿÿÿÿ2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_20/BiasAdd/ReadVariableOp#lstm_cell_20/BiasAdd/ReadVariableOp2H
"lstm_cell_20/MatMul/ReadVariableOp"lstm_cell_20/MatMul/ReadVariableOp2L
$lstm_cell_20/MatMul_1/ReadVariableOp$lstm_cell_20/MatMul_1/ReadVariableOp2P
&lstm_cell_20/MatMul_1/ReadVariableOp_1&lstm_cell_20/MatMul_1/ReadVariableOp_12F
!lstm_cell_20/mul_2/ReadVariableOp!lstm_cell_20/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
"
_user_specified_name
inputs/0
o
¶
B__inference_lstm_20_layer_call_and_return_conditional_losses_59927
inputs_0>
+lstm_cell_20_matmul_readvariableop_resource:	2È?
-lstm_cell_20_matmul_1_readvariableop_resource:2B
/lstm_cell_20_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_20_biasadd_readvariableop_resource:	È<
*lstm_cell_20_mul_2_readvariableop_resource:2
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_20/BiasAdd/ReadVariableOp¢"lstm_cell_20/MatMul/ReadVariableOp¢$lstm_cell_20/MatMul_1/ReadVariableOp¢&lstm_cell_20/MatMul_1/ReadVariableOp_1¢!lstm_cell_20/mul_2/ReadVariableOp¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_20/MatMul/ReadVariableOpReadVariableOp+lstm_cell_20_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02$
"lstm_cell_20/MatMul/ReadVariableOp¤
lstm_cell_20/MatMulMatMulstrided_slice_1:output:0*lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/MatMulº
$lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$lstm_cell_20/MatMul_1/ReadVariableOpÁ
&lstm_cell_20/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_20_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_20/MatMul_1/ReadVariableOp_1À
lstm_cell_20/MatMul_1MatMul,lstm_cell_20/MatMul_1/ReadVariableOp:value:0.lstm_cell_20/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/MatMul_1
lstm_cell_20/addAddV2lstm_cell_20/MatMul:product:0lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_cell_20/add´
#lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_20_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_20/BiasAdd/ReadVariableOp¤
lstm_cell_20/BiasAddBiasAddlstm_cell_20/add:z:0+lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/BiasAdd~
lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_20/split/split_dimÏ
lstm_cell_20/splitSplit%lstm_cell_20/split/split_dim:output:0lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_20/splitm
lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Constq
lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_1
lstm_cell_20/MulMullstm_cell_20/split:output:0lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul
lstm_cell_20/Add_1AddV2lstm_cell_20/Mul:z:0lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_1
$lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_20/clip_by_value/Minimum/yÃ
"lstm_cell_20/clip_by_value/MinimumMinimumlstm_cell_20/Add_1:z:0-lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22$
"lstm_cell_20/clip_by_value/Minimum
lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_20/clip_by_value/y»
lstm_cell_20/clip_by_valueMaximum&lstm_cell_20/clip_by_value/Minimum:z:0%lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_valueq
lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Const_2q
lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_3
lstm_cell_20/Mul_1Mullstm_cell_20/split:output:1lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul_1
lstm_cell_20/Add_2AddV2lstm_cell_20/Mul_1:z:0lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_2
&lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_20/clip_by_value_1/Minimum/yÉ
$lstm_cell_20/clip_by_value_1/MinimumMinimumlstm_cell_20/Add_2:z:0/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_20/clip_by_value_1/Minimum
lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_20/clip_by_value_1/yÃ
lstm_cell_20/clip_by_value_1Maximum(lstm_cell_20/clip_by_value_1/Minimum:z:0'lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_value_1±
!lstm_cell_20/mul_2/ReadVariableOpReadVariableOp*lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02#
!lstm_cell_20/mul_2/ReadVariableOp¥
lstm_cell_20/mul_2Mul lstm_cell_20/clip_by_value_1:z:0)lstm_cell_20/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_2t
lstm_cell_20/TanhTanhlstm_cell_20/split:output:2*
T0*
_output_shapes

:22
lstm_cell_20/Tanh
lstm_cell_20/mul_3Mullstm_cell_20/clip_by_value:z:0lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_3
lstm_cell_20/add_3AddV2lstm_cell_20/mul_2:z:0lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_20/add_3q
lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Const_4q
lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_5
lstm_cell_20/Mul_4Mullstm_cell_20/split:output:3lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul_4
lstm_cell_20/Add_4AddV2lstm_cell_20/Mul_4:z:0lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_4
&lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_20/clip_by_value_2/Minimum/yÉ
$lstm_cell_20/clip_by_value_2/MinimumMinimumlstm_cell_20/Add_4:z:0/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_20/clip_by_value_2/Minimum
lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_20/clip_by_value_2/yÃ
lstm_cell_20/clip_by_value_2Maximum(lstm_cell_20/clip_by_value_2/Minimum:z:0'lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_value_2s
lstm_cell_20/Tanh_1Tanhlstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_20/Tanh_1
lstm_cell_20/mul_5Mul lstm_cell_20/clip_by_value_2:z:0lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
time
ReadVariableOpReadVariableOp-lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterõ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_20_matmul_readvariableop_resource/lstm_cell_20_matmul_1_readvariableop_1_resource,lstm_cell_20_biasadd_readvariableop_resource*
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
while_body_59822*
condR
while_cond_59821*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm¥
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_20_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_20/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_20_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_20/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_20/BiasAdd/ReadVariableOp#^lstm_cell_20/MatMul/ReadVariableOp%^lstm_cell_20/MatMul_1/ReadVariableOp'^lstm_cell_20/MatMul_1/ReadVariableOp_1"^lstm_cell_20/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:ÿÿÿÿÿÿÿÿÿ2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_20/BiasAdd/ReadVariableOp#lstm_cell_20/BiasAdd/ReadVariableOp2H
"lstm_cell_20/MatMul/ReadVariableOp"lstm_cell_20/MatMul/ReadVariableOp2L
$lstm_cell_20/MatMul_1/ReadVariableOp$lstm_cell_20/MatMul_1/ReadVariableOp2P
&lstm_cell_20/MatMul_1/ReadVariableOp_1&lstm_cell_20/MatMul_1/ReadVariableOp_12F
!lstm_cell_20/mul_2/ReadVariableOp!lstm_cell_20/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
"
_user_specified_name
inputs/0
¾
ô
&sequential_10_lstm_20_while_cond_55105H
Dsequential_10_lstm_20_while_sequential_10_lstm_20_while_loop_counterN
Jsequential_10_lstm_20_while_sequential_10_lstm_20_while_maximum_iterations+
'sequential_10_lstm_20_while_placeholder-
)sequential_10_lstm_20_while_placeholder_1-
)sequential_10_lstm_20_while_placeholder_2-
)sequential_10_lstm_20_while_placeholder_3H
Dsequential_10_lstm_20_while_less_sequential_10_lstm_20_strided_slice_
[sequential_10_lstm_20_while_sequential_10_lstm_20_while_cond_55105___redundant_placeholder0_
[sequential_10_lstm_20_while_sequential_10_lstm_20_while_cond_55105___redundant_placeholder1_
[sequential_10_lstm_20_while_sequential_10_lstm_20_while_cond_55105___redundant_placeholder2_
[sequential_10_lstm_20_while_sequential_10_lstm_20_while_cond_55105___redundant_placeholder3(
$sequential_10_lstm_20_while_identity
Ü
 sequential_10/lstm_20/while/LessLess'sequential_10_lstm_20_while_placeholderDsequential_10_lstm_20_while_less_sequential_10_lstm_20_strided_slice*
T0*
_output_shapes
: 2"
 sequential_10/lstm_20/while/Less
$sequential_10/lstm_20/while/IdentityIdentity$sequential_10/lstm_20/while/Less:z:0*
T0
*
_output_shapes
: 2&
$sequential_10/lstm_20/while/Identity"U
$sequential_10_lstm_20_while_identity-sequential_10/lstm_20/while/Identity:output:0*(
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
ü.
º
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_61071

inputs
states_0
states_11
matmul_readvariableop_resource:	2È5
"matmul_1_readvariableop_1_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp¢MatMul_1/ReadVariableOp_1¢mul_2/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMulu
MatMul_1/ReadVariableOpReadVariableOpstates_0*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOp_1
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
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
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2Ì
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
i
Ë

lstm_21_while_body_58450,
(lstm_21_while_lstm_21_while_loop_counter2
.lstm_21_while_lstm_21_while_maximum_iterations
lstm_21_while_placeholder
lstm_21_while_placeholder_1
lstm_21_while_placeholder_2
lstm_21_while_placeholder_3)
%lstm_21_while_lstm_21_strided_slice_0g
clstm_21_while_tensorarrayv2read_tensorlistgetitem_lstm_21_tensorarrayunstack_tensorlistfromtensor_0N
;lstm_21_while_lstm_cell_21_matmul_readvariableop_resource_0:	ÈP
=lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource_0:	2ÈK
<lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource_0:	È
lstm_21_while_identity
lstm_21_while_identity_1
lstm_21_while_identity_2
lstm_21_while_identity_3
lstm_21_while_identity_4
lstm_21_while_identity_5'
#lstm_21_while_lstm_21_strided_slicee
alstm_21_while_tensorarrayv2read_tensorlistgetitem_lstm_21_tensorarrayunstack_tensorlistfromtensorL
9lstm_21_while_lstm_cell_21_matmul_readvariableop_resource:	ÈN
;lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource:	2ÈI
:lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource:	È¢1lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp¢0lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp¢2lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOpÓ
?lstm_21/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      2A
?lstm_21/while/TensorArrayV2Read/TensorListGetItem/element_shapeú
1lstm_21/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemclstm_21_while_tensorarrayv2read_tensorlistgetitem_lstm_21_tensorarrayunstack_tensorlistfromtensor_0lstm_21_while_placeholderHlstm_21/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype023
1lstm_21/while/TensorArrayV2Read/TensorListGetItemá
0lstm_21/while/lstm_cell_21/MatMul/ReadVariableOpReadVariableOp;lstm_21_while_lstm_cell_21_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype022
0lstm_21/while/lstm_cell_21/MatMul/ReadVariableOpî
!lstm_21/while/lstm_cell_21/MatMulMatMul8lstm_21/while/TensorArrayV2Read/TensorListGetItem:item:08lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2#
!lstm_21/while/lstm_cell_21/MatMulç
2lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp=lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype024
2lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp×
#lstm_21/while/lstm_cell_21/MatMul_1MatMullstm_21_while_placeholder_2:lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2%
#lstm_21/while/lstm_cell_21/MatMul_1Ï
lstm_21/while/lstm_cell_21/addAddV2+lstm_21/while/lstm_cell_21/MatMul:product:0-lstm_21/while/lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2 
lstm_21/while/lstm_cell_21/addà
1lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp<lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype023
1lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOpÜ
"lstm_21/while/lstm_cell_21/BiasAddBiasAdd"lstm_21/while/lstm_cell_21/add:z:09lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2$
"lstm_21/while/lstm_cell_21/BiasAdd
*lstm_21/while/lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2,
*lstm_21/while/lstm_cell_21/split/split_dim
 lstm_21/while/lstm_cell_21/splitSplit3lstm_21/while/lstm_cell_21/split/split_dim:output:0+lstm_21/while/lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2"
 lstm_21/while/lstm_cell_21/split
 lstm_21/while/lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2"
 lstm_21/while/lstm_cell_21/Const
"lstm_21/while/lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_21/while/lstm_cell_21/Const_1Æ
lstm_21/while/lstm_cell_21/MulMul)lstm_21/while/lstm_cell_21/split:output:0)lstm_21/while/lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22 
lstm_21/while/lstm_cell_21/MulÇ
 lstm_21/while/lstm_cell_21/Add_1AddV2"lstm_21/while/lstm_cell_21/Mul:z:0+lstm_21/while/lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/Add_1­
2lstm_21/while/lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?24
2lstm_21/while/lstm_cell_21/clip_by_value/Minimum/yû
0lstm_21/while/lstm_cell_21/clip_by_value/MinimumMinimum$lstm_21/while/lstm_cell_21/Add_1:z:0;lstm_21/while/lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222
0lstm_21/while/lstm_cell_21/clip_by_value/Minimum
*lstm_21/while/lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_21/while/lstm_cell_21/clip_by_value/yó
(lstm_21/while/lstm_cell_21/clip_by_valueMaximum4lstm_21/while/lstm_cell_21/clip_by_value/Minimum:z:03lstm_21/while/lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22*
(lstm_21/while/lstm_cell_21/clip_by_value
"lstm_21/while/lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_21/while/lstm_cell_21/Const_2
"lstm_21/while/lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_21/while/lstm_cell_21/Const_3Ì
 lstm_21/while/lstm_cell_21/Mul_1Mul)lstm_21/while/lstm_cell_21/split:output:1+lstm_21/while/lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/Mul_1É
 lstm_21/while/lstm_cell_21/Add_2AddV2$lstm_21/while/lstm_cell_21/Mul_1:z:0+lstm_21/while/lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/Add_2±
4lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum/y
2lstm_21/while/lstm_cell_21/clip_by_value_1/MinimumMinimum$lstm_21/while/lstm_cell_21/Add_2:z:0=lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:224
2lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum¡
,lstm_21/while/lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_21/while/lstm_cell_21/clip_by_value_1/yû
*lstm_21/while/lstm_cell_21/clip_by_value_1Maximum6lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum:z:05lstm_21/while/lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22,
*lstm_21/while/lstm_cell_21/clip_by_value_1Á
 lstm_21/while/lstm_cell_21/mul_2Mul.lstm_21/while/lstm_cell_21/clip_by_value_1:z:0lstm_21_while_placeholder_3*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/mul_2
lstm_21/while/lstm_cell_21/TanhTanh)lstm_21/while/lstm_cell_21/split:output:2*
T0*
_output_shapes

:22!
lstm_21/while/lstm_cell_21/TanhÇ
 lstm_21/while/lstm_cell_21/mul_3Mul,lstm_21/while/lstm_cell_21/clip_by_value:z:0#lstm_21/while/lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/mul_3Â
 lstm_21/while/lstm_cell_21/add_3AddV2$lstm_21/while/lstm_cell_21/mul_2:z:0$lstm_21/while/lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/add_3
"lstm_21/while/lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_21/while/lstm_cell_21/Const_4
"lstm_21/while/lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_21/while/lstm_cell_21/Const_5Ì
 lstm_21/while/lstm_cell_21/Mul_4Mul)lstm_21/while/lstm_cell_21/split:output:3+lstm_21/while/lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/Mul_4É
 lstm_21/while/lstm_cell_21/Add_4AddV2$lstm_21/while/lstm_cell_21/Mul_4:z:0+lstm_21/while/lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/Add_4±
4lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum/y
2lstm_21/while/lstm_cell_21/clip_by_value_2/MinimumMinimum$lstm_21/while/lstm_cell_21/Add_4:z:0=lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:224
2lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum¡
,lstm_21/while/lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_21/while/lstm_cell_21/clip_by_value_2/yû
*lstm_21/while/lstm_cell_21/clip_by_value_2Maximum6lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum:z:05lstm_21/while/lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22,
*lstm_21/while/lstm_cell_21/clip_by_value_2
!lstm_21/while/lstm_cell_21/Tanh_1Tanh$lstm_21/while/lstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22#
!lstm_21/while/lstm_cell_21/Tanh_1Ë
 lstm_21/while/lstm_cell_21/mul_5Mul.lstm_21/while/lstm_cell_21/clip_by_value_2:z:0%lstm_21/while/lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22"
 lstm_21/while/lstm_cell_21/mul_5
2lstm_21/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_21_while_placeholder_1lstm_21_while_placeholder$lstm_21/while/lstm_cell_21/mul_5:z:0*
_output_shapes
: *
element_dtype024
2lstm_21/while/TensorArrayV2Write/TensorListSetIteml
lstm_21/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_21/while/add/y
lstm_21/while/addAddV2lstm_21_while_placeholderlstm_21/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_21/while/addp
lstm_21/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_21/while/add_1/y
lstm_21/while/add_1AddV2(lstm_21_while_lstm_21_while_loop_counterlstm_21/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_21/while/add_1
lstm_21/while/IdentityIdentitylstm_21/while/add_1:z:0^lstm_21/while/NoOp*
T0*
_output_shapes
: 2
lstm_21/while/Identity¦
lstm_21/while/Identity_1Identity.lstm_21_while_lstm_21_while_maximum_iterations^lstm_21/while/NoOp*
T0*
_output_shapes
: 2
lstm_21/while/Identity_1
lstm_21/while/Identity_2Identitylstm_21/while/add:z:0^lstm_21/while/NoOp*
T0*
_output_shapes
: 2
lstm_21/while/Identity_2º
lstm_21/while/Identity_3IdentityBlstm_21/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_21/while/NoOp*
T0*
_output_shapes
: 2
lstm_21/while/Identity_3¤
lstm_21/while/Identity_4Identity$lstm_21/while/lstm_cell_21/mul_5:z:0^lstm_21/while/NoOp*
T0*
_output_shapes

:22
lstm_21/while/Identity_4¤
lstm_21/while/Identity_5Identity$lstm_21/while/lstm_cell_21/add_3:z:0^lstm_21/while/NoOp*
T0*
_output_shapes

:22
lstm_21/while/Identity_5
lstm_21/while/NoOpNoOp2^lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp1^lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp3^lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_21/while/NoOp"9
lstm_21_while_identitylstm_21/while/Identity:output:0"=
lstm_21_while_identity_1!lstm_21/while/Identity_1:output:0"=
lstm_21_while_identity_2!lstm_21/while/Identity_2:output:0"=
lstm_21_while_identity_3!lstm_21/while/Identity_3:output:0"=
lstm_21_while_identity_4!lstm_21/while/Identity_4:output:0"=
lstm_21_while_identity_5!lstm_21/while/Identity_5:output:0"L
#lstm_21_while_lstm_21_strided_slice%lstm_21_while_lstm_21_strided_slice_0"z
:lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource<lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource_0"|
;lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource=lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource_0"x
9lstm_21_while_lstm_cell_21_matmul_readvariableop_resource;lstm_21_while_lstm_cell_21_matmul_readvariableop_resource_0"È
alstm_21_while_tensorarrayv2read_tensorlistgetitem_lstm_21_tensorarrayunstack_tensorlistfromtensorclstm_21_while_tensorarrayv2read_tensorlistgetitem_lstm_21_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2f
1lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp1lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp2d
0lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp0lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp2h
2lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp2lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp: 
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
ô.
¸
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_61166

inputs

states
states_11
matmul_readvariableop_resource:	2È5
"matmul_1_readvariableop_1_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp¢MatMul_1/ReadVariableOp_1¢mul_2/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMuls
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes
:*
dtype02
MatMul_1/ReadVariableOp
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOp_1
MatMul_1BatchMatMulV2MatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:2

MatMul_1[
addAddV2MatMul:product:0MatMul_1:output:0*
T0*
_output_shapes
:2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
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
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2Ì
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
¨
¼
while_cond_57469
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_57469___redundant_placeholder03
/while_while_cond_57469___redundant_placeholder13
/while_while_cond_57469___redundant_placeholder23
/while_while_cond_57469___redundant_placeholder3
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
	
ð
'__inference_lstm_20_layer_call_fn_60313
inputs_0
unknown:2
	unknown_0:2
	unknown_1:	2È
	unknown_2:	2È
	unknown_3:	È
identity¢StatefulPartitionedCall
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_20_layer_call_and_return_conditional_losses_565352
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:ÿÿÿÿÿÿÿÿÿ2: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
"
_user_specified_name
inputs/0


Ü
lstm_20_while_cond_58259,
(lstm_20_while_lstm_20_while_loop_counter2
.lstm_20_while_lstm_20_while_maximum_iterations
lstm_20_while_placeholder
lstm_20_while_placeholder_1
lstm_20_while_placeholder_2
lstm_20_while_placeholder_3,
(lstm_20_while_less_lstm_20_strided_sliceC
?lstm_20_while_lstm_20_while_cond_58259___redundant_placeholder0C
?lstm_20_while_lstm_20_while_cond_58259___redundant_placeholder1C
?lstm_20_while_lstm_20_while_cond_58259___redundant_placeholder2C
?lstm_20_while_lstm_20_while_cond_58259___redundant_placeholder3
lstm_20_while_identity

lstm_20/while/LessLesslstm_20_while_placeholder(lstm_20_while_less_lstm_20_strided_slice*
T0*
_output_shapes
: 2
lstm_20/while/Lessu
lstm_20/while/IdentityIdentitylstm_20/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_20/while/Identity"9
lstm_20_while_identitylstm_20/while/Identity:output:0*(
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
ÔY
Ë
while_body_56993
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_21_matmul_readvariableop_resource_0:	ÈH
5while_lstm_cell_21_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_21_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_21_matmul_readvariableop_resource:	ÈF
3while_lstm_cell_21_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_21_biasadd_readvariableop_resource:	È¢)while/lstm_cell_21/BiasAdd/ReadVariableOp¢(while/lstm_cell_21/MatMul/ReadVariableOp¢*while/lstm_cell_21/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_21/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_21_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02*
(while/lstm_cell_21/MatMul/ReadVariableOpÎ
while/lstm_cell_21/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/MatMulÏ
*while/lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_21_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_21/MatMul_1/ReadVariableOp·
while/lstm_cell_21/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_21/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/MatMul_1¯
while/lstm_cell_21/addAddV2#while/lstm_cell_21/MatMul:product:0%while/lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/addÈ
)while/lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_21_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_21/BiasAdd/ReadVariableOp¼
while/lstm_cell_21/BiasAddBiasAddwhile/lstm_cell_21/add:z:01while/lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/BiasAdd
"while/lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_21/split/split_dimç
while/lstm_cell_21/splitSplit+while/lstm_cell_21/split/split_dim:output:0#while/lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_21/splity
while/lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const}
while/lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_1¦
while/lstm_cell_21/MulMul!while/lstm_cell_21/split:output:0!while/lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul§
while/lstm_cell_21/Add_1AddV2while/lstm_cell_21/Mul:z:0#while/lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_1
*while/lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_21/clip_by_value/Minimum/yÛ
(while/lstm_cell_21/clip_by_value/MinimumMinimumwhile/lstm_cell_21/Add_1:z:03while/lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(while/lstm_cell_21/clip_by_value/Minimum
"while/lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_21/clip_by_value/yÓ
 while/lstm_cell_21/clip_by_valueMaximum,while/lstm_cell_21/clip_by_value/Minimum:z:0+while/lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 while/lstm_cell_21/clip_by_value}
while/lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const_2}
while/lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_3¬
while/lstm_cell_21/Mul_1Mul!while/lstm_cell_21/split:output:1#while/lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul_1©
while/lstm_cell_21/Add_2AddV2while/lstm_cell_21/Mul_1:z:0#while/lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_2¡
,while/lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_21/clip_by_value_1/Minimum/yá
*while/lstm_cell_21/clip_by_value_1/MinimumMinimumwhile/lstm_cell_21/Add_2:z:05while/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_21/clip_by_value_1/Minimum
$while/lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_21/clip_by_value_1/yÛ
"while/lstm_cell_21/clip_by_value_1Maximum.while/lstm_cell_21/clip_by_value_1/Minimum:z:0-while/lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_21/clip_by_value_1¡
while/lstm_cell_21/mul_2Mul&while/lstm_cell_21/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_2
while/lstm_cell_21/TanhTanh!while/lstm_cell_21/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_21/Tanh§
while/lstm_cell_21/mul_3Mul$while/lstm_cell_21/clip_by_value:z:0while/lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_3¢
while/lstm_cell_21/add_3AddV2while/lstm_cell_21/mul_2:z:0while/lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_21/add_3}
while/lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const_4}
while/lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_5¬
while/lstm_cell_21/Mul_4Mul!while/lstm_cell_21/split:output:3#while/lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul_4©
while/lstm_cell_21/Add_4AddV2while/lstm_cell_21/Mul_4:z:0#while/lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_4¡
,while/lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_21/clip_by_value_2/Minimum/yá
*while/lstm_cell_21/clip_by_value_2/MinimumMinimumwhile/lstm_cell_21/Add_4:z:05while/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_21/clip_by_value_2/Minimum
$while/lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_21/clip_by_value_2/yÛ
"while/lstm_cell_21/clip_by_value_2Maximum.while/lstm_cell_21/clip_by_value_2/Minimum:z:0-while/lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_21/clip_by_value_2
while/lstm_cell_21/Tanh_1Tanhwhile/lstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Tanh_1«
while/lstm_cell_21/mul_5Mul&while/lstm_cell_21/clip_by_value_2:z:0while/lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_21/mul_5:z:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identitywhile/lstm_cell_21/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_21/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_21/BiasAdd/ReadVariableOp)^while/lstm_cell_21/MatMul/ReadVariableOp+^while/lstm_cell_21/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_21_biasadd_readvariableop_resource4while_lstm_cell_21_biasadd_readvariableop_resource_0"l
3while_lstm_cell_21_matmul_1_readvariableop_resource5while_lstm_cell_21_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_21_matmul_readvariableop_resource3while_lstm_cell_21_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_21/BiasAdd/ReadVariableOp)while/lstm_cell_21/BiasAdd/ReadVariableOp2T
(while/lstm_cell_21/MatMul/ReadVariableOp(while/lstm_cell_21/MatMul/ReadVariableOp2X
*while/lstm_cell_21/MatMul_1/ReadVariableOp*while/lstm_cell_21/MatMul_1/ReadVariableOp: 
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
¨
¼
while_cond_58871
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_58871___redundant_placeholder03
/while_while_cond_58871___redundant_placeholder13
/while_while_cond_58871___redundant_placeholder23
/while_while_cond_58871___redundant_placeholder3
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
ß
÷
H__inference_sequential_10_layer_call_and_return_conditional_losses_57860

inputs 
lstm_21_57830:	È
lstm_21_57832:2 
lstm_21_57834:	2È
lstm_21_57836:	È
lstm_21_57838:2 
lstm_20_57841:	2È
lstm_20_57843:2 
lstm_20_57845:	2È
lstm_20_57847:	È
lstm_20_57849:2+
time_distributed_10_57852:2'
time_distributed_10_57854:
identity¢lstm_20/StatefulPartitionedCall¢lstm_21/StatefulPartitionedCall¢+time_distributed_10/StatefulPartitionedCall¸
lstm_21/StatefulPartitionedCallStatefulPartitionedCallinputslstm_21_57830lstm_21_57832lstm_21_57834lstm_21_57836lstm_21_57838*
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
GPU 2J 8 *K
fFRD
B__inference_lstm_21_layer_call_and_return_conditional_losses_577832!
lstm_21/StatefulPartitionedCallÚ
lstm_20/StatefulPartitionedCallStatefulPartitionedCall(lstm_21/StatefulPartitionedCall:output:0lstm_20_57841lstm_20_57843lstm_20_57845lstm_20_57847lstm_20_57849*
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
GPU 2J 8 *K
fFRD
B__inference_lstm_20_layer_call_and_return_conditional_losses_575752!
lstm_20/StatefulPartitionedCallå
+time_distributed_10/StatefulPartitionedCallStatefulPartitionedCall(lstm_20/StatefulPartitionedCall:output:0time_distributed_10_57852time_distributed_10_57854*
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
GPU 2J 8 *W
fRRP
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_573732-
+time_distributed_10/StatefulPartitionedCall
!time_distributed_10/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2#
!time_distributed_10/Reshape/shapeÄ
time_distributed_10/ReshapeReshape(lstm_20/StatefulPartitionedCall:output:0*time_distributed_10/Reshape/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_10/Reshape
IdentityIdentity4time_distributed_10/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:
2

IdentityÀ
NoOpNoOp ^lstm_20/StatefulPartitionedCall ^lstm_21/StatefulPartitionedCall,^time_distributed_10/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 2B
lstm_20/StatefulPartitionedCalllstm_20/StatefulPartitionedCall2B
lstm_21/StatefulPartitionedCalllstm_21/StatefulPartitionedCall2Z
+time_distributed_10/StatefulPartitionedCall+time_distributed_10/StatefulPartitionedCall:J F
"
_output_shapes
:

 
_user_specified_nameinputs
¨
¼
while_cond_55689
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_55689___redundant_placeholder03
/while_while_cond_55689___redundant_placeholder13
/while_while_cond_55689___redundant_placeholder23
/while_while_cond_55689___redundant_placeholder3
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


Ü
lstm_21_while_cond_58449,
(lstm_21_while_lstm_21_while_loop_counter2
.lstm_21_while_lstm_21_while_maximum_iterations
lstm_21_while_placeholder
lstm_21_while_placeholder_1
lstm_21_while_placeholder_2
lstm_21_while_placeholder_3,
(lstm_21_while_less_lstm_21_strided_sliceC
?lstm_21_while_lstm_21_while_cond_58449___redundant_placeholder0C
?lstm_21_while_lstm_21_while_cond_58449___redundant_placeholder1C
?lstm_21_while_lstm_21_while_cond_58449___redundant_placeholder2C
?lstm_21_while_lstm_21_while_cond_58449___redundant_placeholder3
lstm_21_while_identity

lstm_21/while/LessLesslstm_21_while_placeholder(lstm_21_while_less_lstm_21_strided_slice*
T0*
_output_shapes
: 2
lstm_21/while/Lessu
lstm_21/while/IdentityIdentitylstm_21/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_21/while/Identity"9
lstm_21_while_identitylstm_21/while/Identity:output:0*(
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
ñ
 
3__inference_time_distributed_10_layer_call_fn_60440

inputs
unknown:2
	unknown_0:
identity¢StatefulPartitionedCallù
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
GPU 2J 8 *W
fRRP
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_573122
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
å$
Ø
while_body_56096
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
while_lstm_cell_20_56173_0:	2È-
while_lstm_cell_20_56175_0:	2È)
while_lstm_cell_20_56177_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
while_lstm_cell_20_56173:	2È+
while_lstm_cell_20_56175:	2È'
while_lstm_cell_20_56177:	È¢*while/lstm_cell_20/StatefulPartitionedCallÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÂ
*while/lstm_cell_20/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_20_56173_0while_lstm_cell_20_56175_0while_lstm_cell_20_56177_0*
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_561722,
*while/lstm_cell_20/StatefulPartitionedCall÷
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder3while/lstm_cell_20/StatefulPartitionedCall:output:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identity3while/lstm_cell_20/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identity3while/lstm_cell_20/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5

while/NoOpNoOp+^while/lstm_cell_20/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"6
while_lstm_cell_20_56173while_lstm_cell_20_56173_0"6
while_lstm_cell_20_56175while_lstm_cell_20_56175_0"6
while_lstm_cell_20_56177while_lstm_cell_20_56177_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2X
*while/lstm_cell_20/StatefulPartitionedCall*while/lstm_cell_20/StatefulPartitionedCall: 
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
¨
¼
while_cond_59049
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_59049___redundant_placeholder03
/while_while_cond_59049___redundant_placeholder13
/while_while_cond_59049___redundant_placeholder23
/while_while_cond_59049___redundant_placeholder3
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
ùn
´
B__inference_lstm_21_layer_call_and_return_conditional_losses_57098

inputs>
+lstm_cell_21_matmul_readvariableop_resource:	È?
-lstm_cell_21_matmul_1_readvariableop_resource:2B
/lstm_cell_21_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_21_biasadd_readvariableop_resource:	È<
*lstm_cell_21_mul_2_readvariableop_resource:2
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_21/BiasAdd/ReadVariableOp¢"lstm_cell_21/MatMul/ReadVariableOp¢$lstm_cell_21/MatMul_1/ReadVariableOp¢&lstm_cell_21/MatMul_1/ReadVariableOp_1¢!lstm_cell_21/mul_2/ReadVariableOp¢whileu
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_21/MatMul/ReadVariableOpReadVariableOp+lstm_cell_21_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02$
"lstm_cell_21/MatMul/ReadVariableOp¤
lstm_cell_21/MatMulMatMulstrided_slice_1:output:0*lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/MatMulº
$lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$lstm_cell_21/MatMul_1/ReadVariableOpÁ
&lstm_cell_21/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_21_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_21/MatMul_1/ReadVariableOp_1À
lstm_cell_21/MatMul_1MatMul,lstm_cell_21/MatMul_1/ReadVariableOp:value:0.lstm_cell_21/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/MatMul_1
lstm_cell_21/addAddV2lstm_cell_21/MatMul:product:0lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_cell_21/add´
#lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_21_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_21/BiasAdd/ReadVariableOp¤
lstm_cell_21/BiasAddBiasAddlstm_cell_21/add:z:0+lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/BiasAdd~
lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_21/split/split_dimÏ
lstm_cell_21/splitSplit%lstm_cell_21/split/split_dim:output:0lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_21/splitm
lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Constq
lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_1
lstm_cell_21/MulMullstm_cell_21/split:output:0lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul
lstm_cell_21/Add_1AddV2lstm_cell_21/Mul:z:0lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_1
$lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_21/clip_by_value/Minimum/yÃ
"lstm_cell_21/clip_by_value/MinimumMinimumlstm_cell_21/Add_1:z:0-lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22$
"lstm_cell_21/clip_by_value/Minimum
lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_21/clip_by_value/y»
lstm_cell_21/clip_by_valueMaximum&lstm_cell_21/clip_by_value/Minimum:z:0%lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_valueq
lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Const_2q
lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_3
lstm_cell_21/Mul_1Mullstm_cell_21/split:output:1lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul_1
lstm_cell_21/Add_2AddV2lstm_cell_21/Mul_1:z:0lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_2
&lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_21/clip_by_value_1/Minimum/yÉ
$lstm_cell_21/clip_by_value_1/MinimumMinimumlstm_cell_21/Add_2:z:0/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_21/clip_by_value_1/Minimum
lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_21/clip_by_value_1/yÃ
lstm_cell_21/clip_by_value_1Maximum(lstm_cell_21/clip_by_value_1/Minimum:z:0'lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_value_1±
!lstm_cell_21/mul_2/ReadVariableOpReadVariableOp*lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02#
!lstm_cell_21/mul_2/ReadVariableOp¥
lstm_cell_21/mul_2Mul lstm_cell_21/clip_by_value_1:z:0)lstm_cell_21/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_2t
lstm_cell_21/TanhTanhlstm_cell_21/split:output:2*
T0*
_output_shapes

:22
lstm_cell_21/Tanh
lstm_cell_21/mul_3Mullstm_cell_21/clip_by_value:z:0lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_3
lstm_cell_21/add_3AddV2lstm_cell_21/mul_2:z:0lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_21/add_3q
lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Const_4q
lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_5
lstm_cell_21/Mul_4Mullstm_cell_21/split:output:3lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul_4
lstm_cell_21/Add_4AddV2lstm_cell_21/Mul_4:z:0lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_4
&lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_21/clip_by_value_2/Minimum/yÉ
$lstm_cell_21/clip_by_value_2/MinimumMinimumlstm_cell_21/Add_4:z:0/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_21/clip_by_value_2/Minimum
lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_21/clip_by_value_2/yÃ
lstm_cell_21/clip_by_value_2Maximum(lstm_cell_21/clip_by_value_2/Minimum:z:0'lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_value_2s
lstm_cell_21/Tanh_1Tanhlstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_21/Tanh_1
lstm_cell_21/mul_5Mul lstm_cell_21/clip_by_value_2:z:0lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
time
ReadVariableOpReadVariableOp-lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterõ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_21_matmul_readvariableop_resource/lstm_cell_21_matmul_1_readvariableop_1_resource,lstm_cell_21_biasadd_readvariableop_resource*
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
while_body_56993*
condR
while_cond_56992*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm
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
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_21_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_21/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_21_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_21/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_21/BiasAdd/ReadVariableOp#^lstm_cell_21/MatMul/ReadVariableOp%^lstm_cell_21/MatMul_1/ReadVariableOp'^lstm_cell_21/MatMul_1/ReadVariableOp_1"^lstm_cell_21/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_21/BiasAdd/ReadVariableOp#lstm_cell_21/BiasAdd/ReadVariableOp2H
"lstm_cell_21/MatMul/ReadVariableOp"lstm_cell_21/MatMul/ReadVariableOp2L
$lstm_cell_21/MatMul_1/ReadVariableOp$lstm_cell_21/MatMul_1/ReadVariableOp2P
&lstm_cell_21/MatMul_1/ReadVariableOp_1&lstm_cell_21/MatMul_1/ReadVariableOp_12F
!lstm_cell_21/mul_2/ReadVariableOp!lstm_cell_21/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:

 
_user_specified_nameinputs
i
Ë

lstm_20_while_body_58624,
(lstm_20_while_lstm_20_while_loop_counter2
.lstm_20_while_lstm_20_while_maximum_iterations
lstm_20_while_placeholder
lstm_20_while_placeholder_1
lstm_20_while_placeholder_2
lstm_20_while_placeholder_3)
%lstm_20_while_lstm_20_strided_slice_0g
clstm_20_while_tensorarrayv2read_tensorlistgetitem_lstm_20_tensorarrayunstack_tensorlistfromtensor_0N
;lstm_20_while_lstm_cell_20_matmul_readvariableop_resource_0:	2ÈP
=lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource_0:	2ÈK
<lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource_0:	È
lstm_20_while_identity
lstm_20_while_identity_1
lstm_20_while_identity_2
lstm_20_while_identity_3
lstm_20_while_identity_4
lstm_20_while_identity_5'
#lstm_20_while_lstm_20_strided_slicee
alstm_20_while_tensorarrayv2read_tensorlistgetitem_lstm_20_tensorarrayunstack_tensorlistfromtensorL
9lstm_20_while_lstm_cell_20_matmul_readvariableop_resource:	2ÈN
;lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource:	2ÈI
:lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource:	È¢1lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp¢0lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp¢2lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOpÓ
?lstm_20/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2A
?lstm_20/while/TensorArrayV2Read/TensorListGetItem/element_shapeú
1lstm_20/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemclstm_20_while_tensorarrayv2read_tensorlistgetitem_lstm_20_tensorarrayunstack_tensorlistfromtensor_0lstm_20_while_placeholderHlstm_20/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype023
1lstm_20/while/TensorArrayV2Read/TensorListGetItemá
0lstm_20/while/lstm_cell_20/MatMul/ReadVariableOpReadVariableOp;lstm_20_while_lstm_cell_20_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype022
0lstm_20/while/lstm_cell_20/MatMul/ReadVariableOpî
!lstm_20/while/lstm_cell_20/MatMulMatMul8lstm_20/while/TensorArrayV2Read/TensorListGetItem:item:08lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2#
!lstm_20/while/lstm_cell_20/MatMulç
2lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp=lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype024
2lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp×
#lstm_20/while/lstm_cell_20/MatMul_1MatMullstm_20_while_placeholder_2:lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2%
#lstm_20/while/lstm_cell_20/MatMul_1Ï
lstm_20/while/lstm_cell_20/addAddV2+lstm_20/while/lstm_cell_20/MatMul:product:0-lstm_20/while/lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2 
lstm_20/while/lstm_cell_20/addà
1lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp<lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype023
1lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOpÜ
"lstm_20/while/lstm_cell_20/BiasAddBiasAdd"lstm_20/while/lstm_cell_20/add:z:09lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2$
"lstm_20/while/lstm_cell_20/BiasAdd
*lstm_20/while/lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2,
*lstm_20/while/lstm_cell_20/split/split_dim
 lstm_20/while/lstm_cell_20/splitSplit3lstm_20/while/lstm_cell_20/split/split_dim:output:0+lstm_20/while/lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2"
 lstm_20/while/lstm_cell_20/split
 lstm_20/while/lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2"
 lstm_20/while/lstm_cell_20/Const
"lstm_20/while/lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_20/while/lstm_cell_20/Const_1Æ
lstm_20/while/lstm_cell_20/MulMul)lstm_20/while/lstm_cell_20/split:output:0)lstm_20/while/lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22 
lstm_20/while/lstm_cell_20/MulÇ
 lstm_20/while/lstm_cell_20/Add_1AddV2"lstm_20/while/lstm_cell_20/Mul:z:0+lstm_20/while/lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/Add_1­
2lstm_20/while/lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?24
2lstm_20/while/lstm_cell_20/clip_by_value/Minimum/yû
0lstm_20/while/lstm_cell_20/clip_by_value/MinimumMinimum$lstm_20/while/lstm_cell_20/Add_1:z:0;lstm_20/while/lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222
0lstm_20/while/lstm_cell_20/clip_by_value/Minimum
*lstm_20/while/lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_20/while/lstm_cell_20/clip_by_value/yó
(lstm_20/while/lstm_cell_20/clip_by_valueMaximum4lstm_20/while/lstm_cell_20/clip_by_value/Minimum:z:03lstm_20/while/lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22*
(lstm_20/while/lstm_cell_20/clip_by_value
"lstm_20/while/lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_20/while/lstm_cell_20/Const_2
"lstm_20/while/lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_20/while/lstm_cell_20/Const_3Ì
 lstm_20/while/lstm_cell_20/Mul_1Mul)lstm_20/while/lstm_cell_20/split:output:1+lstm_20/while/lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/Mul_1É
 lstm_20/while/lstm_cell_20/Add_2AddV2$lstm_20/while/lstm_cell_20/Mul_1:z:0+lstm_20/while/lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/Add_2±
4lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum/y
2lstm_20/while/lstm_cell_20/clip_by_value_1/MinimumMinimum$lstm_20/while/lstm_cell_20/Add_2:z:0=lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:224
2lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum¡
,lstm_20/while/lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_20/while/lstm_cell_20/clip_by_value_1/yû
*lstm_20/while/lstm_cell_20/clip_by_value_1Maximum6lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum:z:05lstm_20/while/lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22,
*lstm_20/while/lstm_cell_20/clip_by_value_1Á
 lstm_20/while/lstm_cell_20/mul_2Mul.lstm_20/while/lstm_cell_20/clip_by_value_1:z:0lstm_20_while_placeholder_3*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/mul_2
lstm_20/while/lstm_cell_20/TanhTanh)lstm_20/while/lstm_cell_20/split:output:2*
T0*
_output_shapes

:22!
lstm_20/while/lstm_cell_20/TanhÇ
 lstm_20/while/lstm_cell_20/mul_3Mul,lstm_20/while/lstm_cell_20/clip_by_value:z:0#lstm_20/while/lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/mul_3Â
 lstm_20/while/lstm_cell_20/add_3AddV2$lstm_20/while/lstm_cell_20/mul_2:z:0$lstm_20/while/lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/add_3
"lstm_20/while/lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_20/while/lstm_cell_20/Const_4
"lstm_20/while/lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_20/while/lstm_cell_20/Const_5Ì
 lstm_20/while/lstm_cell_20/Mul_4Mul)lstm_20/while/lstm_cell_20/split:output:3+lstm_20/while/lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/Mul_4É
 lstm_20/while/lstm_cell_20/Add_4AddV2$lstm_20/while/lstm_cell_20/Mul_4:z:0+lstm_20/while/lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/Add_4±
4lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum/y
2lstm_20/while/lstm_cell_20/clip_by_value_2/MinimumMinimum$lstm_20/while/lstm_cell_20/Add_4:z:0=lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:224
2lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum¡
,lstm_20/while/lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_20/while/lstm_cell_20/clip_by_value_2/yû
*lstm_20/while/lstm_cell_20/clip_by_value_2Maximum6lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum:z:05lstm_20/while/lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22,
*lstm_20/while/lstm_cell_20/clip_by_value_2
!lstm_20/while/lstm_cell_20/Tanh_1Tanh$lstm_20/while/lstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22#
!lstm_20/while/lstm_cell_20/Tanh_1Ë
 lstm_20/while/lstm_cell_20/mul_5Mul.lstm_20/while/lstm_cell_20/clip_by_value_2:z:0%lstm_20/while/lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/mul_5
2lstm_20/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_20_while_placeholder_1lstm_20_while_placeholder$lstm_20/while/lstm_cell_20/mul_5:z:0*
_output_shapes
: *
element_dtype024
2lstm_20/while/TensorArrayV2Write/TensorListSetIteml
lstm_20/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_20/while/add/y
lstm_20/while/addAddV2lstm_20_while_placeholderlstm_20/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_20/while/addp
lstm_20/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_20/while/add_1/y
lstm_20/while/add_1AddV2(lstm_20_while_lstm_20_while_loop_counterlstm_20/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_20/while/add_1
lstm_20/while/IdentityIdentitylstm_20/while/add_1:z:0^lstm_20/while/NoOp*
T0*
_output_shapes
: 2
lstm_20/while/Identity¦
lstm_20/while/Identity_1Identity.lstm_20_while_lstm_20_while_maximum_iterations^lstm_20/while/NoOp*
T0*
_output_shapes
: 2
lstm_20/while/Identity_1
lstm_20/while/Identity_2Identitylstm_20/while/add:z:0^lstm_20/while/NoOp*
T0*
_output_shapes
: 2
lstm_20/while/Identity_2º
lstm_20/while/Identity_3IdentityBlstm_20/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_20/while/NoOp*
T0*
_output_shapes
: 2
lstm_20/while/Identity_3¤
lstm_20/while/Identity_4Identity$lstm_20/while/lstm_cell_20/mul_5:z:0^lstm_20/while/NoOp*
T0*
_output_shapes

:22
lstm_20/while/Identity_4¤
lstm_20/while/Identity_5Identity$lstm_20/while/lstm_cell_20/add_3:z:0^lstm_20/while/NoOp*
T0*
_output_shapes

:22
lstm_20/while/Identity_5
lstm_20/while/NoOpNoOp2^lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp1^lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp3^lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_20/while/NoOp"9
lstm_20_while_identitylstm_20/while/Identity:output:0"=
lstm_20_while_identity_1!lstm_20/while/Identity_1:output:0"=
lstm_20_while_identity_2!lstm_20/while/Identity_2:output:0"=
lstm_20_while_identity_3!lstm_20/while/Identity_3:output:0"=
lstm_20_while_identity_4!lstm_20/while/Identity_4:output:0"=
lstm_20_while_identity_5!lstm_20/while/Identity_5:output:0"L
#lstm_20_while_lstm_20_strided_slice%lstm_20_while_lstm_20_strided_slice_0"z
:lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource<lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource_0"|
;lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource=lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource_0"x
9lstm_20_while_lstm_cell_20_matmul_readvariableop_resource;lstm_20_while_lstm_cell_20_matmul_readvariableop_resource_0"È
alstm_20_while_tensorarrayv2read_tensorlistgetitem_lstm_20_tensorarrayunstack_tensorlistfromtensorclstm_20_while_tensorarrayv2read_tensorlistgetitem_lstm_20_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2f
1lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp1lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp2d
0lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp0lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp2h
2lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp2lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp: 
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
ùn
´
B__inference_lstm_20_layer_call_and_return_conditional_losses_57287

inputs>
+lstm_cell_20_matmul_readvariableop_resource:	2È?
-lstm_cell_20_matmul_1_readvariableop_resource:2B
/lstm_cell_20_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_20_biasadd_readvariableop_resource:	È<
*lstm_cell_20_mul_2_readvariableop_resource:2
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_20/BiasAdd/ReadVariableOp¢"lstm_cell_20/MatMul/ReadVariableOp¢$lstm_cell_20/MatMul_1/ReadVariableOp¢&lstm_cell_20/MatMul_1/ReadVariableOp_1¢!lstm_cell_20/mul_2/ReadVariableOp¢whileu
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_20/MatMul/ReadVariableOpReadVariableOp+lstm_cell_20_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02$
"lstm_cell_20/MatMul/ReadVariableOp¤
lstm_cell_20/MatMulMatMulstrided_slice_1:output:0*lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/MatMulº
$lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$lstm_cell_20/MatMul_1/ReadVariableOpÁ
&lstm_cell_20/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_20_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_20/MatMul_1/ReadVariableOp_1À
lstm_cell_20/MatMul_1MatMul,lstm_cell_20/MatMul_1/ReadVariableOp:value:0.lstm_cell_20/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/MatMul_1
lstm_cell_20/addAddV2lstm_cell_20/MatMul:product:0lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_cell_20/add´
#lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_20_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_20/BiasAdd/ReadVariableOp¤
lstm_cell_20/BiasAddBiasAddlstm_cell_20/add:z:0+lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/BiasAdd~
lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_20/split/split_dimÏ
lstm_cell_20/splitSplit%lstm_cell_20/split/split_dim:output:0lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_20/splitm
lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Constq
lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_1
lstm_cell_20/MulMullstm_cell_20/split:output:0lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul
lstm_cell_20/Add_1AddV2lstm_cell_20/Mul:z:0lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_1
$lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_20/clip_by_value/Minimum/yÃ
"lstm_cell_20/clip_by_value/MinimumMinimumlstm_cell_20/Add_1:z:0-lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22$
"lstm_cell_20/clip_by_value/Minimum
lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_20/clip_by_value/y»
lstm_cell_20/clip_by_valueMaximum&lstm_cell_20/clip_by_value/Minimum:z:0%lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_valueq
lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Const_2q
lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_3
lstm_cell_20/Mul_1Mullstm_cell_20/split:output:1lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul_1
lstm_cell_20/Add_2AddV2lstm_cell_20/Mul_1:z:0lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_2
&lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_20/clip_by_value_1/Minimum/yÉ
$lstm_cell_20/clip_by_value_1/MinimumMinimumlstm_cell_20/Add_2:z:0/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_20/clip_by_value_1/Minimum
lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_20/clip_by_value_1/yÃ
lstm_cell_20/clip_by_value_1Maximum(lstm_cell_20/clip_by_value_1/Minimum:z:0'lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_value_1±
!lstm_cell_20/mul_2/ReadVariableOpReadVariableOp*lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02#
!lstm_cell_20/mul_2/ReadVariableOp¥
lstm_cell_20/mul_2Mul lstm_cell_20/clip_by_value_1:z:0)lstm_cell_20/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_2t
lstm_cell_20/TanhTanhlstm_cell_20/split:output:2*
T0*
_output_shapes

:22
lstm_cell_20/Tanh
lstm_cell_20/mul_3Mullstm_cell_20/clip_by_value:z:0lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_3
lstm_cell_20/add_3AddV2lstm_cell_20/mul_2:z:0lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_20/add_3q
lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Const_4q
lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_5
lstm_cell_20/Mul_4Mullstm_cell_20/split:output:3lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul_4
lstm_cell_20/Add_4AddV2lstm_cell_20/Mul_4:z:0lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_4
&lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_20/clip_by_value_2/Minimum/yÉ
$lstm_cell_20/clip_by_value_2/MinimumMinimumlstm_cell_20/Add_4:z:0/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_20/clip_by_value_2/Minimum
lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_20/clip_by_value_2/yÃ
lstm_cell_20/clip_by_value_2Maximum(lstm_cell_20/clip_by_value_2/Minimum:z:0'lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_value_2s
lstm_cell_20/Tanh_1Tanhlstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_20/Tanh_1
lstm_cell_20/mul_5Mul lstm_cell_20/clip_by_value_2:z:0lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
time
ReadVariableOpReadVariableOp-lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterõ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_20_matmul_readvariableop_resource/lstm_cell_20_matmul_1_readvariableop_1_resource,lstm_cell_20_biasadd_readvariableop_resource*
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
while_body_57182*
condR
while_cond_57181*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm
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
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_20_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_20/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_20_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_20/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_20/BiasAdd/ReadVariableOp#^lstm_cell_20/MatMul/ReadVariableOp%^lstm_cell_20/MatMul_1/ReadVariableOp'^lstm_cell_20/MatMul_1/ReadVariableOp_1"^lstm_cell_20/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_20/BiasAdd/ReadVariableOp#lstm_cell_20/BiasAdd/ReadVariableOp2H
"lstm_cell_20/MatMul/ReadVariableOp"lstm_cell_20/MatMul/ReadVariableOp2L
$lstm_cell_20/MatMul_1/ReadVariableOp$lstm_cell_20/MatMul_1/ReadVariableOp2P
&lstm_cell_20/MatMul_1/ReadVariableOp_1&lstm_cell_20/MatMul_1/ReadVariableOp_12F
!lstm_cell_20/mul_2/ReadVariableOp!lstm_cell_20/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
ÔY
Ë
while_body_59406
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_21_matmul_readvariableop_resource_0:	ÈH
5while_lstm_cell_21_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_21_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_21_matmul_readvariableop_resource:	ÈF
3while_lstm_cell_21_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_21_biasadd_readvariableop_resource:	È¢)while/lstm_cell_21/BiasAdd/ReadVariableOp¢(while/lstm_cell_21/MatMul/ReadVariableOp¢*while/lstm_cell_21/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_21/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_21_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02*
(while/lstm_cell_21/MatMul/ReadVariableOpÎ
while/lstm_cell_21/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/MatMulÏ
*while/lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_21_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_21/MatMul_1/ReadVariableOp·
while/lstm_cell_21/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_21/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/MatMul_1¯
while/lstm_cell_21/addAddV2#while/lstm_cell_21/MatMul:product:0%while/lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/addÈ
)while/lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_21_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_21/BiasAdd/ReadVariableOp¼
while/lstm_cell_21/BiasAddBiasAddwhile/lstm_cell_21/add:z:01while/lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/BiasAdd
"while/lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_21/split/split_dimç
while/lstm_cell_21/splitSplit+while/lstm_cell_21/split/split_dim:output:0#while/lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_21/splity
while/lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const}
while/lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_1¦
while/lstm_cell_21/MulMul!while/lstm_cell_21/split:output:0!while/lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul§
while/lstm_cell_21/Add_1AddV2while/lstm_cell_21/Mul:z:0#while/lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_1
*while/lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_21/clip_by_value/Minimum/yÛ
(while/lstm_cell_21/clip_by_value/MinimumMinimumwhile/lstm_cell_21/Add_1:z:03while/lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(while/lstm_cell_21/clip_by_value/Minimum
"while/lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_21/clip_by_value/yÓ
 while/lstm_cell_21/clip_by_valueMaximum,while/lstm_cell_21/clip_by_value/Minimum:z:0+while/lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 while/lstm_cell_21/clip_by_value}
while/lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const_2}
while/lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_3¬
while/lstm_cell_21/Mul_1Mul!while/lstm_cell_21/split:output:1#while/lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul_1©
while/lstm_cell_21/Add_2AddV2while/lstm_cell_21/Mul_1:z:0#while/lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_2¡
,while/lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_21/clip_by_value_1/Minimum/yá
*while/lstm_cell_21/clip_by_value_1/MinimumMinimumwhile/lstm_cell_21/Add_2:z:05while/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_21/clip_by_value_1/Minimum
$while/lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_21/clip_by_value_1/yÛ
"while/lstm_cell_21/clip_by_value_1Maximum.while/lstm_cell_21/clip_by_value_1/Minimum:z:0-while/lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_21/clip_by_value_1¡
while/lstm_cell_21/mul_2Mul&while/lstm_cell_21/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_2
while/lstm_cell_21/TanhTanh!while/lstm_cell_21/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_21/Tanh§
while/lstm_cell_21/mul_3Mul$while/lstm_cell_21/clip_by_value:z:0while/lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_3¢
while/lstm_cell_21/add_3AddV2while/lstm_cell_21/mul_2:z:0while/lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_21/add_3}
while/lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const_4}
while/lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_5¬
while/lstm_cell_21/Mul_4Mul!while/lstm_cell_21/split:output:3#while/lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul_4©
while/lstm_cell_21/Add_4AddV2while/lstm_cell_21/Mul_4:z:0#while/lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_4¡
,while/lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_21/clip_by_value_2/Minimum/yá
*while/lstm_cell_21/clip_by_value_2/MinimumMinimumwhile/lstm_cell_21/Add_4:z:05while/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_21/clip_by_value_2/Minimum
$while/lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_21/clip_by_value_2/yÛ
"while/lstm_cell_21/clip_by_value_2Maximum.while/lstm_cell_21/clip_by_value_2/Minimum:z:0-while/lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_21/clip_by_value_2
while/lstm_cell_21/Tanh_1Tanhwhile/lstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Tanh_1«
while/lstm_cell_21/mul_5Mul&while/lstm_cell_21/clip_by_value_2:z:0while/lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_21/mul_5:z:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identitywhile/lstm_cell_21/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_21/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_21/BiasAdd/ReadVariableOp)^while/lstm_cell_21/MatMul/ReadVariableOp+^while/lstm_cell_21/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_21_biasadd_readvariableop_resource4while_lstm_cell_21_biasadd_readvariableop_resource_0"l
3while_lstm_cell_21_matmul_1_readvariableop_resource5while_lstm_cell_21_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_21_matmul_readvariableop_resource3while_lstm_cell_21_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_21/BiasAdd/ReadVariableOp)while/lstm_cell_21/BiasAdd/ReadVariableOp2T
(while/lstm_cell_21/MatMul/ReadVariableOp(while/lstm_cell_21/MatMul/ReadVariableOp2X
*while/lstm_cell_21/MatMul_1/ReadVariableOp*while/lstm_cell_21/MatMul_1/ReadVariableOp: 
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
ï,

G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_56172

inputs

states
states_11
matmul_readvariableop_resource:	2È3
 matmul_1_readvariableop_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2
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
¨
¼
while_cond_56095
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_56095___redundant_placeholder03
/while_while_cond_56095___redundant_placeholder13
/while_while_cond_56095___redundant_placeholder23
/while_while_cond_56095___redundant_placeholder3
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
ùn
´
B__inference_lstm_20_layer_call_and_return_conditional_losses_60283

inputs>
+lstm_cell_20_matmul_readvariableop_resource:	2È?
-lstm_cell_20_matmul_1_readvariableop_resource:2B
/lstm_cell_20_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_20_biasadd_readvariableop_resource:	È<
*lstm_cell_20_mul_2_readvariableop_resource:2
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_20/BiasAdd/ReadVariableOp¢"lstm_cell_20/MatMul/ReadVariableOp¢$lstm_cell_20/MatMul_1/ReadVariableOp¢&lstm_cell_20/MatMul_1/ReadVariableOp_1¢!lstm_cell_20/mul_2/ReadVariableOp¢whileu
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_20/MatMul/ReadVariableOpReadVariableOp+lstm_cell_20_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02$
"lstm_cell_20/MatMul/ReadVariableOp¤
lstm_cell_20/MatMulMatMulstrided_slice_1:output:0*lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/MatMulº
$lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$lstm_cell_20/MatMul_1/ReadVariableOpÁ
&lstm_cell_20/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_20_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_20/MatMul_1/ReadVariableOp_1À
lstm_cell_20/MatMul_1MatMul,lstm_cell_20/MatMul_1/ReadVariableOp:value:0.lstm_cell_20/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/MatMul_1
lstm_cell_20/addAddV2lstm_cell_20/MatMul:product:0lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_cell_20/add´
#lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_20_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_20/BiasAdd/ReadVariableOp¤
lstm_cell_20/BiasAddBiasAddlstm_cell_20/add:z:0+lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/BiasAdd~
lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_20/split/split_dimÏ
lstm_cell_20/splitSplit%lstm_cell_20/split/split_dim:output:0lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_20/splitm
lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Constq
lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_1
lstm_cell_20/MulMullstm_cell_20/split:output:0lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul
lstm_cell_20/Add_1AddV2lstm_cell_20/Mul:z:0lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_1
$lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_20/clip_by_value/Minimum/yÃ
"lstm_cell_20/clip_by_value/MinimumMinimumlstm_cell_20/Add_1:z:0-lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22$
"lstm_cell_20/clip_by_value/Minimum
lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_20/clip_by_value/y»
lstm_cell_20/clip_by_valueMaximum&lstm_cell_20/clip_by_value/Minimum:z:0%lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_valueq
lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Const_2q
lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_3
lstm_cell_20/Mul_1Mullstm_cell_20/split:output:1lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul_1
lstm_cell_20/Add_2AddV2lstm_cell_20/Mul_1:z:0lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_2
&lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_20/clip_by_value_1/Minimum/yÉ
$lstm_cell_20/clip_by_value_1/MinimumMinimumlstm_cell_20/Add_2:z:0/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_20/clip_by_value_1/Minimum
lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_20/clip_by_value_1/yÃ
lstm_cell_20/clip_by_value_1Maximum(lstm_cell_20/clip_by_value_1/Minimum:z:0'lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_value_1±
!lstm_cell_20/mul_2/ReadVariableOpReadVariableOp*lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02#
!lstm_cell_20/mul_2/ReadVariableOp¥
lstm_cell_20/mul_2Mul lstm_cell_20/clip_by_value_1:z:0)lstm_cell_20/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_2t
lstm_cell_20/TanhTanhlstm_cell_20/split:output:2*
T0*
_output_shapes

:22
lstm_cell_20/Tanh
lstm_cell_20/mul_3Mullstm_cell_20/clip_by_value:z:0lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_3
lstm_cell_20/add_3AddV2lstm_cell_20/mul_2:z:0lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_20/add_3q
lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Const_4q
lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_5
lstm_cell_20/Mul_4Mullstm_cell_20/split:output:3lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul_4
lstm_cell_20/Add_4AddV2lstm_cell_20/Mul_4:z:0lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_4
&lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_20/clip_by_value_2/Minimum/yÉ
$lstm_cell_20/clip_by_value_2/MinimumMinimumlstm_cell_20/Add_4:z:0/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_20/clip_by_value_2/Minimum
lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_20/clip_by_value_2/yÃ
lstm_cell_20/clip_by_value_2Maximum(lstm_cell_20/clip_by_value_2/Minimum:z:0'lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_value_2s
lstm_cell_20/Tanh_1Tanhlstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_20/Tanh_1
lstm_cell_20/mul_5Mul lstm_cell_20/clip_by_value_2:z:0lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
time
ReadVariableOpReadVariableOp-lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterõ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_20_matmul_readvariableop_resource/lstm_cell_20_matmul_1_readvariableop_1_resource,lstm_cell_20_biasadd_readvariableop_resource*
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
while_body_60178*
condR
while_cond_60177*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm
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
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_20_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_20/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_20_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_20/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_20/BiasAdd/ReadVariableOp#^lstm_cell_20/MatMul/ReadVariableOp%^lstm_cell_20/MatMul_1/ReadVariableOp'^lstm_cell_20/MatMul_1/ReadVariableOp_1"^lstm_cell_20/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_20/BiasAdd/ReadVariableOp#lstm_cell_20/BiasAdd/ReadVariableOp2H
"lstm_cell_20/MatMul/ReadVariableOp"lstm_cell_20/MatMul/ReadVariableOp2L
$lstm_cell_20/MatMul_1/ReadVariableOp$lstm_cell_20/MatMul_1/ReadVariableOp2P
&lstm_cell_20/MatMul_1/ReadVariableOp_1&lstm_cell_20/MatMul_1/ReadVariableOp_12F
!lstm_cell_20/mul_2/ReadVariableOp!lstm_cell_20/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
ÔY
Ë
while_body_60000
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_20_matmul_readvariableop_resource_0:	2ÈH
5while_lstm_cell_20_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_20_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_20_matmul_readvariableop_resource:	2ÈF
3while_lstm_cell_20_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_20_biasadd_readvariableop_resource:	È¢)while/lstm_cell_20/BiasAdd/ReadVariableOp¢(while/lstm_cell_20/MatMul/ReadVariableOp¢*while/lstm_cell_20/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_20/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_20_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02*
(while/lstm_cell_20/MatMul/ReadVariableOpÎ
while/lstm_cell_20/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/MatMulÏ
*while/lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_20_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_20/MatMul_1/ReadVariableOp·
while/lstm_cell_20/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_20/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/MatMul_1¯
while/lstm_cell_20/addAddV2#while/lstm_cell_20/MatMul:product:0%while/lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/addÈ
)while/lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_20_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_20/BiasAdd/ReadVariableOp¼
while/lstm_cell_20/BiasAddBiasAddwhile/lstm_cell_20/add:z:01while/lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/BiasAdd
"while/lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_20/split/split_dimç
while/lstm_cell_20/splitSplit+while/lstm_cell_20/split/split_dim:output:0#while/lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_20/splity
while/lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const}
while/lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_1¦
while/lstm_cell_20/MulMul!while/lstm_cell_20/split:output:0!while/lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul§
while/lstm_cell_20/Add_1AddV2while/lstm_cell_20/Mul:z:0#while/lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_1
*while/lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_20/clip_by_value/Minimum/yÛ
(while/lstm_cell_20/clip_by_value/MinimumMinimumwhile/lstm_cell_20/Add_1:z:03while/lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(while/lstm_cell_20/clip_by_value/Minimum
"while/lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_20/clip_by_value/yÓ
 while/lstm_cell_20/clip_by_valueMaximum,while/lstm_cell_20/clip_by_value/Minimum:z:0+while/lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 while/lstm_cell_20/clip_by_value}
while/lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const_2}
while/lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_3¬
while/lstm_cell_20/Mul_1Mul!while/lstm_cell_20/split:output:1#while/lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul_1©
while/lstm_cell_20/Add_2AddV2while/lstm_cell_20/Mul_1:z:0#while/lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_2¡
,while/lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_20/clip_by_value_1/Minimum/yá
*while/lstm_cell_20/clip_by_value_1/MinimumMinimumwhile/lstm_cell_20/Add_2:z:05while/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_20/clip_by_value_1/Minimum
$while/lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_20/clip_by_value_1/yÛ
"while/lstm_cell_20/clip_by_value_1Maximum.while/lstm_cell_20/clip_by_value_1/Minimum:z:0-while/lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_20/clip_by_value_1¡
while/lstm_cell_20/mul_2Mul&while/lstm_cell_20/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_2
while/lstm_cell_20/TanhTanh!while/lstm_cell_20/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_20/Tanh§
while/lstm_cell_20/mul_3Mul$while/lstm_cell_20/clip_by_value:z:0while/lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_3¢
while/lstm_cell_20/add_3AddV2while/lstm_cell_20/mul_2:z:0while/lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_20/add_3}
while/lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const_4}
while/lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_5¬
while/lstm_cell_20/Mul_4Mul!while/lstm_cell_20/split:output:3#while/lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul_4©
while/lstm_cell_20/Add_4AddV2while/lstm_cell_20/Mul_4:z:0#while/lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_4¡
,while/lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_20/clip_by_value_2/Minimum/yá
*while/lstm_cell_20/clip_by_value_2/MinimumMinimumwhile/lstm_cell_20/Add_4:z:05while/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_20/clip_by_value_2/Minimum
$while/lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_20/clip_by_value_2/yÛ
"while/lstm_cell_20/clip_by_value_2Maximum.while/lstm_cell_20/clip_by_value_2/Minimum:z:0-while/lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_20/clip_by_value_2
while/lstm_cell_20/Tanh_1Tanhwhile/lstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Tanh_1«
while/lstm_cell_20/mul_5Mul&while/lstm_cell_20/clip_by_value_2:z:0while/lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_20/mul_5:z:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identitywhile/lstm_cell_20/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_20/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_20/BiasAdd/ReadVariableOp)^while/lstm_cell_20/MatMul/ReadVariableOp+^while/lstm_cell_20/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_20_biasadd_readvariableop_resource4while_lstm_cell_20_biasadd_readvariableop_resource_0"l
3while_lstm_cell_20_matmul_1_readvariableop_resource5while_lstm_cell_20_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_20_matmul_readvariableop_resource3while_lstm_cell_20_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_20/BiasAdd/ReadVariableOp)while/lstm_cell_20/BiasAdd/ReadVariableOp2T
(while/lstm_cell_20/MatMul/ReadVariableOp(while/lstm_cell_20/MatMul/ReadVariableOp2X
*while/lstm_cell_20/MatMul_1/ReadVariableOp*while/lstm_cell_20/MatMul_1/ReadVariableOp: 
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
ü

¸
#__inference_signature_wrapper_58013
lstm_21_input
unknown:	È
	unknown_0:2
	unknown_1:	2È
	unknown_2:	È
	unknown_3:2
	unknown_4:	2È
	unknown_5:2
	unknown_6:	2È
	unknown_7:	È
	unknown_8:2
	unknown_9:2

unknown_10:
identity¢StatefulPartitionedCallÑ
StatefulPartitionedCallStatefulPartitionedCalllstm_21_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
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
GPU 2J 8 *)
f$R"
 __inference__wrapped_model_552232
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
StatefulPartitionedCallStatefulPartitionedCall:Q M
"
_output_shapes
:

'
_user_specified_namelstm_21_input
ø

&sequential_10_lstm_21_while_body_54932H
Dsequential_10_lstm_21_while_sequential_10_lstm_21_while_loop_counterN
Jsequential_10_lstm_21_while_sequential_10_lstm_21_while_maximum_iterations+
'sequential_10_lstm_21_while_placeholder-
)sequential_10_lstm_21_while_placeholder_1-
)sequential_10_lstm_21_while_placeholder_2-
)sequential_10_lstm_21_while_placeholder_3E
Asequential_10_lstm_21_while_sequential_10_lstm_21_strided_slice_0
sequential_10_lstm_21_while_tensorarrayv2read_tensorlistgetitem_sequential_10_lstm_21_tensorarrayunstack_tensorlistfromtensor_0\
Isequential_10_lstm_21_while_lstm_cell_21_matmul_readvariableop_resource_0:	È^
Ksequential_10_lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource_0:	2ÈY
Jsequential_10_lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource_0:	È(
$sequential_10_lstm_21_while_identity*
&sequential_10_lstm_21_while_identity_1*
&sequential_10_lstm_21_while_identity_2*
&sequential_10_lstm_21_while_identity_3*
&sequential_10_lstm_21_while_identity_4*
&sequential_10_lstm_21_while_identity_5C
?sequential_10_lstm_21_while_sequential_10_lstm_21_strided_slice
}sequential_10_lstm_21_while_tensorarrayv2read_tensorlistgetitem_sequential_10_lstm_21_tensorarrayunstack_tensorlistfromtensorZ
Gsequential_10_lstm_21_while_lstm_cell_21_matmul_readvariableop_resource:	È\
Isequential_10_lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource:	2ÈW
Hsequential_10_lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource:	È¢?sequential_10/lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp¢>sequential_10/lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp¢@sequential_10/lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOpï
Msequential_10/lstm_21/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      2O
Msequential_10/lstm_21/while/TensorArrayV2Read/TensorListGetItem/element_shapeÎ
?sequential_10/lstm_21/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemsequential_10_lstm_21_while_tensorarrayv2read_tensorlistgetitem_sequential_10_lstm_21_tensorarrayunstack_tensorlistfromtensor_0'sequential_10_lstm_21_while_placeholderVsequential_10/lstm_21/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02A
?sequential_10/lstm_21/while/TensorArrayV2Read/TensorListGetItem
>sequential_10/lstm_21/while/lstm_cell_21/MatMul/ReadVariableOpReadVariableOpIsequential_10_lstm_21_while_lstm_cell_21_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02@
>sequential_10/lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp¦
/sequential_10/lstm_21/while/lstm_cell_21/MatMulMatMulFsequential_10/lstm_21/while/TensorArrayV2Read/TensorListGetItem:item:0Fsequential_10/lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È21
/sequential_10/lstm_21/while/lstm_cell_21/MatMul
@sequential_10/lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOpKsequential_10_lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02B
@sequential_10/lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp
1sequential_10/lstm_21/while/lstm_cell_21/MatMul_1MatMul)sequential_10_lstm_21_while_placeholder_2Hsequential_10/lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È23
1sequential_10/lstm_21/while/lstm_cell_21/MatMul_1
,sequential_10/lstm_21/while/lstm_cell_21/addAddV29sequential_10/lstm_21/while/lstm_cell_21/MatMul:product:0;sequential_10/lstm_21/while/lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2.
,sequential_10/lstm_21/while/lstm_cell_21/add
?sequential_10/lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOpJsequential_10_lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02A
?sequential_10/lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp
0sequential_10/lstm_21/while/lstm_cell_21/BiasAddBiasAdd0sequential_10/lstm_21/while/lstm_cell_21/add:z:0Gsequential_10/lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È22
0sequential_10/lstm_21/while/lstm_cell_21/BiasAdd¶
8sequential_10/lstm_21/while/lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2:
8sequential_10/lstm_21/while/lstm_cell_21/split/split_dim¿
.sequential_10/lstm_21/while/lstm_cell_21/splitSplitAsequential_10/lstm_21/while/lstm_cell_21/split/split_dim:output:09sequential_10/lstm_21/while/lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split20
.sequential_10/lstm_21/while/lstm_cell_21/split¥
.sequential_10/lstm_21/while/lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>20
.sequential_10/lstm_21/while/lstm_cell_21/Const©
0sequential_10/lstm_21/while/lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?22
0sequential_10/lstm_21/while/lstm_cell_21/Const_1þ
,sequential_10/lstm_21/while/lstm_cell_21/MulMul7sequential_10/lstm_21/while/lstm_cell_21/split:output:07sequential_10/lstm_21/while/lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22.
,sequential_10/lstm_21/while/lstm_cell_21/Mulÿ
.sequential_10/lstm_21/while/lstm_cell_21/Add_1AddV20sequential_10/lstm_21/while/lstm_cell_21/Mul:z:09sequential_10/lstm_21/while/lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:220
.sequential_10/lstm_21/while/lstm_cell_21/Add_1É
@sequential_10/lstm_21/while/lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2B
@sequential_10/lstm_21/while/lstm_cell_21/clip_by_value/Minimum/y³
>sequential_10/lstm_21/while/lstm_cell_21/clip_by_value/MinimumMinimum2sequential_10/lstm_21/while/lstm_cell_21/Add_1:z:0Isequential_10/lstm_21/while/lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22@
>sequential_10/lstm_21/while/lstm_cell_21/clip_by_value/Minimum¹
8sequential_10/lstm_21/while/lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2:
8sequential_10/lstm_21/while/lstm_cell_21/clip_by_value/y«
6sequential_10/lstm_21/while/lstm_cell_21/clip_by_valueMaximumBsequential_10/lstm_21/while/lstm_cell_21/clip_by_value/Minimum:z:0Asequential_10/lstm_21/while/lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:228
6sequential_10/lstm_21/while/lstm_cell_21/clip_by_value©
0sequential_10/lstm_21/while/lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>22
0sequential_10/lstm_21/while/lstm_cell_21/Const_2©
0sequential_10/lstm_21/while/lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?22
0sequential_10/lstm_21/while/lstm_cell_21/Const_3
.sequential_10/lstm_21/while/lstm_cell_21/Mul_1Mul7sequential_10/lstm_21/while/lstm_cell_21/split:output:19sequential_10/lstm_21/while/lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:220
.sequential_10/lstm_21/while/lstm_cell_21/Mul_1
.sequential_10/lstm_21/while/lstm_cell_21/Add_2AddV22sequential_10/lstm_21/while/lstm_cell_21/Mul_1:z:09sequential_10/lstm_21/while/lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:220
.sequential_10/lstm_21/while/lstm_cell_21/Add_2Í
Bsequential_10/lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2D
Bsequential_10/lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum/y¹
@sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_1/MinimumMinimum2sequential_10/lstm_21/while/lstm_cell_21/Add_2:z:0Ksequential_10/lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22B
@sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum½
:sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2<
:sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_1/y³
8sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_1MaximumDsequential_10/lstm_21/while/lstm_cell_21/clip_by_value_1/Minimum:z:0Csequential_10/lstm_21/while/lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22:
8sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_1ù
.sequential_10/lstm_21/while/lstm_cell_21/mul_2Mul<sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_1:z:0)sequential_10_lstm_21_while_placeholder_3*
T0*
_output_shapes

:220
.sequential_10/lstm_21/while/lstm_cell_21/mul_2È
-sequential_10/lstm_21/while/lstm_cell_21/TanhTanh7sequential_10/lstm_21/while/lstm_cell_21/split:output:2*
T0*
_output_shapes

:22/
-sequential_10/lstm_21/while/lstm_cell_21/Tanhÿ
.sequential_10/lstm_21/while/lstm_cell_21/mul_3Mul:sequential_10/lstm_21/while/lstm_cell_21/clip_by_value:z:01sequential_10/lstm_21/while/lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:220
.sequential_10/lstm_21/while/lstm_cell_21/mul_3ú
.sequential_10/lstm_21/while/lstm_cell_21/add_3AddV22sequential_10/lstm_21/while/lstm_cell_21/mul_2:z:02sequential_10/lstm_21/while/lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:220
.sequential_10/lstm_21/while/lstm_cell_21/add_3©
0sequential_10/lstm_21/while/lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>22
0sequential_10/lstm_21/while/lstm_cell_21/Const_4©
0sequential_10/lstm_21/while/lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?22
0sequential_10/lstm_21/while/lstm_cell_21/Const_5
.sequential_10/lstm_21/while/lstm_cell_21/Mul_4Mul7sequential_10/lstm_21/while/lstm_cell_21/split:output:39sequential_10/lstm_21/while/lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:220
.sequential_10/lstm_21/while/lstm_cell_21/Mul_4
.sequential_10/lstm_21/while/lstm_cell_21/Add_4AddV22sequential_10/lstm_21/while/lstm_cell_21/Mul_4:z:09sequential_10/lstm_21/while/lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:220
.sequential_10/lstm_21/while/lstm_cell_21/Add_4Í
Bsequential_10/lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2D
Bsequential_10/lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum/y¹
@sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_2/MinimumMinimum2sequential_10/lstm_21/while/lstm_cell_21/Add_4:z:0Ksequential_10/lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22B
@sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum½
:sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2<
:sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_2/y³
8sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_2MaximumDsequential_10/lstm_21/while/lstm_cell_21/clip_by_value_2/Minimum:z:0Csequential_10/lstm_21/while/lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22:
8sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_2Ç
/sequential_10/lstm_21/while/lstm_cell_21/Tanh_1Tanh2sequential_10/lstm_21/while/lstm_cell_21/add_3:z:0*
T0*
_output_shapes

:221
/sequential_10/lstm_21/while/lstm_cell_21/Tanh_1
.sequential_10/lstm_21/while/lstm_cell_21/mul_5Mul<sequential_10/lstm_21/while/lstm_cell_21/clip_by_value_2:z:03sequential_10/lstm_21/while/lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:220
.sequential_10/lstm_21/while/lstm_cell_21/mul_5Î
@sequential_10/lstm_21/while/TensorArrayV2Write/TensorListSetItemTensorListSetItem)sequential_10_lstm_21_while_placeholder_1'sequential_10_lstm_21_while_placeholder2sequential_10/lstm_21/while/lstm_cell_21/mul_5:z:0*
_output_shapes
: *
element_dtype02B
@sequential_10/lstm_21/while/TensorArrayV2Write/TensorListSetItem
!sequential_10/lstm_21/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2#
!sequential_10/lstm_21/while/add/yÁ
sequential_10/lstm_21/while/addAddV2'sequential_10_lstm_21_while_placeholder*sequential_10/lstm_21/while/add/y:output:0*
T0*
_output_shapes
: 2!
sequential_10/lstm_21/while/add
#sequential_10/lstm_21/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2%
#sequential_10/lstm_21/while/add_1/yä
!sequential_10/lstm_21/while/add_1AddV2Dsequential_10_lstm_21_while_sequential_10_lstm_21_while_loop_counter,sequential_10/lstm_21/while/add_1/y:output:0*
T0*
_output_shapes
: 2#
!sequential_10/lstm_21/while/add_1Ã
$sequential_10/lstm_21/while/IdentityIdentity%sequential_10/lstm_21/while/add_1:z:0!^sequential_10/lstm_21/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_10/lstm_21/while/Identityì
&sequential_10/lstm_21/while/Identity_1IdentityJsequential_10_lstm_21_while_sequential_10_lstm_21_while_maximum_iterations!^sequential_10/lstm_21/while/NoOp*
T0*
_output_shapes
: 2(
&sequential_10/lstm_21/while/Identity_1Å
&sequential_10/lstm_21/while/Identity_2Identity#sequential_10/lstm_21/while/add:z:0!^sequential_10/lstm_21/while/NoOp*
T0*
_output_shapes
: 2(
&sequential_10/lstm_21/while/Identity_2ò
&sequential_10/lstm_21/while/Identity_3IdentityPsequential_10/lstm_21/while/TensorArrayV2Write/TensorListSetItem:output_handle:0!^sequential_10/lstm_21/while/NoOp*
T0*
_output_shapes
: 2(
&sequential_10/lstm_21/while/Identity_3Ü
&sequential_10/lstm_21/while/Identity_4Identity2sequential_10/lstm_21/while/lstm_cell_21/mul_5:z:0!^sequential_10/lstm_21/while/NoOp*
T0*
_output_shapes

:22(
&sequential_10/lstm_21/while/Identity_4Ü
&sequential_10/lstm_21/while/Identity_5Identity2sequential_10/lstm_21/while/lstm_cell_21/add_3:z:0!^sequential_10/lstm_21/while/NoOp*
T0*
_output_shapes

:22(
&sequential_10/lstm_21/while/Identity_5Ì
 sequential_10/lstm_21/while/NoOpNoOp@^sequential_10/lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp?^sequential_10/lstm_21/while/lstm_cell_21/MatMul/ReadVariableOpA^sequential_10/lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2"
 sequential_10/lstm_21/while/NoOp"U
$sequential_10_lstm_21_while_identity-sequential_10/lstm_21/while/Identity:output:0"Y
&sequential_10_lstm_21_while_identity_1/sequential_10/lstm_21/while/Identity_1:output:0"Y
&sequential_10_lstm_21_while_identity_2/sequential_10/lstm_21/while/Identity_2:output:0"Y
&sequential_10_lstm_21_while_identity_3/sequential_10/lstm_21/while/Identity_3:output:0"Y
&sequential_10_lstm_21_while_identity_4/sequential_10/lstm_21/while/Identity_4:output:0"Y
&sequential_10_lstm_21_while_identity_5/sequential_10/lstm_21/while/Identity_5:output:0"
Hsequential_10_lstm_21_while_lstm_cell_21_biasadd_readvariableop_resourceJsequential_10_lstm_21_while_lstm_cell_21_biasadd_readvariableop_resource_0"
Isequential_10_lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resourceKsequential_10_lstm_21_while_lstm_cell_21_matmul_1_readvariableop_resource_0"
Gsequential_10_lstm_21_while_lstm_cell_21_matmul_readvariableop_resourceIsequential_10_lstm_21_while_lstm_cell_21_matmul_readvariableop_resource_0"
?sequential_10_lstm_21_while_sequential_10_lstm_21_strided_sliceAsequential_10_lstm_21_while_sequential_10_lstm_21_strided_slice_0"
}sequential_10_lstm_21_while_tensorarrayv2read_tensorlistgetitem_sequential_10_lstm_21_tensorarrayunstack_tensorlistfromtensorsequential_10_lstm_21_while_tensorarrayv2read_tensorlistgetitem_sequential_10_lstm_21_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2
?sequential_10/lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp?sequential_10/lstm_21/while/lstm_cell_21/BiasAdd/ReadVariableOp2
>sequential_10/lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp>sequential_10/lstm_21/while/lstm_cell_21/MatMul/ReadVariableOp2
@sequential_10/lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp@sequential_10/lstm_21/while/lstm_cell_21/MatMul_1/ReadVariableOp: 
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
Ä
£
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_60413

inputs9
'dense_10_matmul_readvariableop_resource:26
(dense_10_biasadd_readvariableop_resource:
identity¢dense_10/BiasAdd/ReadVariableOp¢dense_10/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2
Reshape/shapef
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes

:
22	
Reshape¨
dense_10/MatMul/ReadVariableOpReadVariableOp'dense_10_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02 
dense_10/MatMul/ReadVariableOp
dense_10/MatMulMatMulReshape:output:0&dense_10/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_10/MatMul§
dense_10/BiasAdd/ReadVariableOpReadVariableOp(dense_10_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02!
dense_10/BiasAdd/ReadVariableOp
dense_10/BiasAddBiasAdddense_10/MatMul:product:0'dense_10/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_10/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      2
Reshape_1/shape
	Reshape_1Reshapedense_10/BiasAdd:output:0Reshape_1/shape:output:0*
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

Identity
NoOpNoOp ^dense_10/BiasAdd/ReadVariableOp^dense_10/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:
2: : 2B
dense_10/BiasAdd/ReadVariableOpdense_10/BiasAdd/ReadVariableOp2@
dense_10/MatMul/ReadVariableOpdense_10/MatMul/ReadVariableOp:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
¨
¼
while_cond_57181
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_57181___redundant_placeholder03
/while_while_cond_57181___redundant_placeholder13
/while_while_cond_57181___redundant_placeholder23
/while_while_cond_57181___redundant_placeholder3
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
	
ð
'__inference_lstm_21_layer_call_fn_59526
inputs_0
unknown:2
	unknown_0:2
	unknown_1:	È
	unknown_2:	2È
	unknown_3:	È
identity¢StatefulPartitionedCall
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_21_layer_call_and_return_conditional_losses_554422
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:ÿÿÿÿÿÿÿÿÿ: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/0
Ë
£
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_60364

inputs9
'dense_10_matmul_readvariableop_resource:26
(dense_10_biasadd_readvariableop_resource:
identity¢dense_10/BiasAdd/ReadVariableOp¢dense_10/MatMul/ReadVariableOpD
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
strided_slice/stack_2â
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
valueB"ÿÿÿÿ2   2
Reshape/shapeo
ReshapeReshapeinputsReshape/shape:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22	
Reshape¨
dense_10/MatMul/ReadVariableOpReadVariableOp'dense_10_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02 
dense_10/MatMul/ReadVariableOp
dense_10/MatMulMatMulReshape:output:0&dense_10/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
dense_10/MatMul§
dense_10/BiasAdd/ReadVariableOpReadVariableOp(dense_10_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02!
dense_10/BiasAdd/ReadVariableOp¥
dense_10/BiasAddBiasAdddense_10/MatMul:product:0'dense_10/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
dense_10/BiasAddq
Reshape_1/shape/0Const*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
Reshape_1/shape/0h
Reshape_1/shape/2Const*
_output_shapes
: *
dtype0*
value	B :2
Reshape_1/shape/2¨
Reshape_1/shapePackReshape_1/shape/0:output:0strided_slice:output:0Reshape_1/shape/2:output:0*
N*
T0*
_output_shapes
:2
Reshape_1/shape
	Reshape_1Reshapedense_10/BiasAdd:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2

Identity
NoOpNoOp ^dense_10/BiasAdd/ReadVariableOp^dense_10/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2: : 2B
dense_10/BiasAdd/ReadVariableOpdense_10/BiasAdd/ReadVariableOp2@
dense_10/MatMul/ReadVariableOpdense_10/MatMul/ReadVariableOp:\ X
4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
¹H
ª

!__inference__traced_restore_61401
file_prefix?
,assignvariableop_lstm_21_lstm_cell_21_kernel:	ÈK
8assignvariableop_1_lstm_21_lstm_cell_21_recurrent_kernel:	2È;
,assignvariableop_2_lstm_21_lstm_cell_21_bias:	ÈA
.assignvariableop_3_lstm_20_lstm_cell_20_kernel:	2ÈK
8assignvariableop_4_lstm_20_lstm_cell_20_recurrent_kernel:	2È;
,assignvariableop_5_lstm_20_lstm_cell_20_bias:	È?
-assignvariableop_6_time_distributed_10_kernel:29
+assignvariableop_7_time_distributed_10_bias:5
#assignvariableop_8_lstm_21_variable:27
%assignvariableop_9_lstm_21_variable_1:26
$assignvariableop_10_lstm_20_variable:28
&assignvariableop_11_lstm_20_variable_1:2#
assignvariableop_12_total: #
assignvariableop_13_count: %
assignvariableop_14_total_1: %
assignvariableop_15_count_1: 
identity_17¢AssignVariableOp¢AssignVariableOp_1¢AssignVariableOp_10¢AssignVariableOp_11¢AssignVariableOp_12¢AssignVariableOp_13¢AssignVariableOp_14¢AssignVariableOp_15¢AssignVariableOp_2¢AssignVariableOp_3¢AssignVariableOp_4¢AssignVariableOp_5¢AssignVariableOp_6¢AssignVariableOp_7¢AssignVariableOp_8¢AssignVariableOp_9
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*«
value¡BB0trainable_variables/0/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/1/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/2/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/3/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/4/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/5/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/6/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/7/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
RestoreV2/tensor_names°
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*5
value,B*B B B B B B B B B B B B B B B B B 2
RestoreV2/shape_and_slices
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

Identity«
AssignVariableOpAssignVariableOp,assignvariableop_lstm_21_lstm_cell_21_kernelIdentity:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOpk

Identity_1IdentityRestoreV2:tensors:1"/device:CPU:0*
T0*
_output_shapes
:2

Identity_1½
AssignVariableOp_1AssignVariableOp8assignvariableop_1_lstm_21_lstm_cell_21_recurrent_kernelIdentity_1:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_1k

Identity_2IdentityRestoreV2:tensors:2"/device:CPU:0*
T0*
_output_shapes
:2

Identity_2±
AssignVariableOp_2AssignVariableOp,assignvariableop_2_lstm_21_lstm_cell_21_biasIdentity_2:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_2k

Identity_3IdentityRestoreV2:tensors:3"/device:CPU:0*
T0*
_output_shapes
:2

Identity_3³
AssignVariableOp_3AssignVariableOp.assignvariableop_3_lstm_20_lstm_cell_20_kernelIdentity_3:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_3k

Identity_4IdentityRestoreV2:tensors:4"/device:CPU:0*
T0*
_output_shapes
:2

Identity_4½
AssignVariableOp_4AssignVariableOp8assignvariableop_4_lstm_20_lstm_cell_20_recurrent_kernelIdentity_4:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_4k

Identity_5IdentityRestoreV2:tensors:5"/device:CPU:0*
T0*
_output_shapes
:2

Identity_5±
AssignVariableOp_5AssignVariableOp,assignvariableop_5_lstm_20_lstm_cell_20_biasIdentity_5:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_5k

Identity_6IdentityRestoreV2:tensors:6"/device:CPU:0*
T0*
_output_shapes
:2

Identity_6²
AssignVariableOp_6AssignVariableOp-assignvariableop_6_time_distributed_10_kernelIdentity_6:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_6k

Identity_7IdentityRestoreV2:tensors:7"/device:CPU:0*
T0*
_output_shapes
:2

Identity_7°
AssignVariableOp_7AssignVariableOp+assignvariableop_7_time_distributed_10_biasIdentity_7:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_7k

Identity_8IdentityRestoreV2:tensors:8"/device:CPU:0*
T0*
_output_shapes
:2

Identity_8¨
AssignVariableOp_8AssignVariableOp#assignvariableop_8_lstm_21_variableIdentity_8:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_8k

Identity_9IdentityRestoreV2:tensors:9"/device:CPU:0*
T0*
_output_shapes
:2

Identity_9ª
AssignVariableOp_9AssignVariableOp%assignvariableop_9_lstm_21_variable_1Identity_9:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_9n
Identity_10IdentityRestoreV2:tensors:10"/device:CPU:0*
T0*
_output_shapes
:2
Identity_10¬
AssignVariableOp_10AssignVariableOp$assignvariableop_10_lstm_20_variableIdentity_10:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_10n
Identity_11IdentityRestoreV2:tensors:11"/device:CPU:0*
T0*
_output_shapes
:2
Identity_11®
AssignVariableOp_11AssignVariableOp&assignvariableop_11_lstm_20_variable_1Identity_11:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_11n
Identity_12IdentityRestoreV2:tensors:12"/device:CPU:0*
T0*
_output_shapes
:2
Identity_12¡
AssignVariableOp_12AssignVariableOpassignvariableop_12_totalIdentity_12:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_12n
Identity_13IdentityRestoreV2:tensors:13"/device:CPU:0*
T0*
_output_shapes
:2
Identity_13¡
AssignVariableOp_13AssignVariableOpassignvariableop_13_countIdentity_13:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_13n
Identity_14IdentityRestoreV2:tensors:14"/device:CPU:0*
T0*
_output_shapes
:2
Identity_14£
AssignVariableOp_14AssignVariableOpassignvariableop_14_total_1Identity_14:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_14n
Identity_15IdentityRestoreV2:tensors:15"/device:CPU:0*
T0*
_output_shapes
:2
Identity_15£
AssignVariableOp_15AssignVariableOpassignvariableop_15_count_1Identity_15:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_159
NoOpNoOp"/device:CPU:0*
_output_shapes
 2
NoOp¾
Identity_16Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_2^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9^NoOp"/device:CPU:0*
T0*
_output_shapes
: 2
Identity_16f
Identity_17IdentityIdentity_16:output:0^NoOp_1*
T0*
_output_shapes
: 2
Identity_17¦
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
ÔY
Ë
while_body_59644
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_20_matmul_readvariableop_resource_0:	2ÈH
5while_lstm_cell_20_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_20_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_20_matmul_readvariableop_resource:	2ÈF
3while_lstm_cell_20_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_20_biasadd_readvariableop_resource:	È¢)while/lstm_cell_20/BiasAdd/ReadVariableOp¢(while/lstm_cell_20/MatMul/ReadVariableOp¢*while/lstm_cell_20/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_20/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_20_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02*
(while/lstm_cell_20/MatMul/ReadVariableOpÎ
while/lstm_cell_20/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/MatMulÏ
*while/lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_20_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_20/MatMul_1/ReadVariableOp·
while/lstm_cell_20/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_20/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/MatMul_1¯
while/lstm_cell_20/addAddV2#while/lstm_cell_20/MatMul:product:0%while/lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/addÈ
)while/lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_20_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_20/BiasAdd/ReadVariableOp¼
while/lstm_cell_20/BiasAddBiasAddwhile/lstm_cell_20/add:z:01while/lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_20/BiasAdd
"while/lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_20/split/split_dimç
while/lstm_cell_20/splitSplit+while/lstm_cell_20/split/split_dim:output:0#while/lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_20/splity
while/lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const}
while/lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_1¦
while/lstm_cell_20/MulMul!while/lstm_cell_20/split:output:0!while/lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul§
while/lstm_cell_20/Add_1AddV2while/lstm_cell_20/Mul:z:0#while/lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_1
*while/lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_20/clip_by_value/Minimum/yÛ
(while/lstm_cell_20/clip_by_value/MinimumMinimumwhile/lstm_cell_20/Add_1:z:03while/lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(while/lstm_cell_20/clip_by_value/Minimum
"while/lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_20/clip_by_value/yÓ
 while/lstm_cell_20/clip_by_valueMaximum,while/lstm_cell_20/clip_by_value/Minimum:z:0+while/lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 while/lstm_cell_20/clip_by_value}
while/lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const_2}
while/lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_3¬
while/lstm_cell_20/Mul_1Mul!while/lstm_cell_20/split:output:1#while/lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul_1©
while/lstm_cell_20/Add_2AddV2while/lstm_cell_20/Mul_1:z:0#while/lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_2¡
,while/lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_20/clip_by_value_1/Minimum/yá
*while/lstm_cell_20/clip_by_value_1/MinimumMinimumwhile/lstm_cell_20/Add_2:z:05while/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_20/clip_by_value_1/Minimum
$while/lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_20/clip_by_value_1/yÛ
"while/lstm_cell_20/clip_by_value_1Maximum.while/lstm_cell_20/clip_by_value_1/Minimum:z:0-while/lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_20/clip_by_value_1¡
while/lstm_cell_20/mul_2Mul&while/lstm_cell_20/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_2
while/lstm_cell_20/TanhTanh!while/lstm_cell_20/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_20/Tanh§
while/lstm_cell_20/mul_3Mul$while/lstm_cell_20/clip_by_value:z:0while/lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_3¢
while/lstm_cell_20/add_3AddV2while/lstm_cell_20/mul_2:z:0while/lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_20/add_3}
while/lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_20/Const_4}
while/lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_20/Const_5¬
while/lstm_cell_20/Mul_4Mul!while/lstm_cell_20/split:output:3#while/lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Mul_4©
while/lstm_cell_20/Add_4AddV2while/lstm_cell_20/Mul_4:z:0#while/lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Add_4¡
,while/lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_20/clip_by_value_2/Minimum/yá
*while/lstm_cell_20/clip_by_value_2/MinimumMinimumwhile/lstm_cell_20/Add_4:z:05while/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_20/clip_by_value_2/Minimum
$while/lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_20/clip_by_value_2/yÛ
"while/lstm_cell_20/clip_by_value_2Maximum.while/lstm_cell_20/clip_by_value_2/Minimum:z:0-while/lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_20/clip_by_value_2
while/lstm_cell_20/Tanh_1Tanhwhile/lstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_20/Tanh_1«
while/lstm_cell_20/mul_5Mul&while/lstm_cell_20/clip_by_value_2:z:0while/lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_20/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_20/mul_5:z:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identitywhile/lstm_cell_20/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_20/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_20/BiasAdd/ReadVariableOp)^while/lstm_cell_20/MatMul/ReadVariableOp+^while/lstm_cell_20/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_20_biasadd_readvariableop_resource4while_lstm_cell_20_biasadd_readvariableop_resource_0"l
3while_lstm_cell_20_matmul_1_readvariableop_resource5while_lstm_cell_20_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_20_matmul_readvariableop_resource3while_lstm_cell_20_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_20/BiasAdd/ReadVariableOp)while/lstm_cell_20/BiasAdd/ReadVariableOp2T
(while/lstm_cell_20/MatMul/ReadVariableOp(while/lstm_cell_20/MatMul/ReadVariableOp2X
*while/lstm_cell_20/MatMul_1/ReadVariableOp*while/lstm_cell_20/MatMul_1/ReadVariableOp: 
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
i
Ë

lstm_20_while_body_58260,
(lstm_20_while_lstm_20_while_loop_counter2
.lstm_20_while_lstm_20_while_maximum_iterations
lstm_20_while_placeholder
lstm_20_while_placeholder_1
lstm_20_while_placeholder_2
lstm_20_while_placeholder_3)
%lstm_20_while_lstm_20_strided_slice_0g
clstm_20_while_tensorarrayv2read_tensorlistgetitem_lstm_20_tensorarrayunstack_tensorlistfromtensor_0N
;lstm_20_while_lstm_cell_20_matmul_readvariableop_resource_0:	2ÈP
=lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource_0:	2ÈK
<lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource_0:	È
lstm_20_while_identity
lstm_20_while_identity_1
lstm_20_while_identity_2
lstm_20_while_identity_3
lstm_20_while_identity_4
lstm_20_while_identity_5'
#lstm_20_while_lstm_20_strided_slicee
alstm_20_while_tensorarrayv2read_tensorlistgetitem_lstm_20_tensorarrayunstack_tensorlistfromtensorL
9lstm_20_while_lstm_cell_20_matmul_readvariableop_resource:	2ÈN
;lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource:	2ÈI
:lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource:	È¢1lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp¢0lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp¢2lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOpÓ
?lstm_20/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2A
?lstm_20/while/TensorArrayV2Read/TensorListGetItem/element_shapeú
1lstm_20/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemclstm_20_while_tensorarrayv2read_tensorlistgetitem_lstm_20_tensorarrayunstack_tensorlistfromtensor_0lstm_20_while_placeholderHlstm_20/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype023
1lstm_20/while/TensorArrayV2Read/TensorListGetItemá
0lstm_20/while/lstm_cell_20/MatMul/ReadVariableOpReadVariableOp;lstm_20_while_lstm_cell_20_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype022
0lstm_20/while/lstm_cell_20/MatMul/ReadVariableOpî
!lstm_20/while/lstm_cell_20/MatMulMatMul8lstm_20/while/TensorArrayV2Read/TensorListGetItem:item:08lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2#
!lstm_20/while/lstm_cell_20/MatMulç
2lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp=lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype024
2lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp×
#lstm_20/while/lstm_cell_20/MatMul_1MatMullstm_20_while_placeholder_2:lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2%
#lstm_20/while/lstm_cell_20/MatMul_1Ï
lstm_20/while/lstm_cell_20/addAddV2+lstm_20/while/lstm_cell_20/MatMul:product:0-lstm_20/while/lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2 
lstm_20/while/lstm_cell_20/addà
1lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp<lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype023
1lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOpÜ
"lstm_20/while/lstm_cell_20/BiasAddBiasAdd"lstm_20/while/lstm_cell_20/add:z:09lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2$
"lstm_20/while/lstm_cell_20/BiasAdd
*lstm_20/while/lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2,
*lstm_20/while/lstm_cell_20/split/split_dim
 lstm_20/while/lstm_cell_20/splitSplit3lstm_20/while/lstm_cell_20/split/split_dim:output:0+lstm_20/while/lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2"
 lstm_20/while/lstm_cell_20/split
 lstm_20/while/lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2"
 lstm_20/while/lstm_cell_20/Const
"lstm_20/while/lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_20/while/lstm_cell_20/Const_1Æ
lstm_20/while/lstm_cell_20/MulMul)lstm_20/while/lstm_cell_20/split:output:0)lstm_20/while/lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22 
lstm_20/while/lstm_cell_20/MulÇ
 lstm_20/while/lstm_cell_20/Add_1AddV2"lstm_20/while/lstm_cell_20/Mul:z:0+lstm_20/while/lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/Add_1­
2lstm_20/while/lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?24
2lstm_20/while/lstm_cell_20/clip_by_value/Minimum/yû
0lstm_20/while/lstm_cell_20/clip_by_value/MinimumMinimum$lstm_20/while/lstm_cell_20/Add_1:z:0;lstm_20/while/lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222
0lstm_20/while/lstm_cell_20/clip_by_value/Minimum
*lstm_20/while/lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_20/while/lstm_cell_20/clip_by_value/yó
(lstm_20/while/lstm_cell_20/clip_by_valueMaximum4lstm_20/while/lstm_cell_20/clip_by_value/Minimum:z:03lstm_20/while/lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22*
(lstm_20/while/lstm_cell_20/clip_by_value
"lstm_20/while/lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_20/while/lstm_cell_20/Const_2
"lstm_20/while/lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_20/while/lstm_cell_20/Const_3Ì
 lstm_20/while/lstm_cell_20/Mul_1Mul)lstm_20/while/lstm_cell_20/split:output:1+lstm_20/while/lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/Mul_1É
 lstm_20/while/lstm_cell_20/Add_2AddV2$lstm_20/while/lstm_cell_20/Mul_1:z:0+lstm_20/while/lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/Add_2±
4lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum/y
2lstm_20/while/lstm_cell_20/clip_by_value_1/MinimumMinimum$lstm_20/while/lstm_cell_20/Add_2:z:0=lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:224
2lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum¡
,lstm_20/while/lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_20/while/lstm_cell_20/clip_by_value_1/yû
*lstm_20/while/lstm_cell_20/clip_by_value_1Maximum6lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum:z:05lstm_20/while/lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22,
*lstm_20/while/lstm_cell_20/clip_by_value_1Á
 lstm_20/while/lstm_cell_20/mul_2Mul.lstm_20/while/lstm_cell_20/clip_by_value_1:z:0lstm_20_while_placeholder_3*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/mul_2
lstm_20/while/lstm_cell_20/TanhTanh)lstm_20/while/lstm_cell_20/split:output:2*
T0*
_output_shapes

:22!
lstm_20/while/lstm_cell_20/TanhÇ
 lstm_20/while/lstm_cell_20/mul_3Mul,lstm_20/while/lstm_cell_20/clip_by_value:z:0#lstm_20/while/lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/mul_3Â
 lstm_20/while/lstm_cell_20/add_3AddV2$lstm_20/while/lstm_cell_20/mul_2:z:0$lstm_20/while/lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/add_3
"lstm_20/while/lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_20/while/lstm_cell_20/Const_4
"lstm_20/while/lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_20/while/lstm_cell_20/Const_5Ì
 lstm_20/while/lstm_cell_20/Mul_4Mul)lstm_20/while/lstm_cell_20/split:output:3+lstm_20/while/lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/Mul_4É
 lstm_20/while/lstm_cell_20/Add_4AddV2$lstm_20/while/lstm_cell_20/Mul_4:z:0+lstm_20/while/lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/Add_4±
4lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum/y
2lstm_20/while/lstm_cell_20/clip_by_value_2/MinimumMinimum$lstm_20/while/lstm_cell_20/Add_4:z:0=lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:224
2lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum¡
,lstm_20/while/lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_20/while/lstm_cell_20/clip_by_value_2/yû
*lstm_20/while/lstm_cell_20/clip_by_value_2Maximum6lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum:z:05lstm_20/while/lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22,
*lstm_20/while/lstm_cell_20/clip_by_value_2
!lstm_20/while/lstm_cell_20/Tanh_1Tanh$lstm_20/while/lstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22#
!lstm_20/while/lstm_cell_20/Tanh_1Ë
 lstm_20/while/lstm_cell_20/mul_5Mul.lstm_20/while/lstm_cell_20/clip_by_value_2:z:0%lstm_20/while/lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22"
 lstm_20/while/lstm_cell_20/mul_5
2lstm_20/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_20_while_placeholder_1lstm_20_while_placeholder$lstm_20/while/lstm_cell_20/mul_5:z:0*
_output_shapes
: *
element_dtype024
2lstm_20/while/TensorArrayV2Write/TensorListSetIteml
lstm_20/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_20/while/add/y
lstm_20/while/addAddV2lstm_20_while_placeholderlstm_20/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_20/while/addp
lstm_20/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_20/while/add_1/y
lstm_20/while/add_1AddV2(lstm_20_while_lstm_20_while_loop_counterlstm_20/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_20/while/add_1
lstm_20/while/IdentityIdentitylstm_20/while/add_1:z:0^lstm_20/while/NoOp*
T0*
_output_shapes
: 2
lstm_20/while/Identity¦
lstm_20/while/Identity_1Identity.lstm_20_while_lstm_20_while_maximum_iterations^lstm_20/while/NoOp*
T0*
_output_shapes
: 2
lstm_20/while/Identity_1
lstm_20/while/Identity_2Identitylstm_20/while/add:z:0^lstm_20/while/NoOp*
T0*
_output_shapes
: 2
lstm_20/while/Identity_2º
lstm_20/while/Identity_3IdentityBlstm_20/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_20/while/NoOp*
T0*
_output_shapes
: 2
lstm_20/while/Identity_3¤
lstm_20/while/Identity_4Identity$lstm_20/while/lstm_cell_20/mul_5:z:0^lstm_20/while/NoOp*
T0*
_output_shapes

:22
lstm_20/while/Identity_4¤
lstm_20/while/Identity_5Identity$lstm_20/while/lstm_cell_20/add_3:z:0^lstm_20/while/NoOp*
T0*
_output_shapes

:22
lstm_20/while/Identity_5
lstm_20/while/NoOpNoOp2^lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp1^lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp3^lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_20/while/NoOp"9
lstm_20_while_identitylstm_20/while/Identity:output:0"=
lstm_20_while_identity_1!lstm_20/while/Identity_1:output:0"=
lstm_20_while_identity_2!lstm_20/while/Identity_2:output:0"=
lstm_20_while_identity_3!lstm_20/while/Identity_3:output:0"=
lstm_20_while_identity_4!lstm_20/while/Identity_4:output:0"=
lstm_20_while_identity_5!lstm_20/while/Identity_5:output:0"L
#lstm_20_while_lstm_20_strided_slice%lstm_20_while_lstm_20_strided_slice_0"z
:lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource<lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource_0"|
;lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource=lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource_0"x
9lstm_20_while_lstm_cell_20_matmul_readvariableop_resource;lstm_20_while_lstm_cell_20_matmul_readvariableop_resource_0"È
alstm_20_while_tensorarrayv2read_tensorlistgetitem_lstm_20_tensorarrayunstack_tensorlistfromtensorclstm_20_while_tensorarrayv2read_tensorlistgetitem_lstm_20_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2f
1lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp1lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp2d
0lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp0lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp2h
2lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp2lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp: 
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
¨
¼
while_cond_56992
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_56992___redundant_placeholder03
/while_while_cond_56992___redundant_placeholder13
/while_while_cond_56992___redundant_placeholder23
/while_while_cond_56992___redundant_placeholder3
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


H__inference_sequential_10_layer_call_and_return_conditional_losses_58741

inputsF
3lstm_21_lstm_cell_21_matmul_readvariableop_resource:	ÈG
5lstm_21_lstm_cell_21_matmul_1_readvariableop_resource:2J
7lstm_21_lstm_cell_21_matmul_1_readvariableop_1_resource:	2ÈC
4lstm_21_lstm_cell_21_biasadd_readvariableop_resource:	ÈD
2lstm_21_lstm_cell_21_mul_2_readvariableop_resource:2F
3lstm_20_lstm_cell_20_matmul_readvariableop_resource:	2ÈG
5lstm_20_lstm_cell_20_matmul_1_readvariableop_resource:2J
7lstm_20_lstm_cell_20_matmul_1_readvariableop_1_resource:	2ÈC
4lstm_20_lstm_cell_20_biasadd_readvariableop_resource:	ÈD
2lstm_20_lstm_cell_20_mul_2_readvariableop_resource:2M
;time_distributed_10_dense_10_matmul_readvariableop_resource:2J
<time_distributed_10_dense_10_biasadd_readvariableop_resource:
identity¢lstm_20/AssignVariableOp¢lstm_20/AssignVariableOp_1¢lstm_20/ReadVariableOp¢lstm_20/ReadVariableOp_1¢+lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp¢*lstm_20/lstm_cell_20/MatMul/ReadVariableOp¢,lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp¢.lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1¢)lstm_20/lstm_cell_20/mul_2/ReadVariableOp¢lstm_20/while¢lstm_21/AssignVariableOp¢lstm_21/AssignVariableOp_1¢lstm_21/ReadVariableOp¢lstm_21/ReadVariableOp_1¢+lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp¢*lstm_21/lstm_cell_21/MatMul/ReadVariableOp¢,lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp¢.lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1¢)lstm_21/lstm_cell_21/mul_2/ReadVariableOp¢lstm_21/while¢3time_distributed_10/dense_10/BiasAdd/ReadVariableOp¢2time_distributed_10/dense_10/MatMul/ReadVariableOp
lstm_21/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_21/transpose/perm
lstm_21/transpose	Transposeinputslstm_21/transpose/perm:output:0*
T0*"
_output_shapes
:
2
lstm_21/transposes
lstm_21/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
         2
lstm_21/Shape
lstm_21/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_21/strided_slice/stack
lstm_21/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_21/strided_slice/stack_1
lstm_21/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_21/strided_slice/stack_2
lstm_21/strided_sliceStridedSlicelstm_21/Shape:output:0$lstm_21/strided_slice/stack:output:0&lstm_21/strided_slice/stack_1:output:0&lstm_21/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_21/strided_slice
#lstm_21/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2%
#lstm_21/TensorArrayV2/element_shapeÐ
lstm_21/TensorArrayV2TensorListReserve,lstm_21/TensorArrayV2/element_shape:output:0lstm_21/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_21/TensorArrayV2Ï
=lstm_21/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      2?
=lstm_21/TensorArrayUnstack/TensorListFromTensor/element_shape
/lstm_21/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_21/transpose:y:0Flstm_21/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type021
/lstm_21/TensorArrayUnstack/TensorListFromTensor
lstm_21/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_21/strided_slice_1/stack
lstm_21/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_21/strided_slice_1/stack_1
lstm_21/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_21/strided_slice_1/stack_2£
lstm_21/strided_slice_1StridedSlicelstm_21/transpose:y:0&lstm_21/strided_slice_1/stack:output:0(lstm_21/strided_slice_1/stack_1:output:0(lstm_21/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
lstm_21/strided_slice_1Í
*lstm_21/lstm_cell_21/MatMul/ReadVariableOpReadVariableOp3lstm_21_lstm_cell_21_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02,
*lstm_21/lstm_cell_21/MatMul/ReadVariableOpÄ
lstm_21/lstm_cell_21/MatMulMatMul lstm_21/strided_slice_1:output:02lstm_21/lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_21/lstm_cell_21/MatMulÒ
,lstm_21/lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp5lstm_21_lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02.
,lstm_21/lstm_cell_21/MatMul_1/ReadVariableOpÙ
.lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1ReadVariableOp7lstm_21_lstm_cell_21_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype020
.lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1à
lstm_21/lstm_cell_21/MatMul_1MatMul4lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp:value:06lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_21/lstm_cell_21/MatMul_1·
lstm_21/lstm_cell_21/addAddV2%lstm_21/lstm_cell_21/MatMul:product:0'lstm_21/lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_21/lstm_cell_21/addÌ
+lstm_21/lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp4lstm_21_lstm_cell_21_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02-
+lstm_21/lstm_cell_21/BiasAdd/ReadVariableOpÄ
lstm_21/lstm_cell_21/BiasAddBiasAddlstm_21/lstm_cell_21/add:z:03lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_21/lstm_cell_21/BiasAdd
$lstm_21/lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2&
$lstm_21/lstm_cell_21/split/split_dimï
lstm_21/lstm_cell_21/splitSplit-lstm_21/lstm_cell_21/split/split_dim:output:0%lstm_21/lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_21/lstm_cell_21/split}
lstm_21/lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_21/lstm_cell_21/Const
lstm_21/lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_21/lstm_cell_21/Const_1®
lstm_21/lstm_cell_21/MulMul#lstm_21/lstm_cell_21/split:output:0#lstm_21/lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Mul¯
lstm_21/lstm_cell_21/Add_1AddV2lstm_21/lstm_cell_21/Mul:z:0%lstm_21/lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Add_1¡
,lstm_21/lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,lstm_21/lstm_cell_21/clip_by_value/Minimum/yã
*lstm_21/lstm_cell_21/clip_by_value/MinimumMinimumlstm_21/lstm_cell_21/Add_1:z:05lstm_21/lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22,
*lstm_21/lstm_cell_21/clip_by_value/Minimum
$lstm_21/lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_21/lstm_cell_21/clip_by_value/yÛ
"lstm_21/lstm_cell_21/clip_by_valueMaximum.lstm_21/lstm_cell_21/clip_by_value/Minimum:z:0-lstm_21/lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22$
"lstm_21/lstm_cell_21/clip_by_value
lstm_21/lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_21/lstm_cell_21/Const_2
lstm_21/lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_21/lstm_cell_21/Const_3´
lstm_21/lstm_cell_21/Mul_1Mul#lstm_21/lstm_cell_21/split:output:1%lstm_21/lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Mul_1±
lstm_21/lstm_cell_21/Add_2AddV2lstm_21/lstm_cell_21/Mul_1:z:0%lstm_21/lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Add_2¥
.lstm_21/lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_21/lstm_cell_21/clip_by_value_1/Minimum/yé
,lstm_21/lstm_cell_21/clip_by_value_1/MinimumMinimumlstm_21/lstm_cell_21/Add_2:z:07lstm_21/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22.
,lstm_21/lstm_cell_21/clip_by_value_1/Minimum
&lstm_21/lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_21/lstm_cell_21/clip_by_value_1/yã
$lstm_21/lstm_cell_21/clip_by_value_1Maximum0lstm_21/lstm_cell_21/clip_by_value_1/Minimum:z:0/lstm_21/lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22&
$lstm_21/lstm_cell_21/clip_by_value_1É
)lstm_21/lstm_cell_21/mul_2/ReadVariableOpReadVariableOp2lstm_21_lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02+
)lstm_21/lstm_cell_21/mul_2/ReadVariableOpÅ
lstm_21/lstm_cell_21/mul_2Mul(lstm_21/lstm_cell_21/clip_by_value_1:z:01lstm_21/lstm_cell_21/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/mul_2
lstm_21/lstm_cell_21/TanhTanh#lstm_21/lstm_cell_21/split:output:2*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Tanh¯
lstm_21/lstm_cell_21/mul_3Mul&lstm_21/lstm_cell_21/clip_by_value:z:0lstm_21/lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/mul_3ª
lstm_21/lstm_cell_21/add_3AddV2lstm_21/lstm_cell_21/mul_2:z:0lstm_21/lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/add_3
lstm_21/lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_21/lstm_cell_21/Const_4
lstm_21/lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_21/lstm_cell_21/Const_5´
lstm_21/lstm_cell_21/Mul_4Mul#lstm_21/lstm_cell_21/split:output:3%lstm_21/lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Mul_4±
lstm_21/lstm_cell_21/Add_4AddV2lstm_21/lstm_cell_21/Mul_4:z:0%lstm_21/lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Add_4¥
.lstm_21/lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_21/lstm_cell_21/clip_by_value_2/Minimum/yé
,lstm_21/lstm_cell_21/clip_by_value_2/MinimumMinimumlstm_21/lstm_cell_21/Add_4:z:07lstm_21/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22.
,lstm_21/lstm_cell_21/clip_by_value_2/Minimum
&lstm_21/lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_21/lstm_cell_21/clip_by_value_2/yã
$lstm_21/lstm_cell_21/clip_by_value_2Maximum0lstm_21/lstm_cell_21/clip_by_value_2/Minimum:z:0/lstm_21/lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22&
$lstm_21/lstm_cell_21/clip_by_value_2
lstm_21/lstm_cell_21/Tanh_1Tanhlstm_21/lstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/Tanh_1³
lstm_21/lstm_cell_21/mul_5Mul(lstm_21/lstm_cell_21/clip_by_value_2:z:0lstm_21/lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_21/lstm_cell_21/mul_5
%lstm_21/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2'
%lstm_21/TensorArrayV2_1/element_shapeÖ
lstm_21/TensorArrayV2_1TensorListReserve.lstm_21/TensorArrayV2_1/element_shape:output:0lstm_21/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_21/TensorArrayV2_1^
lstm_21/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_21/time¦
lstm_21/ReadVariableOpReadVariableOp5lstm_21_lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_21/ReadVariableOp§
lstm_21/ReadVariableOp_1ReadVariableOp2lstm_21_lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_21/ReadVariableOp_1
 lstm_21/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2"
 lstm_21/while/maximum_iterationsz
lstm_21/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_21/while/loop_counterí
lstm_21/whileWhile#lstm_21/while/loop_counter:output:0)lstm_21/while/maximum_iterations:output:0lstm_21/time:output:0 lstm_21/TensorArrayV2_1:handle:0lstm_21/ReadVariableOp:value:0 lstm_21/ReadVariableOp_1:value:0lstm_21/strided_slice:output:0?lstm_21/TensorArrayUnstack/TensorListFromTensor:output_handle:03lstm_21_lstm_cell_21_matmul_readvariableop_resource7lstm_21_lstm_cell_21_matmul_1_readvariableop_1_resource4lstm_21_lstm_cell_21_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *$
bodyR
lstm_21_while_body_58450*$
condR
lstm_21_while_cond_58449*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
lstm_21/whileÅ
8lstm_21/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2:
8lstm_21/TensorArrayV2Stack/TensorListStack/element_shapeÿ
*lstm_21/TensorArrayV2Stack/TensorListStackTensorListStacklstm_21/while:output:3Alstm_21/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02,
*lstm_21/TensorArrayV2Stack/TensorListStack
lstm_21/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
lstm_21/strided_slice_2/stack
lstm_21/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2!
lstm_21/strided_slice_2/stack_1
lstm_21/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_21/strided_slice_2/stack_2Á
lstm_21/strided_slice_2StridedSlice3lstm_21/TensorArrayV2Stack/TensorListStack:tensor:0&lstm_21/strided_slice_2/stack:output:0(lstm_21/strided_slice_2/stack_1:output:0(lstm_21/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_21/strided_slice_2
lstm_21/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_21/transpose_1/perm¼
lstm_21/transpose_1	Transpose3lstm_21/TensorArrayV2Stack/TensorListStack:tensor:0!lstm_21/transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
lstm_21/transpose_1v
lstm_21/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_21/runtime
lstm_21/AssignVariableOpAssignVariableOp5lstm_21_lstm_cell_21_matmul_1_readvariableop_resourcelstm_21/while:output:4^lstm_21/ReadVariableOp-^lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_21/AssignVariableOp
lstm_21/AssignVariableOp_1AssignVariableOp2lstm_21_lstm_cell_21_mul_2_readvariableop_resourcelstm_21/while:output:5^lstm_21/ReadVariableOp_1*^lstm_21/lstm_cell_21/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_21/AssignVariableOp_1
lstm_20/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_20/transpose/perm
lstm_20/transpose	Transposelstm_21/transpose_1:y:0lstm_20/transpose/perm:output:0*
T0*"
_output_shapes
:
22
lstm_20/transposes
lstm_20/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
      2   2
lstm_20/Shape
lstm_20/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_20/strided_slice/stack
lstm_20/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_20/strided_slice/stack_1
lstm_20/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_20/strided_slice/stack_2
lstm_20/strided_sliceStridedSlicelstm_20/Shape:output:0$lstm_20/strided_slice/stack:output:0&lstm_20/strided_slice/stack_1:output:0&lstm_20/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_20/strided_slice
#lstm_20/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2%
#lstm_20/TensorArrayV2/element_shapeÐ
lstm_20/TensorArrayV2TensorListReserve,lstm_20/TensorArrayV2/element_shape:output:0lstm_20/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_20/TensorArrayV2Ï
=lstm_20/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2?
=lstm_20/TensorArrayUnstack/TensorListFromTensor/element_shape
/lstm_20/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_20/transpose:y:0Flstm_20/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type021
/lstm_20/TensorArrayUnstack/TensorListFromTensor
lstm_20/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_20/strided_slice_1/stack
lstm_20/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_20/strided_slice_1/stack_1
lstm_20/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_20/strided_slice_1/stack_2£
lstm_20/strided_slice_1StridedSlicelstm_20/transpose:y:0&lstm_20/strided_slice_1/stack:output:0(lstm_20/strided_slice_1/stack_1:output:0(lstm_20/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_20/strided_slice_1Í
*lstm_20/lstm_cell_20/MatMul/ReadVariableOpReadVariableOp3lstm_20_lstm_cell_20_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02,
*lstm_20/lstm_cell_20/MatMul/ReadVariableOpÄ
lstm_20/lstm_cell_20/MatMulMatMul lstm_20/strided_slice_1:output:02lstm_20/lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_20/lstm_cell_20/MatMulÒ
,lstm_20/lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp5lstm_20_lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02.
,lstm_20/lstm_cell_20/MatMul_1/ReadVariableOpÙ
.lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1ReadVariableOp7lstm_20_lstm_cell_20_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype020
.lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1à
lstm_20/lstm_cell_20/MatMul_1MatMul4lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp:value:06lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_20/lstm_cell_20/MatMul_1·
lstm_20/lstm_cell_20/addAddV2%lstm_20/lstm_cell_20/MatMul:product:0'lstm_20/lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_20/lstm_cell_20/addÌ
+lstm_20/lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp4lstm_20_lstm_cell_20_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02-
+lstm_20/lstm_cell_20/BiasAdd/ReadVariableOpÄ
lstm_20/lstm_cell_20/BiasAddBiasAddlstm_20/lstm_cell_20/add:z:03lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_20/lstm_cell_20/BiasAdd
$lstm_20/lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2&
$lstm_20/lstm_cell_20/split/split_dimï
lstm_20/lstm_cell_20/splitSplit-lstm_20/lstm_cell_20/split/split_dim:output:0%lstm_20/lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_20/lstm_cell_20/split}
lstm_20/lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_20/lstm_cell_20/Const
lstm_20/lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_20/lstm_cell_20/Const_1®
lstm_20/lstm_cell_20/MulMul#lstm_20/lstm_cell_20/split:output:0#lstm_20/lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Mul¯
lstm_20/lstm_cell_20/Add_1AddV2lstm_20/lstm_cell_20/Mul:z:0%lstm_20/lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Add_1¡
,lstm_20/lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,lstm_20/lstm_cell_20/clip_by_value/Minimum/yã
*lstm_20/lstm_cell_20/clip_by_value/MinimumMinimumlstm_20/lstm_cell_20/Add_1:z:05lstm_20/lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22,
*lstm_20/lstm_cell_20/clip_by_value/Minimum
$lstm_20/lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_20/lstm_cell_20/clip_by_value/yÛ
"lstm_20/lstm_cell_20/clip_by_valueMaximum.lstm_20/lstm_cell_20/clip_by_value/Minimum:z:0-lstm_20/lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22$
"lstm_20/lstm_cell_20/clip_by_value
lstm_20/lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_20/lstm_cell_20/Const_2
lstm_20/lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_20/lstm_cell_20/Const_3´
lstm_20/lstm_cell_20/Mul_1Mul#lstm_20/lstm_cell_20/split:output:1%lstm_20/lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Mul_1±
lstm_20/lstm_cell_20/Add_2AddV2lstm_20/lstm_cell_20/Mul_1:z:0%lstm_20/lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Add_2¥
.lstm_20/lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_20/lstm_cell_20/clip_by_value_1/Minimum/yé
,lstm_20/lstm_cell_20/clip_by_value_1/MinimumMinimumlstm_20/lstm_cell_20/Add_2:z:07lstm_20/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22.
,lstm_20/lstm_cell_20/clip_by_value_1/Minimum
&lstm_20/lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_20/lstm_cell_20/clip_by_value_1/yã
$lstm_20/lstm_cell_20/clip_by_value_1Maximum0lstm_20/lstm_cell_20/clip_by_value_1/Minimum:z:0/lstm_20/lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22&
$lstm_20/lstm_cell_20/clip_by_value_1É
)lstm_20/lstm_cell_20/mul_2/ReadVariableOpReadVariableOp2lstm_20_lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02+
)lstm_20/lstm_cell_20/mul_2/ReadVariableOpÅ
lstm_20/lstm_cell_20/mul_2Mul(lstm_20/lstm_cell_20/clip_by_value_1:z:01lstm_20/lstm_cell_20/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/mul_2
lstm_20/lstm_cell_20/TanhTanh#lstm_20/lstm_cell_20/split:output:2*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Tanh¯
lstm_20/lstm_cell_20/mul_3Mul&lstm_20/lstm_cell_20/clip_by_value:z:0lstm_20/lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/mul_3ª
lstm_20/lstm_cell_20/add_3AddV2lstm_20/lstm_cell_20/mul_2:z:0lstm_20/lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/add_3
lstm_20/lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_20/lstm_cell_20/Const_4
lstm_20/lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_20/lstm_cell_20/Const_5´
lstm_20/lstm_cell_20/Mul_4Mul#lstm_20/lstm_cell_20/split:output:3%lstm_20/lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Mul_4±
lstm_20/lstm_cell_20/Add_4AddV2lstm_20/lstm_cell_20/Mul_4:z:0%lstm_20/lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Add_4¥
.lstm_20/lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_20/lstm_cell_20/clip_by_value_2/Minimum/yé
,lstm_20/lstm_cell_20/clip_by_value_2/MinimumMinimumlstm_20/lstm_cell_20/Add_4:z:07lstm_20/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22.
,lstm_20/lstm_cell_20/clip_by_value_2/Minimum
&lstm_20/lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_20/lstm_cell_20/clip_by_value_2/yã
$lstm_20/lstm_cell_20/clip_by_value_2Maximum0lstm_20/lstm_cell_20/clip_by_value_2/Minimum:z:0/lstm_20/lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22&
$lstm_20/lstm_cell_20/clip_by_value_2
lstm_20/lstm_cell_20/Tanh_1Tanhlstm_20/lstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/Tanh_1³
lstm_20/lstm_cell_20/mul_5Mul(lstm_20/lstm_cell_20/clip_by_value_2:z:0lstm_20/lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_20/lstm_cell_20/mul_5
%lstm_20/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2'
%lstm_20/TensorArrayV2_1/element_shapeÖ
lstm_20/TensorArrayV2_1TensorListReserve.lstm_20/TensorArrayV2_1/element_shape:output:0lstm_20/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_20/TensorArrayV2_1^
lstm_20/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_20/time¦
lstm_20/ReadVariableOpReadVariableOp5lstm_20_lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_20/ReadVariableOp§
lstm_20/ReadVariableOp_1ReadVariableOp2lstm_20_lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
lstm_20/ReadVariableOp_1
 lstm_20/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2"
 lstm_20/while/maximum_iterationsz
lstm_20/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_20/while/loop_counterí
lstm_20/whileWhile#lstm_20/while/loop_counter:output:0)lstm_20/while/maximum_iterations:output:0lstm_20/time:output:0 lstm_20/TensorArrayV2_1:handle:0lstm_20/ReadVariableOp:value:0 lstm_20/ReadVariableOp_1:value:0lstm_20/strided_slice:output:0?lstm_20/TensorArrayUnstack/TensorListFromTensor:output_handle:03lstm_20_lstm_cell_20_matmul_readvariableop_resource7lstm_20_lstm_cell_20_matmul_1_readvariableop_1_resource4lstm_20_lstm_cell_20_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :2:2: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *$
bodyR
lstm_20_while_body_58624*$
condR
lstm_20_while_cond_58623*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
lstm_20/whileÅ
8lstm_20/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2:
8lstm_20/TensorArrayV2Stack/TensorListStack/element_shapeÿ
*lstm_20/TensorArrayV2Stack/TensorListStackTensorListStacklstm_20/while:output:3Alstm_20/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02,
*lstm_20/TensorArrayV2Stack/TensorListStack
lstm_20/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
lstm_20/strided_slice_2/stack
lstm_20/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2!
lstm_20/strided_slice_2/stack_1
lstm_20/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_20/strided_slice_2/stack_2Á
lstm_20/strided_slice_2StridedSlice3lstm_20/TensorArrayV2Stack/TensorListStack:tensor:0&lstm_20/strided_slice_2/stack:output:0(lstm_20/strided_slice_2/stack_1:output:0(lstm_20/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_20/strided_slice_2
lstm_20/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_20/transpose_1/perm¼
lstm_20/transpose_1	Transpose3lstm_20/TensorArrayV2Stack/TensorListStack:tensor:0!lstm_20/transpose_1/perm:output:0*
T0*"
_output_shapes
:
22
lstm_20/transpose_1v
lstm_20/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_20/runtime
lstm_20/AssignVariableOpAssignVariableOp5lstm_20_lstm_cell_20_matmul_1_readvariableop_resourcelstm_20/while:output:4^lstm_20/ReadVariableOp-^lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_20/AssignVariableOp
lstm_20/AssignVariableOp_1AssignVariableOp2lstm_20_lstm_cell_20_mul_2_readvariableop_resourcelstm_20/while:output:5^lstm_20/ReadVariableOp_1*^lstm_20/lstm_cell_20/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_20/AssignVariableOp_1
!time_distributed_10/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2#
!time_distributed_10/Reshape/shape³
time_distributed_10/ReshapeReshapelstm_20/transpose_1:y:0*time_distributed_10/Reshape/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_10/Reshapeä
2time_distributed_10/dense_10/MatMul/ReadVariableOpReadVariableOp;time_distributed_10_dense_10_matmul_readvariableop_resource*
_output_shapes

:2*
dtype024
2time_distributed_10/dense_10/MatMul/ReadVariableOpß
#time_distributed_10/dense_10/MatMulMatMul$time_distributed_10/Reshape:output:0:time_distributed_10/dense_10/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
2%
#time_distributed_10/dense_10/MatMulã
3time_distributed_10/dense_10/BiasAdd/ReadVariableOpReadVariableOp<time_distributed_10_dense_10_biasadd_readvariableop_resource*
_output_shapes
:*
dtype025
3time_distributed_10/dense_10/BiasAdd/ReadVariableOpì
$time_distributed_10/dense_10/BiasAddBiasAdd-time_distributed_10/dense_10/MatMul:product:0;time_distributed_10/dense_10/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
2&
$time_distributed_10/dense_10/BiasAdd
#time_distributed_10/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      2%
#time_distributed_10/Reshape_1/shapeÓ
time_distributed_10/Reshape_1Reshape-time_distributed_10/dense_10/BiasAdd:output:0,time_distributed_10/Reshape_1/shape:output:0*
T0*"
_output_shapes
:
2
time_distributed_10/Reshape_1
#time_distributed_10/Reshape_2/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2%
#time_distributed_10/Reshape_2/shape¹
time_distributed_10/Reshape_2Reshapelstm_20/transpose_1:y:0,time_distributed_10/Reshape_2/shape:output:0*
T0*
_output_shapes

:
22
time_distributed_10/Reshape_2|
IdentityIdentity&time_distributed_10/Reshape_1:output:0^NoOp*
T0*"
_output_shapes
:
2

Identityÿ
NoOpNoOp^lstm_20/AssignVariableOp^lstm_20/AssignVariableOp_1^lstm_20/ReadVariableOp^lstm_20/ReadVariableOp_1,^lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp+^lstm_20/lstm_cell_20/MatMul/ReadVariableOp-^lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp/^lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1*^lstm_20/lstm_cell_20/mul_2/ReadVariableOp^lstm_20/while^lstm_21/AssignVariableOp^lstm_21/AssignVariableOp_1^lstm_21/ReadVariableOp^lstm_21/ReadVariableOp_1,^lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp+^lstm_21/lstm_cell_21/MatMul/ReadVariableOp-^lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp/^lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1*^lstm_21/lstm_cell_21/mul_2/ReadVariableOp^lstm_21/while4^time_distributed_10/dense_10/BiasAdd/ReadVariableOp3^time_distributed_10/dense_10/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:
: : : : : : : : : : : : 24
lstm_20/AssignVariableOplstm_20/AssignVariableOp28
lstm_20/AssignVariableOp_1lstm_20/AssignVariableOp_120
lstm_20/ReadVariableOplstm_20/ReadVariableOp24
lstm_20/ReadVariableOp_1lstm_20/ReadVariableOp_12Z
+lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp+lstm_20/lstm_cell_20/BiasAdd/ReadVariableOp2X
*lstm_20/lstm_cell_20/MatMul/ReadVariableOp*lstm_20/lstm_cell_20/MatMul/ReadVariableOp2\
,lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp,lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp2`
.lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_1.lstm_20/lstm_cell_20/MatMul_1/ReadVariableOp_12V
)lstm_20/lstm_cell_20/mul_2/ReadVariableOp)lstm_20/lstm_cell_20/mul_2/ReadVariableOp2
lstm_20/whilelstm_20/while24
lstm_21/AssignVariableOplstm_21/AssignVariableOp28
lstm_21/AssignVariableOp_1lstm_21/AssignVariableOp_120
lstm_21/ReadVariableOplstm_21/ReadVariableOp24
lstm_21/ReadVariableOp_1lstm_21/ReadVariableOp_12Z
+lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp+lstm_21/lstm_cell_21/BiasAdd/ReadVariableOp2X
*lstm_21/lstm_cell_21/MatMul/ReadVariableOp*lstm_21/lstm_cell_21/MatMul/ReadVariableOp2\
,lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp,lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp2`
.lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_1.lstm_21/lstm_cell_21/MatMul_1/ReadVariableOp_12V
)lstm_21/lstm_cell_21/mul_2/ReadVariableOp)lstm_21/lstm_cell_21/mul_2/ReadVariableOp2
lstm_21/whilelstm_21/while2j
3time_distributed_10/dense_10/BiasAdd/ReadVariableOp3time_distributed_10/dense_10/BiasAdd/ReadVariableOp2h
2time_distributed_10/dense_10/MatMul/ReadVariableOp2time_distributed_10/dense_10/MatMul/ReadVariableOp:J F
"
_output_shapes
:

 
_user_specified_nameinputs
Ä
£
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_57312

inputs9
'dense_10_matmul_readvariableop_resource:26
(dense_10_biasadd_readvariableop_resource:
identity¢dense_10/BiasAdd/ReadVariableOp¢dense_10/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2
Reshape/shapef
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes

:
22	
Reshape¨
dense_10/MatMul/ReadVariableOpReadVariableOp'dense_10_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02 
dense_10/MatMul/ReadVariableOp
dense_10/MatMulMatMulReshape:output:0&dense_10/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_10/MatMul§
dense_10/BiasAdd/ReadVariableOpReadVariableOp(dense_10_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02!
dense_10/BiasAdd/ReadVariableOp
dense_10/BiasAddBiasAdddense_10/MatMul:product:0'dense_10/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_10/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      2
Reshape_1/shape
	Reshape_1Reshapedense_10/BiasAdd:output:0Reshape_1/shape:output:0*
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

Identity
NoOpNoOp ^dense_10/BiasAdd/ReadVariableOp^dense_10/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:
2: : 2B
dense_10/BiasAdd/ReadVariableOpdense_10/BiasAdd/ReadVariableOp2@
dense_10/MatMul/ReadVariableOpdense_10/MatMul/ReadVariableOp:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
Ä
£
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_57373

inputs9
'dense_10_matmul_readvariableop_resource:26
(dense_10_biasadd_readvariableop_resource:
identity¢dense_10/BiasAdd/ReadVariableOp¢dense_10/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2
Reshape/shapef
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes

:
22	
Reshape¨
dense_10/MatMul/ReadVariableOpReadVariableOp'dense_10_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02 
dense_10/MatMul/ReadVariableOp
dense_10/MatMulMatMulReshape:output:0&dense_10/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_10/MatMul§
dense_10/BiasAdd/ReadVariableOpReadVariableOp(dense_10_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02!
dense_10/BiasAdd/ReadVariableOp
dense_10/BiasAddBiasAdddense_10/MatMul:product:0'dense_10/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes

:
2
dense_10/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      2
Reshape_1/shape
	Reshape_1Reshapedense_10/BiasAdd:output:0Reshape_1/shape:output:0*
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

Identity
NoOpNoOp ^dense_10/BiasAdd/ReadVariableOp^dense_10/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:
2: : 2B
dense_10/BiasAdd/ReadVariableOpdense_10/BiasAdd/ReadVariableOp2@
dense_10/MatMul/ReadVariableOpdense_10/MatMul/ReadVariableOp:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
¨
¼
while_cond_59999
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_59999___redundant_placeholder03
/while_while_cond_59999___redundant_placeholder13
/while_while_cond_59999___redundant_placeholder23
/while_while_cond_59999___redundant_placeholder3
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
ùn
´
B__inference_lstm_21_layer_call_and_return_conditional_losses_57783

inputs>
+lstm_cell_21_matmul_readvariableop_resource:	È?
-lstm_cell_21_matmul_1_readvariableop_resource:2B
/lstm_cell_21_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_21_biasadd_readvariableop_resource:	È<
*lstm_cell_21_mul_2_readvariableop_resource:2
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_21/BiasAdd/ReadVariableOp¢"lstm_cell_21/MatMul/ReadVariableOp¢$lstm_cell_21/MatMul_1/ReadVariableOp¢&lstm_cell_21/MatMul_1/ReadVariableOp_1¢!lstm_cell_21/mul_2/ReadVariableOp¢whileu
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_21/MatMul/ReadVariableOpReadVariableOp+lstm_cell_21_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02$
"lstm_cell_21/MatMul/ReadVariableOp¤
lstm_cell_21/MatMulMatMulstrided_slice_1:output:0*lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/MatMulº
$lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$lstm_cell_21/MatMul_1/ReadVariableOpÁ
&lstm_cell_21/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_21_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_21/MatMul_1/ReadVariableOp_1À
lstm_cell_21/MatMul_1MatMul,lstm_cell_21/MatMul_1/ReadVariableOp:value:0.lstm_cell_21/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/MatMul_1
lstm_cell_21/addAddV2lstm_cell_21/MatMul:product:0lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_cell_21/add´
#lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_21_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_21/BiasAdd/ReadVariableOp¤
lstm_cell_21/BiasAddBiasAddlstm_cell_21/add:z:0+lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_21/BiasAdd~
lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_21/split/split_dimÏ
lstm_cell_21/splitSplit%lstm_cell_21/split/split_dim:output:0lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_21/splitm
lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Constq
lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_1
lstm_cell_21/MulMullstm_cell_21/split:output:0lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul
lstm_cell_21/Add_1AddV2lstm_cell_21/Mul:z:0lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_1
$lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_21/clip_by_value/Minimum/yÃ
"lstm_cell_21/clip_by_value/MinimumMinimumlstm_cell_21/Add_1:z:0-lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22$
"lstm_cell_21/clip_by_value/Minimum
lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_21/clip_by_value/y»
lstm_cell_21/clip_by_valueMaximum&lstm_cell_21/clip_by_value/Minimum:z:0%lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_valueq
lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Const_2q
lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_3
lstm_cell_21/Mul_1Mullstm_cell_21/split:output:1lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul_1
lstm_cell_21/Add_2AddV2lstm_cell_21/Mul_1:z:0lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_2
&lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_21/clip_by_value_1/Minimum/yÉ
$lstm_cell_21/clip_by_value_1/MinimumMinimumlstm_cell_21/Add_2:z:0/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_21/clip_by_value_1/Minimum
lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_21/clip_by_value_1/yÃ
lstm_cell_21/clip_by_value_1Maximum(lstm_cell_21/clip_by_value_1/Minimum:z:0'lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_value_1±
!lstm_cell_21/mul_2/ReadVariableOpReadVariableOp*lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02#
!lstm_cell_21/mul_2/ReadVariableOp¥
lstm_cell_21/mul_2Mul lstm_cell_21/clip_by_value_1:z:0)lstm_cell_21/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_2t
lstm_cell_21/TanhTanhlstm_cell_21/split:output:2*
T0*
_output_shapes

:22
lstm_cell_21/Tanh
lstm_cell_21/mul_3Mullstm_cell_21/clip_by_value:z:0lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_3
lstm_cell_21/add_3AddV2lstm_cell_21/mul_2:z:0lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_21/add_3q
lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_21/Const_4q
lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_21/Const_5
lstm_cell_21/Mul_4Mullstm_cell_21/split:output:3lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Mul_4
lstm_cell_21/Add_4AddV2lstm_cell_21/Mul_4:z:0lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_21/Add_4
&lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_21/clip_by_value_2/Minimum/yÉ
$lstm_cell_21/clip_by_value_2/MinimumMinimumlstm_cell_21/Add_4:z:0/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_21/clip_by_value_2/Minimum
lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_21/clip_by_value_2/yÃ
lstm_cell_21/clip_by_value_2Maximum(lstm_cell_21/clip_by_value_2/Minimum:z:0'lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_21/clip_by_value_2s
lstm_cell_21/Tanh_1Tanhlstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_21/Tanh_1
lstm_cell_21/mul_5Mul lstm_cell_21/clip_by_value_2:z:0lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_21/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
time
ReadVariableOpReadVariableOp-lstm_cell_21_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_21_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterõ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_21_matmul_readvariableop_resource/lstm_cell_21_matmul_1_readvariableop_1_resource,lstm_cell_21_biasadd_readvariableop_resource*
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
while_body_57678*
condR
while_cond_57677*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm
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
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_21_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_21/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_21_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_21/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_21/BiasAdd/ReadVariableOp#^lstm_cell_21/MatMul/ReadVariableOp%^lstm_cell_21/MatMul_1/ReadVariableOp'^lstm_cell_21/MatMul_1/ReadVariableOp_1"^lstm_cell_21/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_21/BiasAdd/ReadVariableOp#lstm_cell_21/BiasAdd/ReadVariableOp2H
"lstm_cell_21/MatMul/ReadVariableOp"lstm_cell_21/MatMul/ReadVariableOp2L
$lstm_cell_21/MatMul_1/ReadVariableOp$lstm_cell_21/MatMul_1/ReadVariableOp2P
&lstm_cell_21/MatMul_1/ReadVariableOp_1&lstm_cell_21/MatMul_1/ReadVariableOp_12F
!lstm_cell_21/mul_2/ReadVariableOp!lstm_cell_21/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:

 
_user_specified_nameinputs
±0
Ô
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_55300

inputs
states:2
states_1:21
matmul_readvariableop_resource:	È5
"matmul_1_readvariableop_1_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp¢MatMul_1/ReadVariableOp_1¢mul_2/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:2*
dtype02
MatMul_1/ReadVariableOp
MatMul_1/ReadVariableOp_1ReadVariableOp"matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOp_1
MatMul_1MatMulMatMul_1/ReadVariableOp:value:0!MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2Ì
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
ï,

G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_55530

inputs

states
states_11
matmul_readvariableop_resource:	È3
 matmul_1_readvariableop_resource:	2È.
biasadd_readvariableop_resource:	È
identity

identity_1

identity_2¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOp¢MatMul_1/ReadVariableOp
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	È*
dtype02
MatMul/ReadVariableOpk
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2	
BiasAddd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim
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
 *ÍÌL>2
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
 *  ?2
clip_by_value/Minimum/y
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
clip_by_value/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_1/Minimum/y
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
clip_by_value_1/y
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
 *ÍÌL>2	
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
 *  ?2
clip_by_value_2/Minimum/y
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
clip_by_value_2/y
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

Identity_2
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
	
ð
'__inference_lstm_20_layer_call_fn_60298
inputs_0
unknown:2
	unknown_0:2
	unknown_1:	2È
	unknown_2:	2È
	unknown_3:	È
identity¢StatefulPartitionedCall
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_20_layer_call_and_return_conditional_losses_562182
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:ÿÿÿÿÿÿÿÿÿ2: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
"
_user_specified_name
inputs/0
¨
¼
while_cond_60177
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_60177___redundant_placeholder03
/while_while_cond_60177___redundant_placeholder13
/while_while_cond_60177___redundant_placeholder23
/while_while_cond_60177___redundant_placeholder3
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
Þ
î
'__inference_lstm_21_layer_call_fn_59571

inputs
unknown:	È
	unknown_0:2
	unknown_1:	2È
	unknown_2:	È
	unknown_3:2
identity¢StatefulPartitionedCall
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
GPU 2J 8 *K
fFRD
B__inference_lstm_21_layer_call_and_return_conditional_losses_577832
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
Þ
î
'__inference_lstm_21_layer_call_fn_59556

inputs
unknown:	È
	unknown_0:2
	unknown_1:	2È
	unknown_2:	È
	unknown_3:2
identity¢StatefulPartitionedCall
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
GPU 2J 8 *K
fFRD
B__inference_lstm_21_layer_call_and_return_conditional_losses_570982
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
ñ
 
3__inference_time_distributed_10_layer_call_fn_60449

inputs
unknown:2
	unknown_0:
identity¢StatefulPartitionedCallù
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
GPU 2J 8 *W
fRRP
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_573732
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
ùn
´
B__inference_lstm_20_layer_call_and_return_conditional_losses_57575

inputs>
+lstm_cell_20_matmul_readvariableop_resource:	2È?
-lstm_cell_20_matmul_1_readvariableop_resource:2B
/lstm_cell_20_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_20_biasadd_readvariableop_resource:	È<
*lstm_cell_20_mul_2_readvariableop_resource:2
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_20/BiasAdd/ReadVariableOp¢"lstm_cell_20/MatMul/ReadVariableOp¢$lstm_cell_20/MatMul_1/ReadVariableOp¢&lstm_cell_20/MatMul_1/ReadVariableOp_1¢!lstm_cell_20/mul_2/ReadVariableOp¢whileu
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
strided_slice/stack_2â
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
TensorArrayV2/element_shape°
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2¿
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   27
5TensorArrayUnstack/TensorListFromTensor/element_shapeø
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
strided_slice_1/stack_2ó
strided_slice_1StridedSlicetranspose:y:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_20/MatMul/ReadVariableOpReadVariableOp+lstm_cell_20_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02$
"lstm_cell_20/MatMul/ReadVariableOp¤
lstm_cell_20/MatMulMatMulstrided_slice_1:output:0*lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/MatMulº
$lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02&
$lstm_cell_20/MatMul_1/ReadVariableOpÁ
&lstm_cell_20/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_20_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_20/MatMul_1/ReadVariableOp_1À
lstm_cell_20/MatMul_1MatMul,lstm_cell_20/MatMul_1/ReadVariableOp:value:0.lstm_cell_20/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/MatMul_1
lstm_cell_20/addAddV2lstm_cell_20/MatMul:product:0lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2
lstm_cell_20/add´
#lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_20_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_20/BiasAdd/ReadVariableOp¤
lstm_cell_20/BiasAddBiasAddlstm_cell_20/add:z:0+lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
lstm_cell_20/BiasAdd~
lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_20/split/split_dimÏ
lstm_cell_20/splitSplit%lstm_cell_20/split/split_dim:output:0lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
lstm_cell_20/splitm
lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Constq
lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_1
lstm_cell_20/MulMullstm_cell_20/split:output:0lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul
lstm_cell_20/Add_1AddV2lstm_cell_20/Mul:z:0lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_1
$lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_20/clip_by_value/Minimum/yÃ
"lstm_cell_20/clip_by_value/MinimumMinimumlstm_cell_20/Add_1:z:0-lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22$
"lstm_cell_20/clip_by_value/Minimum
lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_20/clip_by_value/y»
lstm_cell_20/clip_by_valueMaximum&lstm_cell_20/clip_by_value/Minimum:z:0%lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_valueq
lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Const_2q
lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_3
lstm_cell_20/Mul_1Mullstm_cell_20/split:output:1lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul_1
lstm_cell_20/Add_2AddV2lstm_cell_20/Mul_1:z:0lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_2
&lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_20/clip_by_value_1/Minimum/yÉ
$lstm_cell_20/clip_by_value_1/MinimumMinimumlstm_cell_20/Add_2:z:0/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_20/clip_by_value_1/Minimum
lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_20/clip_by_value_1/yÃ
lstm_cell_20/clip_by_value_1Maximum(lstm_cell_20/clip_by_value_1/Minimum:z:0'lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_value_1±
!lstm_cell_20/mul_2/ReadVariableOpReadVariableOp*lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02#
!lstm_cell_20/mul_2/ReadVariableOp¥
lstm_cell_20/mul_2Mul lstm_cell_20/clip_by_value_1:z:0)lstm_cell_20/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_2t
lstm_cell_20/TanhTanhlstm_cell_20/split:output:2*
T0*
_output_shapes

:22
lstm_cell_20/Tanh
lstm_cell_20/mul_3Mullstm_cell_20/clip_by_value:z:0lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_3
lstm_cell_20/add_3AddV2lstm_cell_20/mul_2:z:0lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:22
lstm_cell_20/add_3q
lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_20/Const_4q
lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_20/Const_5
lstm_cell_20/Mul_4Mullstm_cell_20/split:output:3lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Mul_4
lstm_cell_20/Add_4AddV2lstm_cell_20/Mul_4:z:0lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:22
lstm_cell_20/Add_4
&lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_20/clip_by_value_2/Minimum/yÉ
$lstm_cell_20/clip_by_value_2/MinimumMinimumlstm_cell_20/Add_4:z:0/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22&
$lstm_cell_20/clip_by_value_2/Minimum
lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_20/clip_by_value_2/yÃ
lstm_cell_20/clip_by_value_2Maximum(lstm_cell_20/clip_by_value_2/Minimum:z:0'lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22
lstm_cell_20/clip_by_value_2s
lstm_cell_20/Tanh_1Tanhlstm_cell_20/add_3:z:0*
T0*
_output_shapes

:22
lstm_cell_20/Tanh_1
lstm_cell_20/mul_5Mul lstm_cell_20/clip_by_value_2:z:0lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:22
lstm_cell_20/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2
TensorArrayV2_1/element_shape¶
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
time
ReadVariableOpReadVariableOp-lstm_cell_20_matmul_1_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_20_mul_2_readvariableop_resource*
_output_shapes

:2*
dtype02
ReadVariableOp_1
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counterõ
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_20_matmul_readvariableop_resource/lstm_cell_20_matmul_1_readvariableop_1_resource,lstm_cell_20_biasadd_readvariableop_resource*
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
while_body_57470*
condR
while_cond_57469*9
output_shapes(
&: : : : :2:2: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
2*
element_dtype02$
"TensorArrayV2Stack/TensorListStack
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
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
strided_slice_2/stack_2
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
transpose_1/perm
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
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_20_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_20/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_20_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_20/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_20/BiasAdd/ReadVariableOp#^lstm_cell_20/MatMul/ReadVariableOp%^lstm_cell_20/MatMul_1/ReadVariableOp'^lstm_cell_20/MatMul_1/ReadVariableOp_1"^lstm_cell_20/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_20/BiasAdd/ReadVariableOp#lstm_cell_20/BiasAdd/ReadVariableOp2H
"lstm_cell_20/MatMul/ReadVariableOp"lstm_cell_20/MatMul/ReadVariableOp2L
$lstm_cell_20/MatMul_1/ReadVariableOp$lstm_cell_20/MatMul_1/ReadVariableOp2P
&lstm_cell_20/MatMul_1/ReadVariableOp_1&lstm_cell_20/MatMul_1/ReadVariableOp_12F
!lstm_cell_20/mul_2/ReadVariableOp!lstm_cell_20/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:
2
 
_user_specified_nameinputs
ÔY
Ë
while_body_57678
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_21_matmul_readvariableop_resource_0:	ÈH
5while_lstm_cell_21_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_21_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_21_matmul_readvariableop_resource:	ÈF
3while_lstm_cell_21_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_21_biasadd_readvariableop_resource:	È¢)while/lstm_cell_21/BiasAdd/ReadVariableOp¢(while/lstm_cell_21/MatMul/ReadVariableOp¢*while/lstm_cell_21/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_21/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_21_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02*
(while/lstm_cell_21/MatMul/ReadVariableOpÎ
while/lstm_cell_21/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_21/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/MatMulÏ
*while/lstm_cell_21/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_21_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_21/MatMul_1/ReadVariableOp·
while/lstm_cell_21/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_21/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/MatMul_1¯
while/lstm_cell_21/addAddV2#while/lstm_cell_21/MatMul:product:0%while/lstm_cell_21/MatMul_1:product:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/addÈ
)while/lstm_cell_21/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_21_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_21/BiasAdd/ReadVariableOp¼
while/lstm_cell_21/BiasAddBiasAddwhile/lstm_cell_21/add:z:01while/lstm_cell_21/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È2
while/lstm_cell_21/BiasAdd
"while/lstm_cell_21/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_21/split/split_dimç
while/lstm_cell_21/splitSplit+while/lstm_cell_21/split/split_dim:output:0#while/lstm_cell_21/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split2
while/lstm_cell_21/splity
while/lstm_cell_21/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const}
while/lstm_cell_21/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_1¦
while/lstm_cell_21/MulMul!while/lstm_cell_21/split:output:0!while/lstm_cell_21/Const:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul§
while/lstm_cell_21/Add_1AddV2while/lstm_cell_21/Mul:z:0#while/lstm_cell_21/Const_1:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_1
*while/lstm_cell_21/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_21/clip_by_value/Minimum/yÛ
(while/lstm_cell_21/clip_by_value/MinimumMinimumwhile/lstm_cell_21/Add_1:z:03while/lstm_cell_21/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22*
(while/lstm_cell_21/clip_by_value/Minimum
"while/lstm_cell_21/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_21/clip_by_value/yÓ
 while/lstm_cell_21/clip_by_valueMaximum,while/lstm_cell_21/clip_by_value/Minimum:z:0+while/lstm_cell_21/clip_by_value/y:output:0*
T0*
_output_shapes

:22"
 while/lstm_cell_21/clip_by_value}
while/lstm_cell_21/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const_2}
while/lstm_cell_21/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_3¬
while/lstm_cell_21/Mul_1Mul!while/lstm_cell_21/split:output:1#while/lstm_cell_21/Const_2:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul_1©
while/lstm_cell_21/Add_2AddV2while/lstm_cell_21/Mul_1:z:0#while/lstm_cell_21/Const_3:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_2¡
,while/lstm_cell_21/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_21/clip_by_value_1/Minimum/yá
*while/lstm_cell_21/clip_by_value_1/MinimumMinimumwhile/lstm_cell_21/Add_2:z:05while/lstm_cell_21/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_21/clip_by_value_1/Minimum
$while/lstm_cell_21/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_21/clip_by_value_1/yÛ
"while/lstm_cell_21/clip_by_value_1Maximum.while/lstm_cell_21/clip_by_value_1/Minimum:z:0-while/lstm_cell_21/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_21/clip_by_value_1¡
while/lstm_cell_21/mul_2Mul&while/lstm_cell_21/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_2
while/lstm_cell_21/TanhTanh!while/lstm_cell_21/split:output:2*
T0*
_output_shapes

:22
while/lstm_cell_21/Tanh§
while/lstm_cell_21/mul_3Mul$while/lstm_cell_21/clip_by_value:z:0while/lstm_cell_21/Tanh:y:0*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_3¢
while/lstm_cell_21/add_3AddV2while/lstm_cell_21/mul_2:z:0while/lstm_cell_21/mul_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_21/add_3}
while/lstm_cell_21/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_21/Const_4}
while/lstm_cell_21/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_21/Const_5¬
while/lstm_cell_21/Mul_4Mul!while/lstm_cell_21/split:output:3#while/lstm_cell_21/Const_4:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Mul_4©
while/lstm_cell_21/Add_4AddV2while/lstm_cell_21/Mul_4:z:0#while/lstm_cell_21/Const_5:output:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Add_4¡
,while/lstm_cell_21/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_21/clip_by_value_2/Minimum/yá
*while/lstm_cell_21/clip_by_value_2/MinimumMinimumwhile/lstm_cell_21/Add_4:z:05while/lstm_cell_21/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22,
*while/lstm_cell_21/clip_by_value_2/Minimum
$while/lstm_cell_21/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_21/clip_by_value_2/yÛ
"while/lstm_cell_21/clip_by_value_2Maximum.while/lstm_cell_21/clip_by_value_2/Minimum:z:0-while/lstm_cell_21/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22$
"while/lstm_cell_21/clip_by_value_2
while/lstm_cell_21/Tanh_1Tanhwhile/lstm_cell_21/add_3:z:0*
T0*
_output_shapes

:22
while/lstm_cell_21/Tanh_1«
while/lstm_cell_21/mul_5Mul&while/lstm_cell_21/clip_by_value_2:z:0while/lstm_cell_21/Tanh_1:y:0*
T0*
_output_shapes

:22
while/lstm_cell_21/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_21/mul_5:z:0*
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
while/Identity_2
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0^while/NoOp*
T0*
_output_shapes
: 2
while/Identity_3
while/Identity_4Identitywhile/lstm_cell_21/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_21/add_3:z:0^while/NoOp*
T0*
_output_shapes

:22
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_21/BiasAdd/ReadVariableOp)^while/lstm_cell_21/MatMul/ReadVariableOp+^while/lstm_cell_21/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2

while/NoOp")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_21_biasadd_readvariableop_resource4while_lstm_cell_21_biasadd_readvariableop_resource_0"l
3while_lstm_cell_21_matmul_1_readvariableop_resource5while_lstm_cell_21_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_21_matmul_readvariableop_resource3while_lstm_cell_21_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2V
)while/lstm_cell_21/BiasAdd/ReadVariableOp)while/lstm_cell_21/BiasAdd/ReadVariableOp2T
(while/lstm_cell_21/MatMul/ReadVariableOp(while/lstm_cell_21/MatMul/ReadVariableOp2X
*while/lstm_cell_21/MatMul_1/ReadVariableOp*while/lstm_cell_21/MatMul_1/ReadVariableOp: 
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
È
õ
,__inference_lstm_cell_21_layer_call_fn_60703

inputs
states_0
states_1
unknown:	È
	unknown_0:	2È
	unknown_1:	È
identity

identity_1

identity_2¢StatefulPartitionedCall§
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_555302
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
ø

&sequential_10_lstm_20_while_body_55106H
Dsequential_10_lstm_20_while_sequential_10_lstm_20_while_loop_counterN
Jsequential_10_lstm_20_while_sequential_10_lstm_20_while_maximum_iterations+
'sequential_10_lstm_20_while_placeholder-
)sequential_10_lstm_20_while_placeholder_1-
)sequential_10_lstm_20_while_placeholder_2-
)sequential_10_lstm_20_while_placeholder_3E
Asequential_10_lstm_20_while_sequential_10_lstm_20_strided_slice_0
sequential_10_lstm_20_while_tensorarrayv2read_tensorlistgetitem_sequential_10_lstm_20_tensorarrayunstack_tensorlistfromtensor_0\
Isequential_10_lstm_20_while_lstm_cell_20_matmul_readvariableop_resource_0:	2È^
Ksequential_10_lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource_0:	2ÈY
Jsequential_10_lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource_0:	È(
$sequential_10_lstm_20_while_identity*
&sequential_10_lstm_20_while_identity_1*
&sequential_10_lstm_20_while_identity_2*
&sequential_10_lstm_20_while_identity_3*
&sequential_10_lstm_20_while_identity_4*
&sequential_10_lstm_20_while_identity_5C
?sequential_10_lstm_20_while_sequential_10_lstm_20_strided_slice
}sequential_10_lstm_20_while_tensorarrayv2read_tensorlistgetitem_sequential_10_lstm_20_tensorarrayunstack_tensorlistfromtensorZ
Gsequential_10_lstm_20_while_lstm_cell_20_matmul_readvariableop_resource:	2È\
Isequential_10_lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource:	2ÈW
Hsequential_10_lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource:	È¢?sequential_10/lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp¢>sequential_10/lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp¢@sequential_10/lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOpï
Msequential_10/lstm_20/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"   2   2O
Msequential_10/lstm_20/while/TensorArrayV2Read/TensorListGetItem/element_shapeÎ
?sequential_10/lstm_20/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemsequential_10_lstm_20_while_tensorarrayv2read_tensorlistgetitem_sequential_10_lstm_20_tensorarrayunstack_tensorlistfromtensor_0'sequential_10_lstm_20_while_placeholderVsequential_10/lstm_20/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02A
?sequential_10/lstm_20/while/TensorArrayV2Read/TensorListGetItem
>sequential_10/lstm_20/while/lstm_cell_20/MatMul/ReadVariableOpReadVariableOpIsequential_10_lstm_20_while_lstm_cell_20_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02@
>sequential_10/lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp¦
/sequential_10/lstm_20/while/lstm_cell_20/MatMulMatMulFsequential_10/lstm_20/while/TensorArrayV2Read/TensorListGetItem:item:0Fsequential_10/lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	È21
/sequential_10/lstm_20/while/lstm_cell_20/MatMul
@sequential_10/lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOpReadVariableOpKsequential_10_lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02B
@sequential_10/lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp
1sequential_10/lstm_20/while/lstm_cell_20/MatMul_1MatMul)sequential_10_lstm_20_while_placeholder_2Hsequential_10/lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	È23
1sequential_10/lstm_20/while/lstm_cell_20/MatMul_1
,sequential_10/lstm_20/while/lstm_cell_20/addAddV29sequential_10/lstm_20/while/lstm_cell_20/MatMul:product:0;sequential_10/lstm_20/while/lstm_cell_20/MatMul_1:product:0*
T0*
_output_shapes
:	È2.
,sequential_10/lstm_20/while/lstm_cell_20/add
?sequential_10/lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOpReadVariableOpJsequential_10_lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02A
?sequential_10/lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp
0sequential_10/lstm_20/while/lstm_cell_20/BiasAddBiasAdd0sequential_10/lstm_20/while/lstm_cell_20/add:z:0Gsequential_10/lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	È22
0sequential_10/lstm_20/while/lstm_cell_20/BiasAdd¶
8sequential_10/lstm_20/while/lstm_cell_20/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2:
8sequential_10/lstm_20/while/lstm_cell_20/split/split_dim¿
.sequential_10/lstm_20/while/lstm_cell_20/splitSplitAsequential_10/lstm_20/while/lstm_cell_20/split/split_dim:output:09sequential_10/lstm_20/while/lstm_cell_20/BiasAdd:output:0*
T0*<
_output_shapes*
(:2:2:2:2*
	num_split20
.sequential_10/lstm_20/while/lstm_cell_20/split¥
.sequential_10/lstm_20/while/lstm_cell_20/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>20
.sequential_10/lstm_20/while/lstm_cell_20/Const©
0sequential_10/lstm_20/while/lstm_cell_20/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?22
0sequential_10/lstm_20/while/lstm_cell_20/Const_1þ
,sequential_10/lstm_20/while/lstm_cell_20/MulMul7sequential_10/lstm_20/while/lstm_cell_20/split:output:07sequential_10/lstm_20/while/lstm_cell_20/Const:output:0*
T0*
_output_shapes

:22.
,sequential_10/lstm_20/while/lstm_cell_20/Mulÿ
.sequential_10/lstm_20/while/lstm_cell_20/Add_1AddV20sequential_10/lstm_20/while/lstm_cell_20/Mul:z:09sequential_10/lstm_20/while/lstm_cell_20/Const_1:output:0*
T0*
_output_shapes

:220
.sequential_10/lstm_20/while/lstm_cell_20/Add_1É
@sequential_10/lstm_20/while/lstm_cell_20/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2B
@sequential_10/lstm_20/while/lstm_cell_20/clip_by_value/Minimum/y³
>sequential_10/lstm_20/while/lstm_cell_20/clip_by_value/MinimumMinimum2sequential_10/lstm_20/while/lstm_cell_20/Add_1:z:0Isequential_10/lstm_20/while/lstm_cell_20/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:22@
>sequential_10/lstm_20/while/lstm_cell_20/clip_by_value/Minimum¹
8sequential_10/lstm_20/while/lstm_cell_20/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2:
8sequential_10/lstm_20/while/lstm_cell_20/clip_by_value/y«
6sequential_10/lstm_20/while/lstm_cell_20/clip_by_valueMaximumBsequential_10/lstm_20/while/lstm_cell_20/clip_by_value/Minimum:z:0Asequential_10/lstm_20/while/lstm_cell_20/clip_by_value/y:output:0*
T0*
_output_shapes

:228
6sequential_10/lstm_20/while/lstm_cell_20/clip_by_value©
0sequential_10/lstm_20/while/lstm_cell_20/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>22
0sequential_10/lstm_20/while/lstm_cell_20/Const_2©
0sequential_10/lstm_20/while/lstm_cell_20/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?22
0sequential_10/lstm_20/while/lstm_cell_20/Const_3
.sequential_10/lstm_20/while/lstm_cell_20/Mul_1Mul7sequential_10/lstm_20/while/lstm_cell_20/split:output:19sequential_10/lstm_20/while/lstm_cell_20/Const_2:output:0*
T0*
_output_shapes

:220
.sequential_10/lstm_20/while/lstm_cell_20/Mul_1
.sequential_10/lstm_20/while/lstm_cell_20/Add_2AddV22sequential_10/lstm_20/while/lstm_cell_20/Mul_1:z:09sequential_10/lstm_20/while/lstm_cell_20/Const_3:output:0*
T0*
_output_shapes

:220
.sequential_10/lstm_20/while/lstm_cell_20/Add_2Í
Bsequential_10/lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2D
Bsequential_10/lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum/y¹
@sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_1/MinimumMinimum2sequential_10/lstm_20/while/lstm_cell_20/Add_2:z:0Ksequential_10/lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:22B
@sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum½
:sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2<
:sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_1/y³
8sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_1MaximumDsequential_10/lstm_20/while/lstm_cell_20/clip_by_value_1/Minimum:z:0Csequential_10/lstm_20/while/lstm_cell_20/clip_by_value_1/y:output:0*
T0*
_output_shapes

:22:
8sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_1ù
.sequential_10/lstm_20/while/lstm_cell_20/mul_2Mul<sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_1:z:0)sequential_10_lstm_20_while_placeholder_3*
T0*
_output_shapes

:220
.sequential_10/lstm_20/while/lstm_cell_20/mul_2È
-sequential_10/lstm_20/while/lstm_cell_20/TanhTanh7sequential_10/lstm_20/while/lstm_cell_20/split:output:2*
T0*
_output_shapes

:22/
-sequential_10/lstm_20/while/lstm_cell_20/Tanhÿ
.sequential_10/lstm_20/while/lstm_cell_20/mul_3Mul:sequential_10/lstm_20/while/lstm_cell_20/clip_by_value:z:01sequential_10/lstm_20/while/lstm_cell_20/Tanh:y:0*
T0*
_output_shapes

:220
.sequential_10/lstm_20/while/lstm_cell_20/mul_3ú
.sequential_10/lstm_20/while/lstm_cell_20/add_3AddV22sequential_10/lstm_20/while/lstm_cell_20/mul_2:z:02sequential_10/lstm_20/while/lstm_cell_20/mul_3:z:0*
T0*
_output_shapes

:220
.sequential_10/lstm_20/while/lstm_cell_20/add_3©
0sequential_10/lstm_20/while/lstm_cell_20/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>22
0sequential_10/lstm_20/while/lstm_cell_20/Const_4©
0sequential_10/lstm_20/while/lstm_cell_20/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?22
0sequential_10/lstm_20/while/lstm_cell_20/Const_5
.sequential_10/lstm_20/while/lstm_cell_20/Mul_4Mul7sequential_10/lstm_20/while/lstm_cell_20/split:output:39sequential_10/lstm_20/while/lstm_cell_20/Const_4:output:0*
T0*
_output_shapes

:220
.sequential_10/lstm_20/while/lstm_cell_20/Mul_4
.sequential_10/lstm_20/while/lstm_cell_20/Add_4AddV22sequential_10/lstm_20/while/lstm_cell_20/Mul_4:z:09sequential_10/lstm_20/while/lstm_cell_20/Const_5:output:0*
T0*
_output_shapes

:220
.sequential_10/lstm_20/while/lstm_cell_20/Add_4Í
Bsequential_10/lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2D
Bsequential_10/lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum/y¹
@sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_2/MinimumMinimum2sequential_10/lstm_20/while/lstm_cell_20/Add_4:z:0Ksequential_10/lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:22B
@sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum½
:sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2<
:sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_2/y³
8sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_2MaximumDsequential_10/lstm_20/while/lstm_cell_20/clip_by_value_2/Minimum:z:0Csequential_10/lstm_20/while/lstm_cell_20/clip_by_value_2/y:output:0*
T0*
_output_shapes

:22:
8sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_2Ç
/sequential_10/lstm_20/while/lstm_cell_20/Tanh_1Tanh2sequential_10/lstm_20/while/lstm_cell_20/add_3:z:0*
T0*
_output_shapes

:221
/sequential_10/lstm_20/while/lstm_cell_20/Tanh_1
.sequential_10/lstm_20/while/lstm_cell_20/mul_5Mul<sequential_10/lstm_20/while/lstm_cell_20/clip_by_value_2:z:03sequential_10/lstm_20/while/lstm_cell_20/Tanh_1:y:0*
T0*
_output_shapes

:220
.sequential_10/lstm_20/while/lstm_cell_20/mul_5Î
@sequential_10/lstm_20/while/TensorArrayV2Write/TensorListSetItemTensorListSetItem)sequential_10_lstm_20_while_placeholder_1'sequential_10_lstm_20_while_placeholder2sequential_10/lstm_20/while/lstm_cell_20/mul_5:z:0*
_output_shapes
: *
element_dtype02B
@sequential_10/lstm_20/while/TensorArrayV2Write/TensorListSetItem
!sequential_10/lstm_20/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2#
!sequential_10/lstm_20/while/add/yÁ
sequential_10/lstm_20/while/addAddV2'sequential_10_lstm_20_while_placeholder*sequential_10/lstm_20/while/add/y:output:0*
T0*
_output_shapes
: 2!
sequential_10/lstm_20/while/add
#sequential_10/lstm_20/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2%
#sequential_10/lstm_20/while/add_1/yä
!sequential_10/lstm_20/while/add_1AddV2Dsequential_10_lstm_20_while_sequential_10_lstm_20_while_loop_counter,sequential_10/lstm_20/while/add_1/y:output:0*
T0*
_output_shapes
: 2#
!sequential_10/lstm_20/while/add_1Ã
$sequential_10/lstm_20/while/IdentityIdentity%sequential_10/lstm_20/while/add_1:z:0!^sequential_10/lstm_20/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_10/lstm_20/while/Identityì
&sequential_10/lstm_20/while/Identity_1IdentityJsequential_10_lstm_20_while_sequential_10_lstm_20_while_maximum_iterations!^sequential_10/lstm_20/while/NoOp*
T0*
_output_shapes
: 2(
&sequential_10/lstm_20/while/Identity_1Å
&sequential_10/lstm_20/while/Identity_2Identity#sequential_10/lstm_20/while/add:z:0!^sequential_10/lstm_20/while/NoOp*
T0*
_output_shapes
: 2(
&sequential_10/lstm_20/while/Identity_2ò
&sequential_10/lstm_20/while/Identity_3IdentityPsequential_10/lstm_20/while/TensorArrayV2Write/TensorListSetItem:output_handle:0!^sequential_10/lstm_20/while/NoOp*
T0*
_output_shapes
: 2(
&sequential_10/lstm_20/while/Identity_3Ü
&sequential_10/lstm_20/while/Identity_4Identity2sequential_10/lstm_20/while/lstm_cell_20/mul_5:z:0!^sequential_10/lstm_20/while/NoOp*
T0*
_output_shapes

:22(
&sequential_10/lstm_20/while/Identity_4Ü
&sequential_10/lstm_20/while/Identity_5Identity2sequential_10/lstm_20/while/lstm_cell_20/add_3:z:0!^sequential_10/lstm_20/while/NoOp*
T0*
_output_shapes

:22(
&sequential_10/lstm_20/while/Identity_5Ì
 sequential_10/lstm_20/while/NoOpNoOp@^sequential_10/lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp?^sequential_10/lstm_20/while/lstm_cell_20/MatMul/ReadVariableOpA^sequential_10/lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2"
 sequential_10/lstm_20/while/NoOp"U
$sequential_10_lstm_20_while_identity-sequential_10/lstm_20/while/Identity:output:0"Y
&sequential_10_lstm_20_while_identity_1/sequential_10/lstm_20/while/Identity_1:output:0"Y
&sequential_10_lstm_20_while_identity_2/sequential_10/lstm_20/while/Identity_2:output:0"Y
&sequential_10_lstm_20_while_identity_3/sequential_10/lstm_20/while/Identity_3:output:0"Y
&sequential_10_lstm_20_while_identity_4/sequential_10/lstm_20/while/Identity_4:output:0"Y
&sequential_10_lstm_20_while_identity_5/sequential_10/lstm_20/while/Identity_5:output:0"
Hsequential_10_lstm_20_while_lstm_cell_20_biasadd_readvariableop_resourceJsequential_10_lstm_20_while_lstm_cell_20_biasadd_readvariableop_resource_0"
Isequential_10_lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resourceKsequential_10_lstm_20_while_lstm_cell_20_matmul_1_readvariableop_resource_0"
Gsequential_10_lstm_20_while_lstm_cell_20_matmul_readvariableop_resourceIsequential_10_lstm_20_while_lstm_cell_20_matmul_readvariableop_resource_0"
?sequential_10_lstm_20_while_sequential_10_lstm_20_strided_sliceAsequential_10_lstm_20_while_sequential_10_lstm_20_strided_slice_0"
}sequential_10_lstm_20_while_tensorarrayv2read_tensorlistgetitem_sequential_10_lstm_20_tensorarrayunstack_tensorlistfromtensorsequential_10_lstm_20_while_tensorarrayv2read_tensorlistgetitem_sequential_10_lstm_20_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :2:2: : : : : 2
?sequential_10/lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp?sequential_10/lstm_20/while/lstm_cell_20/BiasAdd/ReadVariableOp2
>sequential_10/lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp>sequential_10/lstm_20/while/lstm_cell_20/MatMul/ReadVariableOp2
@sequential_10/lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp@sequential_10/lstm_20/while/lstm_cell_20/MatMul_1/ReadVariableOp: 
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
æ
õ
,__inference_lstm_cell_21_layer_call_fn_60851

inputs
states_0
states_1
unknown:	È
	unknown_0:	2È
	unknown_1:	È
identity

identity_1

identity_2¢StatefulPartitionedCall
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
GPU 2J 8 *P
fKRI
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_608382
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
­
Ð
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_56858

inputs 
dense_10_56848:2
dense_10_56850:
identity¢ dense_10/StatefulPartitionedCallD
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
strided_slice/stack_2â
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
valueB"ÿÿÿÿ2   2
Reshape/shapeo
ReshapeReshapeinputsReshape/shape:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22	
Reshape
 dense_10/StatefulPartitionedCallStatefulPartitionedCallReshape:output:0dense_10_56848dense_10_56850*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *L
fGRE
C__inference_dense_10_layer_call_and_return_conditional_losses_567992"
 dense_10/StatefulPartitionedCallq
Reshape_1/shape/0Const*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2
Reshape_1/shape/0h
Reshape_1/shape/2Const*
_output_shapes
: *
dtype0*
value	B :2
Reshape_1/shape/2¨
Reshape_1/shapePackReshape_1/shape/0:output:0strided_slice:output:0Reshape_1/shape/2:output:0*
N*
T0*
_output_shapes
:2
Reshape_1/shape¥
	Reshape_1Reshape)dense_10/StatefulPartitionedCall:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2

Identityq
NoOpNoOp!^dense_10/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2: : 2D
 dense_10/StatefulPartitionedCall dense_10/StatefulPartitionedCall:\ X
4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
¨
¼
while_cond_56465
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_56465___redundant_placeholder03
/while_while_cond_56465___redundant_placeholder13
/while_while_cond_56465___redundant_placeholder23
/while_while_cond_56465___redundant_placeholder3
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
¨
¼
while_cond_59821
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_59821___redundant_placeholder03
/while_while_cond_59821___redundant_placeholder13
/while_while_cond_59821___redundant_placeholder23
/while_while_cond_59821___redundant_placeholder3
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
:"¨L
saver_filename:0StatefulPartitionedCall_1:0StatefulPartitionedCall_28"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*¸
serving_default¤
B
lstm_21_input1
serving_default_lstm_21_input:0
B
time_distributed_10+
StatefulPartitionedCall:0
tensorflow/serving/predict:þº
Û
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
layer_with_weights-2
layer-2
	optimizer
trainable_variables
	variables
regularization_losses
	keras_api
	
signatures
*e&call_and_return_all_conditional_losses
f__call__
g_default_save_signature"
_tf_keras_sequential
Ã

cell

state_spec
trainable_variables
	variables
regularization_losses
	keras_api
*h&call_and_return_all_conditional_losses
i__call__"
_tf_keras_rnn_layer
Ã
cell

state_spec
trainable_variables
	variables
regularization_losses
	keras_api
*j&call_and_return_all_conditional_losses
k__call__"
_tf_keras_rnn_layer
°
	layer
trainable_variables
	variables
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
Ê
trainable_variables

#layers
	variables
$metrics
regularization_losses
%layer_metrics
&layer_regularization_losses
'non_trainable_variables
f__call__
g_default_save_signature
*e&call_and_return_all_conditional_losses
&e"call_and_return_conditional_losses"
_generic_user_object
,
nserving_default"
signature_map
á
(
state_size

kernel
recurrent_kernel
bias
)trainable_variables
*	variables
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
¹
trainable_variables

-layers
	variables
.metrics
regularization_losses
/layer_metrics
0layer_regularization_losses
1non_trainable_variables

2states
i__call__
*h&call_and_return_all_conditional_losses
&h"call_and_return_conditional_losses"
_generic_user_object
á
3
state_size

kernel
recurrent_kernel
 bias
4trainable_variables
5	variables
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
¹
trainable_variables

8layers
	variables
9metrics
regularization_losses
:layer_metrics
;layer_regularization_losses
<non_trainable_variables

=states
k__call__
*j&call_and_return_all_conditional_losses
&j"call_and_return_conditional_losses"
_generic_user_object
»

!kernel
"bias
>trainable_variables
?	variables
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
­
trainable_variables

Blayers
	variables
Cmetrics
regularization_losses
Dlayer_metrics
Elayer_regularization_losses
Fnon_trainable_variables
m__call__
*l&call_and_return_all_conditional_losses
&l"call_and_return_conditional_losses"
_generic_user_object
.:,	È2lstm_21/lstm_cell_21/kernel
8:6	2È2%lstm_21/lstm_cell_21/recurrent_kernel
(:&È2lstm_21/lstm_cell_21/bias
.:,	2È2lstm_20/lstm_cell_20/kernel
8:6	2È2%lstm_20/lstm_cell_20/recurrent_kernel
(:&È2lstm_20/lstm_cell_20/bias
,:*22time_distributed_10/kernel
&:$2time_distributed_10/bias
5
0
1
2"
trackable_list_wrapper
.
G0
H1"
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
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
­
)trainable_variables

Ilayers
*	variables
Jmetrics
+regularization_losses
Klayer_metrics
Llayer_regularization_losses
Mnon_trainable_variables
p__call__
*o&call_and_return_all_conditional_losses
&o"call_and_return_conditional_losses"
_generic_user_object
'

0"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
.
N0
O1"
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
­
4trainable_variables

Players
5	variables
Qmetrics
6regularization_losses
Rlayer_metrics
Slayer_regularization_losses
Tnon_trainable_variables
r__call__
*q&call_and_return_all_conditional_losses
&q"call_and_return_conditional_losses"
_generic_user_object
'
0"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
.
U0
V1"
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
­
>trainable_variables

Wlayers
?	variables
Xmetrics
@regularization_losses
Ylayer_metrics
Zlayer_regularization_losses
[non_trainable_variables
t__call__
*s&call_and_return_all_conditional_losses
&s"call_and_return_conditional_losses"
_generic_user_object
'
0"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
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
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
": 22lstm_21/Variable
": 22lstm_21/Variable
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
": 22lstm_20/Variable
": 22lstm_20/Variable
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
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
î2ë
H__inference_sequential_10_layer_call_and_return_conditional_losses_58377
H__inference_sequential_10_layer_call_and_return_conditional_losses_58741
H__inference_sequential_10_layer_call_and_return_conditional_losses_57949
H__inference_sequential_10_layer_call_and_return_conditional_losses_57982À
·²³
FullArgSpec1
args)&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults
p 

 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
2ÿ
-__inference_sequential_10_layer_call_fn_57348
-__inference_sequential_10_layer_call_fn_58770
-__inference_sequential_10_layer_call_fn_58799
-__inference_sequential_10_layer_call_fn_57916À
·²³
FullArgSpec1
args)&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults
p 

 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
ÑBÎ
 __inference__wrapped_model_55223lstm_21_input"
²
FullArgSpec
args 
varargsjargs
varkwjkwargs
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
ë2è
B__inference_lstm_21_layer_call_and_return_conditional_losses_58977
B__inference_lstm_21_layer_call_and_return_conditional_losses_59155
B__inference_lstm_21_layer_call_and_return_conditional_losses_59333
B__inference_lstm_21_layer_call_and_return_conditional_losses_59511Õ
Ì²È
FullArgSpecB
args:7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults

 
p 

 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
ÿ2ü
'__inference_lstm_21_layer_call_fn_59526
'__inference_lstm_21_layer_call_fn_59541
'__inference_lstm_21_layer_call_fn_59556
'__inference_lstm_21_layer_call_fn_59571Õ
Ì²È
FullArgSpecB
args:7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults

 
p 

 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
ë2è
B__inference_lstm_20_layer_call_and_return_conditional_losses_59749
B__inference_lstm_20_layer_call_and_return_conditional_losses_59927
B__inference_lstm_20_layer_call_and_return_conditional_losses_60105
B__inference_lstm_20_layer_call_and_return_conditional_losses_60283Õ
Ì²È
FullArgSpecB
args:7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults

 
p 

 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
ÿ2ü
'__inference_lstm_20_layer_call_fn_60298
'__inference_lstm_20_layer_call_fn_60313
'__inference_lstm_20_layer_call_fn_60328
'__inference_lstm_20_layer_call_fn_60343Õ
Ì²È
FullArgSpecB
args:7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults

 
p 

 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
2
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_60364
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_60385
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_60399
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_60413À
·²³
FullArgSpec1
args)&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults
p 

 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
2
3__inference_time_distributed_10_layer_call_fn_60422
3__inference_time_distributed_10_layer_call_fn_60431
3__inference_time_distributed_10_layer_call_fn_60440
3__inference_time_distributed_10_layer_call_fn_60449À
·²³
FullArgSpec1
args)&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults
p 

 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
ÐBÍ
#__inference_signature_wrapper_58013lstm_21_input"
²
FullArgSpec
args 
varargs
 
varkwjkwargs
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
è2å
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60506
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60559
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60612
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60669¾
µ²±
FullArgSpec3
args+(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults
p 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
ü2ù
,__inference_lstm_cell_21_layer_call_fn_60686
,__inference_lstm_cell_21_layer_call_fn_60703
,__inference_lstm_cell_21_layer_call_fn_60777
,__inference_lstm_cell_21_layer_call_fn_60851¾
µ²±
FullArgSpec3
args+(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults
p 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
è2å
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_60908
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_60961
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_61014
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_61071¾
µ²±
FullArgSpec3
args+(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults
p 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
ü2ù
,__inference_lstm_cell_20_layer_call_fn_61088
,__inference_lstm_cell_20_layer_call_fn_61105
,__inference_lstm_cell_20_layer_call_fn_61179
,__inference_lstm_cell_20_layer_call_fn_61253¾
µ²±
FullArgSpec3
args+(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults
p 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
í2ê
C__inference_dense_10_layer_call_and_return_conditional_losses_61263¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
Ò2Ï
(__inference_dense_10_layer_call_fn_61272¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 ¬
 __inference__wrapped_model_55223NOU V!"1¢.
'¢$
"
lstm_21_input

ª "DªA
?
time_distributed_10(%
time_distributed_10
£
C__inference_dense_10_layer_call_and_return_conditional_losses_61263\!"/¢,
%¢"
 
inputsÿÿÿÿÿÿÿÿÿ2
ª "%¢"

0ÿÿÿÿÿÿÿÿÿ
 {
(__inference_dense_10_layer_call_fn_61272O!"/¢,
%¢"
 
inputsÿÿÿÿÿÿÿÿÿ2
ª "ÿÿÿÿÿÿÿÿÿÀ
B__inference_lstm_20_layer_call_and_return_conditional_losses_59749zU VF¢C
<¢9
+(
&#
inputs/0ÿÿÿÿÿÿÿÿÿ2

 
p 

 
ª ")¢&

0ÿÿÿÿÿÿÿÿÿ2
 À
B__inference_lstm_20_layer_call_and_return_conditional_losses_59927zU VF¢C
<¢9
+(
&#
inputs/0ÿÿÿÿÿÿÿÿÿ2

 
p

 
ª ")¢&

0ÿÿÿÿÿÿÿÿÿ2
 §
B__inference_lstm_20_layer_call_and_return_conditional_losses_60105aU V6¢3
,¢)

inputs
2

 
p 

 
ª " ¢

0
2
 §
B__inference_lstm_20_layer_call_and_return_conditional_losses_60283aU V6¢3
,¢)

inputs
2

 
p

 
ª " ¢

0
2
 
'__inference_lstm_20_layer_call_fn_60298mUV F¢C
<¢9
+(
&#
inputs/0ÿÿÿÿÿÿÿÿÿ2

 
p 

 
ª "ÿÿÿÿÿÿÿÿÿ2
'__inference_lstm_20_layer_call_fn_60313mUV F¢C
<¢9
+(
&#
inputs/0ÿÿÿÿÿÿÿÿÿ2

 
p

 
ª "ÿÿÿÿÿÿÿÿÿ2
'__inference_lstm_20_layer_call_fn_60328TU V6¢3
,¢)

inputs
2

 
p 

 
ª "
2
'__inference_lstm_20_layer_call_fn_60343TU V6¢3
,¢)

inputs
2

 
p

 
ª "
2À
B__inference_lstm_21_layer_call_and_return_conditional_losses_58977zNOF¢C
<¢9
+(
&#
inputs/0ÿÿÿÿÿÿÿÿÿ

 
p 

 
ª ")¢&

0ÿÿÿÿÿÿÿÿÿ2
 À
B__inference_lstm_21_layer_call_and_return_conditional_losses_59155zNOF¢C
<¢9
+(
&#
inputs/0ÿÿÿÿÿÿÿÿÿ

 
p

 
ª ")¢&

0ÿÿÿÿÿÿÿÿÿ2
 §
B__inference_lstm_21_layer_call_and_return_conditional_losses_59333aNO6¢3
,¢)

inputs


 
p 

 
ª " ¢

0
2
 §
B__inference_lstm_21_layer_call_and_return_conditional_losses_59511aNO6¢3
,¢)

inputs


 
p

 
ª " ¢

0
2
 
'__inference_lstm_21_layer_call_fn_59526mNOF¢C
<¢9
+(
&#
inputs/0ÿÿÿÿÿÿÿÿÿ

 
p 

 
ª "ÿÿÿÿÿÿÿÿÿ2
'__inference_lstm_21_layer_call_fn_59541mNOF¢C
<¢9
+(
&#
inputs/0ÿÿÿÿÿÿÿÿÿ

 
p

 
ª "ÿÿÿÿÿÿÿÿÿ2
'__inference_lstm_21_layer_call_fn_59556TNO6¢3
,¢)

inputs


 
p 

 
ª "
2
'__inference_lstm_21_layer_call_fn_59571TNO6¢3
,¢)

inputs


 
p

 
ª "
2¾
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_60908ò ¢¢
¢

inputs2
s¢p
63	!¢
ú2


jstates/0VariableSpec
63	!¢
ú2


jstates/1VariableSpec
p 
ª "F¢C
<¢9

0/0
'$

0/1/0

0/1/1
 
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_60961Æ e¢b
[¢X

inputs2
9¢6

states/02

states/12
p 
ª "X¢U
N¢K

0/02
30

0/1/02

0/1/12
 
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_61014Æ e¢b
[¢X

inputs2
9¢6

states/02

states/12
p
ª "X¢U
N¢K

0/02
30

0/1/02

0/1/12
 ¾
G__inference_lstm_cell_20_layer_call_and_return_conditional_losses_61071ò ¢¢
¢

inputs2
s¢p
63	!¢
ú2


jstates/0VariableSpec
63	!¢
ú2


jstates/1VariableSpec
p
ª "F¢C
<¢9

0/0
'$

0/1/0

0/1/1
 ç
,__inference_lstm_cell_20_layer_call_fn_61088¶ e¢b
[¢X

inputs2
9¢6

states/02

states/12
p 
ª "H¢E

02
/,

1/02

1/12ç
,__inference_lstm_cell_20_layer_call_fn_61105¶ e¢b
[¢X

inputs2
9¢6

states/02

states/12
p
ª "H¢E

02
/,

1/02

1/12
,__inference_lstm_cell_20_layer_call_fn_61179â ¢¢
¢

inputs2
s¢p
63	!¢
ú2


jstates/0VariableSpec
63	!¢
ú2


jstates/1VariableSpec
p 
ª "6¢3
	
0
# 

1/0

1/1
,__inference_lstm_cell_20_layer_call_fn_61253â ¢¢
¢

inputs2
s¢p
63	!¢
ú2


jstates/0VariableSpec
63	!¢
ú2


jstates/1VariableSpec
p
ª "6¢3
	
0
# 

1/0

1/1¾
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60506ò¢¢
¢

inputs
s¢p
63	!¢
ú2


jstates/0VariableSpec
63	!¢
ú2


jstates/1VariableSpec
p 
ª "F¢C
<¢9

0/0
'$

0/1/0

0/1/1
 
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60559Æe¢b
[¢X

inputs
9¢6

states/02

states/12
p 
ª "X¢U
N¢K

0/02
30

0/1/02

0/1/12
 
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60612Æe¢b
[¢X

inputs
9¢6

states/02

states/12
p
ª "X¢U
N¢K

0/02
30

0/1/02

0/1/12
 ¾
G__inference_lstm_cell_21_layer_call_and_return_conditional_losses_60669ò¢¢
¢

inputs
s¢p
63	!¢
ú2


jstates/0VariableSpec
63	!¢
ú2


jstates/1VariableSpec
p
ª "F¢C
<¢9

0/0
'$

0/1/0

0/1/1
 ç
,__inference_lstm_cell_21_layer_call_fn_60686¶e¢b
[¢X

inputs
9¢6

states/02

states/12
p 
ª "H¢E

02
/,

1/02

1/12ç
,__inference_lstm_cell_21_layer_call_fn_60703¶e¢b
[¢X

inputs
9¢6

states/02

states/12
p
ª "H¢E

02
/,

1/02

1/12
,__inference_lstm_cell_21_layer_call_fn_60777â¢¢
¢

inputs
s¢p
63	!¢
ú2


jstates/0VariableSpec
63	!¢
ú2


jstates/1VariableSpec
p 
ª "6¢3
	
0
# 

1/0

1/1
,__inference_lstm_cell_21_layer_call_fn_60851â¢¢
¢

inputs
s¢p
63	!¢
ú2


jstates/0VariableSpec
63	!¢
ú2


jstates/1VariableSpec
p
ª "6¢3
	
0
# 

1/0

1/1·
H__inference_sequential_10_layer_call_and_return_conditional_losses_57949kNOU V!"9¢6
/¢,
"
lstm_21_input

p 

 
ª " ¢

0

 ·
H__inference_sequential_10_layer_call_and_return_conditional_losses_57982kNOU V!"9¢6
/¢,
"
lstm_21_input

p

 
ª " ¢

0

 °
H__inference_sequential_10_layer_call_and_return_conditional_losses_58377dNOU V!"2¢/
(¢%

inputs

p 

 
ª " ¢

0

 °
H__inference_sequential_10_layer_call_and_return_conditional_losses_58741dNOU V!"2¢/
(¢%

inputs

p

 
ª " ¢

0

 
-__inference_sequential_10_layer_call_fn_57348^NOU V!"9¢6
/¢,
"
lstm_21_input

p 

 
ª "

-__inference_sequential_10_layer_call_fn_57916^NOU V!"9¢6
/¢,
"
lstm_21_input

p

 
ª "

-__inference_sequential_10_layer_call_fn_58770WNOU V!"2¢/
(¢%

inputs

p 

 
ª "

-__inference_sequential_10_layer_call_fn_58799WNOU V!"2¢/
(¢%

inputs

p

 
ª "
À
#__inference_signature_wrapper_58013NOU V!"B¢?
¢ 
8ª5
3
lstm_21_input"
lstm_21_input
"DªA
?
time_distributed_10(%
time_distributed_10
Ð
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_60364~!"D¢A
:¢7
-*
inputsÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
p 

 
ª "2¢/
(%
0ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ
 Ð
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_60385~!"D¢A
:¢7
-*
inputsÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
p

 
ª "2¢/
(%
0ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ
 ¬
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_60399Z!"2¢/
(¢%

inputs
2
p 

 
ª " ¢

0

 ¬
N__inference_time_distributed_10_layer_call_and_return_conditional_losses_60413Z!"2¢/
(¢%

inputs
2
p

 
ª " ¢

0

 ¨
3__inference_time_distributed_10_layer_call_fn_60422q!"D¢A
:¢7
-*
inputsÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
p 

 
ª "%"ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ¨
3__inference_time_distributed_10_layer_call_fn_60431q!"D¢A
:¢7
-*
inputsÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
p

 
ª "%"ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ
3__inference_time_distributed_10_layer_call_fn_60440M!"2¢/
(¢%

inputs
2
p 

 
ª "

3__inference_time_distributed_10_layer_call_fn_60449M!"2¢/
(¢%

inputs
2
p

 
ª "
