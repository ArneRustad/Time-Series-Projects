¥4
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
"serve*2.6.02v2.6.0-rc2-32-g919f693420e8£§2
f
	Adam/iterVarHandleOp*
_output_shapes
: *
dtype0	*
shape: *
shared_name	Adam/iter
_
Adam/iter/Read/ReadVariableOpReadVariableOp	Adam/iter*
_output_shapes
: *
dtype0	
j
Adam/beta_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameAdam/beta_1
c
Adam/beta_1/Read/ReadVariableOpReadVariableOpAdam/beta_1*
_output_shapes
: *
dtype0
j
Adam/beta_2VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameAdam/beta_2
c
Adam/beta_2/Read/ReadVariableOpReadVariableOpAdam/beta_2*
_output_shapes
: *
dtype0
h

Adam/decayVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_name
Adam/decay
a
Adam/decay/Read/ReadVariableOpReadVariableOp
Adam/decay*
_output_shapes
: *
dtype0
x
Adam/learning_rateVarHandleOp*
_output_shapes
: *
dtype0*
shape: *#
shared_nameAdam/learning_rate
q
&Adam/learning_rate/Read/ReadVariableOpReadVariableOpAdam/learning_rate*
_output_shapes
: *
dtype0

lstm_17/lstm_cell_17/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	È*,
shared_namelstm_17/lstm_cell_17/kernel

/lstm_17/lstm_cell_17/kernel/Read/ReadVariableOpReadVariableOplstm_17/lstm_cell_17/kernel*
_output_shapes
:	È*
dtype0
§
%lstm_17/lstm_cell_17/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2È*6
shared_name'%lstm_17/lstm_cell_17/recurrent_kernel
 
9lstm_17/lstm_cell_17/recurrent_kernel/Read/ReadVariableOpReadVariableOp%lstm_17/lstm_cell_17/recurrent_kernel*
_output_shapes
:	2È*
dtype0

lstm_17/lstm_cell_17/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:È**
shared_namelstm_17/lstm_cell_17/bias

-lstm_17/lstm_cell_17/bias/Read/ReadVariableOpReadVariableOplstm_17/lstm_cell_17/bias*
_output_shapes	
:È*
dtype0

lstm_16/lstm_cell_16/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2È*,
shared_namelstm_16/lstm_cell_16/kernel

/lstm_16/lstm_cell_16/kernel/Read/ReadVariableOpReadVariableOplstm_16/lstm_cell_16/kernel*
_output_shapes
:	2È*
dtype0
§
%lstm_16/lstm_cell_16/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2È*6
shared_name'%lstm_16/lstm_cell_16/recurrent_kernel
 
9lstm_16/lstm_cell_16/recurrent_kernel/Read/ReadVariableOpReadVariableOp%lstm_16/lstm_cell_16/recurrent_kernel*
_output_shapes
:	2È*
dtype0

lstm_16/lstm_cell_16/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:È**
shared_namelstm_16/lstm_cell_16/bias

-lstm_16/lstm_cell_16/bias/Read/ReadVariableOpReadVariableOplstm_16/lstm_cell_16/bias*
_output_shapes	
:È*
dtype0

time_distributed_8/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:2**
shared_nametime_distributed_8/kernel

-time_distributed_8/kernel/Read/ReadVariableOpReadVariableOptime_distributed_8/kernel*
_output_shapes

:2*
dtype0

time_distributed_8/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*(
shared_nametime_distributed_8/bias

+time_distributed_8/bias/Read/ReadVariableOpReadVariableOptime_distributed_8/bias*
_output_shapes
:*
dtype0
|
lstm_17/VariableVarHandleOp*
_output_shapes
: *
dtype0*
shape
:22*!
shared_namelstm_17/Variable
u
$lstm_17/Variable/Read/ReadVariableOpReadVariableOplstm_17/Variable*
_output_shapes

:22*
dtype0

lstm_17/Variable_1VarHandleOp*
_output_shapes
: *
dtype0*
shape
:22*#
shared_namelstm_17/Variable_1
y
&lstm_17/Variable_1/Read/ReadVariableOpReadVariableOplstm_17/Variable_1*
_output_shapes

:22*
dtype0
|
lstm_16/VariableVarHandleOp*
_output_shapes
: *
dtype0*
shape
:22*!
shared_namelstm_16/Variable
u
$lstm_16/Variable/Read/ReadVariableOpReadVariableOplstm_16/Variable*
_output_shapes

:22*
dtype0

lstm_16/Variable_1VarHandleOp*
_output_shapes
: *
dtype0*
shape
:22*#
shared_namelstm_16/Variable_1
y
&lstm_16/Variable_1/Read/ReadVariableOpReadVariableOplstm_16/Variable_1*
_output_shapes

:22*
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
¡
"Adam/lstm_17/lstm_cell_17/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	È*3
shared_name$"Adam/lstm_17/lstm_cell_17/kernel/m

6Adam/lstm_17/lstm_cell_17/kernel/m/Read/ReadVariableOpReadVariableOp"Adam/lstm_17/lstm_cell_17/kernel/m*
_output_shapes
:	È*
dtype0
µ
,Adam/lstm_17/lstm_cell_17/recurrent_kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2È*=
shared_name.,Adam/lstm_17/lstm_cell_17/recurrent_kernel/m
®
@Adam/lstm_17/lstm_cell_17/recurrent_kernel/m/Read/ReadVariableOpReadVariableOp,Adam/lstm_17/lstm_cell_17/recurrent_kernel/m*
_output_shapes
:	2È*
dtype0

 Adam/lstm_17/lstm_cell_17/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:È*1
shared_name" Adam/lstm_17/lstm_cell_17/bias/m

4Adam/lstm_17/lstm_cell_17/bias/m/Read/ReadVariableOpReadVariableOp Adam/lstm_17/lstm_cell_17/bias/m*
_output_shapes	
:È*
dtype0
¡
"Adam/lstm_16/lstm_cell_16/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2È*3
shared_name$"Adam/lstm_16/lstm_cell_16/kernel/m

6Adam/lstm_16/lstm_cell_16/kernel/m/Read/ReadVariableOpReadVariableOp"Adam/lstm_16/lstm_cell_16/kernel/m*
_output_shapes
:	2È*
dtype0
µ
,Adam/lstm_16/lstm_cell_16/recurrent_kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2È*=
shared_name.,Adam/lstm_16/lstm_cell_16/recurrent_kernel/m
®
@Adam/lstm_16/lstm_cell_16/recurrent_kernel/m/Read/ReadVariableOpReadVariableOp,Adam/lstm_16/lstm_cell_16/recurrent_kernel/m*
_output_shapes
:	2È*
dtype0

 Adam/lstm_16/lstm_cell_16/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:È*1
shared_name" Adam/lstm_16/lstm_cell_16/bias/m

4Adam/lstm_16/lstm_cell_16/bias/m/Read/ReadVariableOpReadVariableOp Adam/lstm_16/lstm_cell_16/bias/m*
_output_shapes	
:È*
dtype0

 Adam/time_distributed_8/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:2*1
shared_name" Adam/time_distributed_8/kernel/m

4Adam/time_distributed_8/kernel/m/Read/ReadVariableOpReadVariableOp Adam/time_distributed_8/kernel/m*
_output_shapes

:2*
dtype0

Adam/time_distributed_8/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*/
shared_name Adam/time_distributed_8/bias/m

2Adam/time_distributed_8/bias/m/Read/ReadVariableOpReadVariableOpAdam/time_distributed_8/bias/m*
_output_shapes
:*
dtype0
¡
"Adam/lstm_17/lstm_cell_17/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	È*3
shared_name$"Adam/lstm_17/lstm_cell_17/kernel/v

6Adam/lstm_17/lstm_cell_17/kernel/v/Read/ReadVariableOpReadVariableOp"Adam/lstm_17/lstm_cell_17/kernel/v*
_output_shapes
:	È*
dtype0
µ
,Adam/lstm_17/lstm_cell_17/recurrent_kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2È*=
shared_name.,Adam/lstm_17/lstm_cell_17/recurrent_kernel/v
®
@Adam/lstm_17/lstm_cell_17/recurrent_kernel/v/Read/ReadVariableOpReadVariableOp,Adam/lstm_17/lstm_cell_17/recurrent_kernel/v*
_output_shapes
:	2È*
dtype0

 Adam/lstm_17/lstm_cell_17/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:È*1
shared_name" Adam/lstm_17/lstm_cell_17/bias/v

4Adam/lstm_17/lstm_cell_17/bias/v/Read/ReadVariableOpReadVariableOp Adam/lstm_17/lstm_cell_17/bias/v*
_output_shapes	
:È*
dtype0
¡
"Adam/lstm_16/lstm_cell_16/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2È*3
shared_name$"Adam/lstm_16/lstm_cell_16/kernel/v

6Adam/lstm_16/lstm_cell_16/kernel/v/Read/ReadVariableOpReadVariableOp"Adam/lstm_16/lstm_cell_16/kernel/v*
_output_shapes
:	2È*
dtype0
µ
,Adam/lstm_16/lstm_cell_16/recurrent_kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2È*=
shared_name.,Adam/lstm_16/lstm_cell_16/recurrent_kernel/v
®
@Adam/lstm_16/lstm_cell_16/recurrent_kernel/v/Read/ReadVariableOpReadVariableOp,Adam/lstm_16/lstm_cell_16/recurrent_kernel/v*
_output_shapes
:	2È*
dtype0

 Adam/lstm_16/lstm_cell_16/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:È*1
shared_name" Adam/lstm_16/lstm_cell_16/bias/v

4Adam/lstm_16/lstm_cell_16/bias/v/Read/ReadVariableOpReadVariableOp Adam/lstm_16/lstm_cell_16/bias/v*
_output_shapes	
:È*
dtype0

 Adam/time_distributed_8/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:2*1
shared_name" Adam/time_distributed_8/kernel/v

4Adam/time_distributed_8/kernel/v/Read/ReadVariableOpReadVariableOp Adam/time_distributed_8/kernel/v*
_output_shapes

:2*
dtype0

Adam/time_distributed_8/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*/
shared_name Adam/time_distributed_8/bias/v

2Adam/time_distributed_8/bias/v/Read/ReadVariableOpReadVariableOpAdam/time_distributed_8/bias/v*
_output_shapes
:*
dtype0

NoOpNoOp
Ü9
ConstConst"/device:CPU:0*
_output_shapes
: *
dtype0*9
value9B9 B9
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
Ð
iter

beta_1

beta_2
	decay
learning_rate mj!mk"ml#mm$mn%mo&mp'mq vr!vs"vt#vu$vv%vw&vx'vy
8
 0
!1
"2
#3
$4
%5
&6
'7
8
 0
!1
"2
#3
$4
%5
&6
'7
 
­
trainable_variables

(layers
	variables
)metrics
regularization_losses
*layer_metrics
+layer_regularization_losses
,non_trainable_variables
 

-
state_size

 kernel
!recurrent_kernel
"bias
.trainable_variables
/	variables
0regularization_losses
1	keras_api
 

 0
!1
"2

 0
!1
"2
 
¹
trainable_variables

2layers
	variables
3metrics
regularization_losses
4layer_metrics
5layer_regularization_losses
6non_trainable_variables

7states

8
state_size

#kernel
$recurrent_kernel
%bias
9trainable_variables
:	variables
;regularization_losses
<	keras_api
 

#0
$1
%2

#0
$1
%2
 
¹
trainable_variables

=layers
	variables
>metrics
regularization_losses
?layer_metrics
@layer_regularization_losses
Anon_trainable_variables

Bstates
h

&kernel
'bias
Ctrainable_variables
D	variables
Eregularization_losses
F	keras_api

&0
'1

&0
'1
 
­
trainable_variables

Glayers
	variables
Hmetrics
regularization_losses
Ilayer_metrics
Jlayer_regularization_losses
Knon_trainable_variables
HF
VARIABLE_VALUE	Adam/iter)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUE
LJ
VARIABLE_VALUEAdam/beta_1+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUE
LJ
VARIABLE_VALUEAdam/beta_2+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUE
JH
VARIABLE_VALUE
Adam/decay*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUE
ZX
VARIABLE_VALUEAdam/learning_rate2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUE
a_
VARIABLE_VALUElstm_17/lstm_cell_17/kernel0trainable_variables/0/.ATTRIBUTES/VARIABLE_VALUE
ki
VARIABLE_VALUE%lstm_17/lstm_cell_17/recurrent_kernel0trainable_variables/1/.ATTRIBUTES/VARIABLE_VALUE
_]
VARIABLE_VALUElstm_17/lstm_cell_17/bias0trainable_variables/2/.ATTRIBUTES/VARIABLE_VALUE
a_
VARIABLE_VALUElstm_16/lstm_cell_16/kernel0trainable_variables/3/.ATTRIBUTES/VARIABLE_VALUE
ki
VARIABLE_VALUE%lstm_16/lstm_cell_16/recurrent_kernel0trainable_variables/4/.ATTRIBUTES/VARIABLE_VALUE
_]
VARIABLE_VALUElstm_16/lstm_cell_16/bias0trainable_variables/5/.ATTRIBUTES/VARIABLE_VALUE
_]
VARIABLE_VALUEtime_distributed_8/kernel0trainable_variables/6/.ATTRIBUTES/VARIABLE_VALUE
][
VARIABLE_VALUEtime_distributed_8/bias0trainable_variables/7/.ATTRIBUTES/VARIABLE_VALUE

0
1
2

L0
M1
 
 
 
 

 0
!1
"2

 0
!1
"2
 
­
.trainable_variables

Nlayers
/	variables
Ometrics
0regularization_losses
Player_metrics
Qlayer_regularization_losses
Rnon_trainable_variables


0
 
 
 
 

S0
T1
 

#0
$1
%2

#0
$1
%2
 
­
9trainable_variables

Ulayers
:	variables
Vmetrics
;regularization_losses
Wlayer_metrics
Xlayer_regularization_losses
Ynon_trainable_variables

0
 
 
 
 

Z0
[1

&0
'1

&0
'1
 
­
Ctrainable_variables

\layers
D	variables
]metrics
Eregularization_losses
^layer_metrics
_layer_regularization_losses
`non_trainable_variables

0
 
 
 
 
4
	atotal
	bcount
c	variables
d	keras_api
D
	etotal
	fcount
g
_fn_kwargs
h	variables
i	keras_api
 
 
 
 
 
hf
VARIABLE_VALUElstm_17/VariableBlayer_with_weights-0/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUE
jh
VARIABLE_VALUElstm_17/Variable_1Blayer_with_weights-0/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUE
 
 
 
 
 
hf
VARIABLE_VALUElstm_16/VariableBlayer_with_weights-1/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUE
jh
VARIABLE_VALUElstm_16/Variable_1Blayer_with_weights-1/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUE
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
a0
b1

c	variables
QO
VARIABLE_VALUEtotal_14keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUE
QO
VARIABLE_VALUEcount_14keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUE
 

e0
f1

h	variables

VARIABLE_VALUE"Adam/lstm_17/lstm_cell_17/kernel/mLtrainable_variables/0/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE,Adam/lstm_17/lstm_cell_17/recurrent_kernel/mLtrainable_variables/1/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE Adam/lstm_17/lstm_cell_17/bias/mLtrainable_variables/2/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE"Adam/lstm_16/lstm_cell_16/kernel/mLtrainable_variables/3/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE,Adam/lstm_16/lstm_cell_16/recurrent_kernel/mLtrainable_variables/4/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE Adam/lstm_16/lstm_cell_16/bias/mLtrainable_variables/5/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE Adam/time_distributed_8/kernel/mLtrainable_variables/6/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
~
VARIABLE_VALUEAdam/time_distributed_8/bias/mLtrainable_variables/7/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE"Adam/lstm_17/lstm_cell_17/kernel/vLtrainable_variables/0/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE,Adam/lstm_17/lstm_cell_17/recurrent_kernel/vLtrainable_variables/1/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE Adam/lstm_17/lstm_cell_17/bias/vLtrainable_variables/2/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE"Adam/lstm_16/lstm_cell_16/kernel/vLtrainable_variables/3/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE,Adam/lstm_16/lstm_cell_16/recurrent_kernel/vLtrainable_variables/4/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE Adam/lstm_16/lstm_cell_16/bias/vLtrainable_variables/5/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE

VARIABLE_VALUE Adam/time_distributed_8/kernel/vLtrainable_variables/6/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
~
VARIABLE_VALUEAdam/time_distributed_8/bias/vLtrainable_variables/7/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
v
serving_default_lstm_17_inputPlaceholder*"
_output_shapes
:2
*
dtype0*
shape:2


StatefulPartitionedCallStatefulPartitionedCallserving_default_lstm_17_inputlstm_17/lstm_cell_17/kernellstm_17/Variable%lstm_17/lstm_cell_17/recurrent_kernellstm_17/lstm_cell_17/biaslstm_17/Variable_1lstm_16/lstm_cell_16/kernellstm_16/Variable%lstm_16/lstm_cell_16/recurrent_kernellstm_16/lstm_cell_16/biaslstm_16/Variable_1time_distributed_8/kerneltime_distributed_8/bias*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8 *,
f'R%
#__inference_signature_wrapper_51165
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 

StatefulPartitionedCall_1StatefulPartitionedCallsaver_filenameAdam/iter/Read/ReadVariableOpAdam/beta_1/Read/ReadVariableOpAdam/beta_2/Read/ReadVariableOpAdam/decay/Read/ReadVariableOp&Adam/learning_rate/Read/ReadVariableOp/lstm_17/lstm_cell_17/kernel/Read/ReadVariableOp9lstm_17/lstm_cell_17/recurrent_kernel/Read/ReadVariableOp-lstm_17/lstm_cell_17/bias/Read/ReadVariableOp/lstm_16/lstm_cell_16/kernel/Read/ReadVariableOp9lstm_16/lstm_cell_16/recurrent_kernel/Read/ReadVariableOp-lstm_16/lstm_cell_16/bias/Read/ReadVariableOp-time_distributed_8/kernel/Read/ReadVariableOp+time_distributed_8/bias/Read/ReadVariableOp$lstm_17/Variable/Read/ReadVariableOp&lstm_17/Variable_1/Read/ReadVariableOp$lstm_16/Variable/Read/ReadVariableOp&lstm_16/Variable_1/Read/ReadVariableOptotal/Read/ReadVariableOpcount/Read/ReadVariableOptotal_1/Read/ReadVariableOpcount_1/Read/ReadVariableOp6Adam/lstm_17/lstm_cell_17/kernel/m/Read/ReadVariableOp@Adam/lstm_17/lstm_cell_17/recurrent_kernel/m/Read/ReadVariableOp4Adam/lstm_17/lstm_cell_17/bias/m/Read/ReadVariableOp6Adam/lstm_16/lstm_cell_16/kernel/m/Read/ReadVariableOp@Adam/lstm_16/lstm_cell_16/recurrent_kernel/m/Read/ReadVariableOp4Adam/lstm_16/lstm_cell_16/bias/m/Read/ReadVariableOp4Adam/time_distributed_8/kernel/m/Read/ReadVariableOp2Adam/time_distributed_8/bias/m/Read/ReadVariableOp6Adam/lstm_17/lstm_cell_17/kernel/v/Read/ReadVariableOp@Adam/lstm_17/lstm_cell_17/recurrent_kernel/v/Read/ReadVariableOp4Adam/lstm_17/lstm_cell_17/bias/v/Read/ReadVariableOp6Adam/lstm_16/lstm_cell_16/kernel/v/Read/ReadVariableOp@Adam/lstm_16/lstm_cell_16/recurrent_kernel/v/Read/ReadVariableOp4Adam/lstm_16/lstm_cell_16/bias/v/Read/ReadVariableOp4Adam/time_distributed_8/kernel/v/Read/ReadVariableOp2Adam/time_distributed_8/bias/v/Read/ReadVariableOpConst*2
Tin+
)2'	*
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
__inference__traced_save_54558
±

StatefulPartitionedCall_2StatefulPartitionedCallsaver_filename	Adam/iterAdam/beta_1Adam/beta_2
Adam/decayAdam/learning_ratelstm_17/lstm_cell_17/kernel%lstm_17/lstm_cell_17/recurrent_kernellstm_17/lstm_cell_17/biaslstm_16/lstm_cell_16/kernel%lstm_16/lstm_cell_16/recurrent_kernellstm_16/lstm_cell_16/biastime_distributed_8/kerneltime_distributed_8/biaslstm_17/Variablelstm_17/Variable_1lstm_16/Variablelstm_16/Variable_1totalcounttotal_1count_1"Adam/lstm_17/lstm_cell_17/kernel/m,Adam/lstm_17/lstm_cell_17/recurrent_kernel/m Adam/lstm_17/lstm_cell_17/bias/m"Adam/lstm_16/lstm_cell_16/kernel/m,Adam/lstm_16/lstm_cell_16/recurrent_kernel/m Adam/lstm_16/lstm_cell_16/bias/m Adam/time_distributed_8/kernel/mAdam/time_distributed_8/bias/m"Adam/lstm_17/lstm_cell_17/kernel/v,Adam/lstm_17/lstm_cell_17/recurrent_kernel/v Adam/lstm_17/lstm_cell_17/bias/v"Adam/lstm_16/lstm_cell_16/kernel/v,Adam/lstm_16/lstm_cell_16/recurrent_kernel/v Adam/lstm_16/lstm_cell_16/bias/v Adam/time_distributed_8/kernel/vAdam/time_distributed_8/bias/v*1
Tin*
(2&*
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
!__inference__traced_restore_54679°û0
¨
¼
while_cond_53329
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_53329___redundant_placeholder03
/while_while_cond_53329___redundant_placeholder13
/while_while_cond_53329___redundant_placeholder23
/while_while_cond_53329___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
²

M__inference_time_distributed_8_layer_call_and_return_conditional_losses_53516

inputs8
&dense_8_matmul_readvariableop_resource:25
'dense_8_biasadd_readvariableop_resource:
identity¢dense_8/BiasAdd/ReadVariableOp¢dense_8/MatMul/ReadVariableOpD
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
Reshape¥
dense_8/MatMul/ReadVariableOpReadVariableOp&dense_8_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_8/MatMul/ReadVariableOp
dense_8/MatMulMatMulReshape:output:0%dense_8/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
dense_8/MatMul¤
dense_8/BiasAdd/ReadVariableOpReadVariableOp'dense_8_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_8/BiasAdd/ReadVariableOp¡
dense_8/BiasAddBiasAdddense_8/MatMul:product:0&dense_8/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
dense_8/BiasAddq
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
Reshape_1/shape
	Reshape_1Reshapedense_8/BiasAdd:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2

Identity
NoOpNoOp^dense_8/BiasAdd/ReadVariableOp^dense_8/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2: : 2@
dense_8/BiasAdd/ReadVariableOpdense_8/BiasAdd/ReadVariableOp2>
dense_8/MatMul/ReadVariableOpdense_8/MatMul/ReadVariableOp:\ X
4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
 
Ì
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_49956

inputs
dense_8_49946:2
dense_8_49948:
identity¢dense_8/StatefulPartitionedCallD
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
Reshape
dense_8/StatefulPartitionedCallStatefulPartitionedCallReshape:output:0dense_8_49946dense_8_49948*
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
GPU 2J 8 *K
fFRD
B__inference_dense_8_layer_call_and_return_conditional_losses_499452!
dense_8/StatefulPartitionedCallq
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
Reshape_1/shape¤
	Reshape_1Reshape(dense_8/StatefulPartitionedCall:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2

Identityp
NoOpNoOp ^dense_8/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2: : 2B
dense_8/StatefulPartitionedCalldense_8/StatefulPartitionedCall:\ X
4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
i
Ë

lstm_17_while_body_51602,
(lstm_17_while_lstm_17_while_loop_counter2
.lstm_17_while_lstm_17_while_maximum_iterations
lstm_17_while_placeholder
lstm_17_while_placeholder_1
lstm_17_while_placeholder_2
lstm_17_while_placeholder_3)
%lstm_17_while_lstm_17_strided_slice_0g
clstm_17_while_tensorarrayv2read_tensorlistgetitem_lstm_17_tensorarrayunstack_tensorlistfromtensor_0N
;lstm_17_while_lstm_cell_17_matmul_readvariableop_resource_0:	ÈP
=lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource_0:	2ÈK
<lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource_0:	È
lstm_17_while_identity
lstm_17_while_identity_1
lstm_17_while_identity_2
lstm_17_while_identity_3
lstm_17_while_identity_4
lstm_17_while_identity_5'
#lstm_17_while_lstm_17_strided_slicee
alstm_17_while_tensorarrayv2read_tensorlistgetitem_lstm_17_tensorarrayunstack_tensorlistfromtensorL
9lstm_17_while_lstm_cell_17_matmul_readvariableop_resource:	ÈN
;lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource:	2ÈI
:lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource:	È¢1lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp¢0lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp¢2lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOpÓ
?lstm_17/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      2A
?lstm_17/while/TensorArrayV2Read/TensorListGetItem/element_shapeú
1lstm_17/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemclstm_17_while_tensorarrayv2read_tensorlistgetitem_lstm_17_tensorarrayunstack_tensorlistfromtensor_0lstm_17_while_placeholderHlstm_17/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype023
1lstm_17/while/TensorArrayV2Read/TensorListGetItemá
0lstm_17/while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp;lstm_17_while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype022
0lstm_17/while/lstm_cell_17/MatMul/ReadVariableOpî
!lstm_17/while/lstm_cell_17/MatMulMatMul8lstm_17/while/TensorArrayV2Read/TensorListGetItem:item:08lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2#
!lstm_17/while/lstm_cell_17/MatMulç
2lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp=lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype024
2lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp×
#lstm_17/while/lstm_cell_17/MatMul_1MatMullstm_17_while_placeholder_2:lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2%
#lstm_17/while/lstm_cell_17/MatMul_1Ï
lstm_17/while/lstm_cell_17/addAddV2+lstm_17/while/lstm_cell_17/MatMul:product:0-lstm_17/while/lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2 
lstm_17/while/lstm_cell_17/addà
1lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp<lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype023
1lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOpÜ
"lstm_17/while/lstm_cell_17/BiasAddBiasAdd"lstm_17/while/lstm_cell_17/add:z:09lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2$
"lstm_17/while/lstm_cell_17/BiasAdd
*lstm_17/while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2,
*lstm_17/while/lstm_cell_17/split/split_dim
 lstm_17/while/lstm_cell_17/splitSplit3lstm_17/while/lstm_cell_17/split/split_dim:output:0+lstm_17/while/lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2"
 lstm_17/while/lstm_cell_17/split
 lstm_17/while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2"
 lstm_17/while/lstm_cell_17/Const
"lstm_17/while/lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_17/while/lstm_cell_17/Const_1Æ
lstm_17/while/lstm_cell_17/MulMul)lstm_17/while/lstm_cell_17/split:output:0)lstm_17/while/lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222 
lstm_17/while/lstm_cell_17/MulÇ
 lstm_17/while/lstm_cell_17/Add_1AddV2"lstm_17/while/lstm_cell_17/Mul:z:0+lstm_17/while/lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/Add_1­
2lstm_17/while/lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?24
2lstm_17/while/lstm_cell_17/clip_by_value/Minimum/yû
0lstm_17/while/lstm_cell_17/clip_by_value/MinimumMinimum$lstm_17/while/lstm_cell_17/Add_1:z:0;lstm_17/while/lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:2222
0lstm_17/while/lstm_cell_17/clip_by_value/Minimum
*lstm_17/while/lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_17/while/lstm_cell_17/clip_by_value/yó
(lstm_17/while/lstm_cell_17/clip_by_valueMaximum4lstm_17/while/lstm_cell_17/clip_by_value/Minimum:z:03lstm_17/while/lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222*
(lstm_17/while/lstm_cell_17/clip_by_value
"lstm_17/while/lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_17/while/lstm_cell_17/Const_2
"lstm_17/while/lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_17/while/lstm_cell_17/Const_3Ì
 lstm_17/while/lstm_cell_17/Mul_1Mul)lstm_17/while/lstm_cell_17/split:output:1+lstm_17/while/lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/Mul_1É
 lstm_17/while/lstm_cell_17/Add_2AddV2$lstm_17/while/lstm_cell_17/Mul_1:z:0+lstm_17/while/lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/Add_2±
4lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum/y
2lstm_17/while/lstm_cell_17/clip_by_value_1/MinimumMinimum$lstm_17/while/lstm_cell_17/Add_2:z:0=lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:2224
2lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum¡
,lstm_17/while/lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_17/while/lstm_cell_17/clip_by_value_1/yû
*lstm_17/while/lstm_cell_17/clip_by_value_1Maximum6lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum:z:05lstm_17/while/lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222,
*lstm_17/while/lstm_cell_17/clip_by_value_1Á
 lstm_17/while/lstm_cell_17/mul_2Mul.lstm_17/while/lstm_cell_17/clip_by_value_1:z:0lstm_17_while_placeholder_3*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/mul_2
lstm_17/while/lstm_cell_17/TanhTanh)lstm_17/while/lstm_cell_17/split:output:2*
T0*
_output_shapes

:222!
lstm_17/while/lstm_cell_17/TanhÇ
 lstm_17/while/lstm_cell_17/mul_3Mul,lstm_17/while/lstm_cell_17/clip_by_value:z:0#lstm_17/while/lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/mul_3Â
 lstm_17/while/lstm_cell_17/add_3AddV2$lstm_17/while/lstm_cell_17/mul_2:z:0$lstm_17/while/lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/add_3
"lstm_17/while/lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_17/while/lstm_cell_17/Const_4
"lstm_17/while/lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_17/while/lstm_cell_17/Const_5Ì
 lstm_17/while/lstm_cell_17/Mul_4Mul)lstm_17/while/lstm_cell_17/split:output:3+lstm_17/while/lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/Mul_4É
 lstm_17/while/lstm_cell_17/Add_4AddV2$lstm_17/while/lstm_cell_17/Mul_4:z:0+lstm_17/while/lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/Add_4±
4lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum/y
2lstm_17/while/lstm_cell_17/clip_by_value_2/MinimumMinimum$lstm_17/while/lstm_cell_17/Add_4:z:0=lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:2224
2lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum¡
,lstm_17/while/lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_17/while/lstm_cell_17/clip_by_value_2/yû
*lstm_17/while/lstm_cell_17/clip_by_value_2Maximum6lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum:z:05lstm_17/while/lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222,
*lstm_17/while/lstm_cell_17/clip_by_value_2
!lstm_17/while/lstm_cell_17/Tanh_1Tanh$lstm_17/while/lstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222#
!lstm_17/while/lstm_cell_17/Tanh_1Ë
 lstm_17/while/lstm_cell_17/mul_5Mul.lstm_17/while/lstm_cell_17/clip_by_value_2:z:0%lstm_17/while/lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/mul_5
2lstm_17/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_17_while_placeholder_1lstm_17_while_placeholder$lstm_17/while/lstm_cell_17/mul_5:z:0*
_output_shapes
: *
element_dtype024
2lstm_17/while/TensorArrayV2Write/TensorListSetIteml
lstm_17/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_17/while/add/y
lstm_17/while/addAddV2lstm_17_while_placeholderlstm_17/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_17/while/addp
lstm_17/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_17/while/add_1/y
lstm_17/while/add_1AddV2(lstm_17_while_lstm_17_while_loop_counterlstm_17/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_17/while/add_1
lstm_17/while/IdentityIdentitylstm_17/while/add_1:z:0^lstm_17/while/NoOp*
T0*
_output_shapes
: 2
lstm_17/while/Identity¦
lstm_17/while/Identity_1Identity.lstm_17_while_lstm_17_while_maximum_iterations^lstm_17/while/NoOp*
T0*
_output_shapes
: 2
lstm_17/while/Identity_1
lstm_17/while/Identity_2Identitylstm_17/while/add:z:0^lstm_17/while/NoOp*
T0*
_output_shapes
: 2
lstm_17/while/Identity_2º
lstm_17/while/Identity_3IdentityBlstm_17/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_17/while/NoOp*
T0*
_output_shapes
: 2
lstm_17/while/Identity_3¤
lstm_17/while/Identity_4Identity$lstm_17/while/lstm_cell_17/mul_5:z:0^lstm_17/while/NoOp*
T0*
_output_shapes

:222
lstm_17/while/Identity_4¤
lstm_17/while/Identity_5Identity$lstm_17/while/lstm_cell_17/add_3:z:0^lstm_17/while/NoOp*
T0*
_output_shapes

:222
lstm_17/while/Identity_5
lstm_17/while/NoOpNoOp2^lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp1^lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp3^lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_17/while/NoOp"9
lstm_17_while_identitylstm_17/while/Identity:output:0"=
lstm_17_while_identity_1!lstm_17/while/Identity_1:output:0"=
lstm_17_while_identity_2!lstm_17/while/Identity_2:output:0"=
lstm_17_while_identity_3!lstm_17/while/Identity_3:output:0"=
lstm_17_while_identity_4!lstm_17/while/Identity_4:output:0"=
lstm_17_while_identity_5!lstm_17/while/Identity_5:output:0"L
#lstm_17_while_lstm_17_strided_slice%lstm_17_while_lstm_17_strided_slice_0"z
:lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource<lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource_0"|
;lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource=lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource_0"x
9lstm_17_while_lstm_cell_17_matmul_readvariableop_resource;lstm_17_while_lstm_cell_17_matmul_readvariableop_resource_0"È
alstm_17_while_tensorarrayv2read_tensorlistgetitem_lstm_17_tensorarrayunstack_tensorlistfromtensorclstm_17_while_tensorarrayv2read_tensorlistgetitem_lstm_17_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2f
1lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp1lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp2d
0lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp0lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp2h
2lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp2lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
	
ð
'__inference_lstm_17_layer_call_fn_52678
inputs_0
unknown:22
	unknown_0:22
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
:2ÿÿÿÿÿÿÿÿÿ2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_17_layer_call_and_return_conditional_losses_485882
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2ÿÿÿÿÿÿÿÿÿ: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/0
ùn
´
B__inference_lstm_16_layer_call_and_return_conditional_losses_53435

inputs>
+lstm_cell_16_matmul_readvariableop_resource:	2È?
-lstm_cell_16_matmul_1_readvariableop_resource:22B
/lstm_cell_16_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_16_biasadd_readvariableop_resource:	È<
*lstm_cell_16_mul_2_readvariableop_resource:22
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_16/BiasAdd/ReadVariableOp¢"lstm_cell_16/MatMul/ReadVariableOp¢$lstm_cell_16/MatMul_1/ReadVariableOp¢&lstm_cell_16/MatMul_1/ReadVariableOp_1¢!lstm_cell_16/mul_2/ReadVariableOp¢whileu
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
222
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2   2   2
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_16/MatMul/ReadVariableOpReadVariableOp+lstm_cell_16_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02$
"lstm_cell_16/MatMul/ReadVariableOp¤
lstm_cell_16/MatMulMatMulstrided_slice_1:output:0*lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/MatMulº
$lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02&
$lstm_cell_16/MatMul_1/ReadVariableOpÁ
&lstm_cell_16/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_16_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_16/MatMul_1/ReadVariableOp_1À
lstm_cell_16/MatMul_1MatMul,lstm_cell_16/MatMul_1/ReadVariableOp:value:0.lstm_cell_16/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/MatMul_1
lstm_cell_16/addAddV2lstm_cell_16/MatMul:product:0lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/add´
#lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_16_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_16/BiasAdd/ReadVariableOp¤
lstm_cell_16/BiasAddBiasAddlstm_cell_16/add:z:0+lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/BiasAdd~
lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_16/split/split_dimÏ
lstm_cell_16/splitSplit%lstm_cell_16/split/split_dim:output:0lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_16/splitm
lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Constq
lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_1
lstm_cell_16/MulMullstm_cell_16/split:output:0lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul
lstm_cell_16/Add_1AddV2lstm_cell_16/Mul:z:0lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_1
$lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_16/clip_by_value/Minimum/yÃ
"lstm_cell_16/clip_by_value/MinimumMinimumlstm_cell_16/Add_1:z:0-lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222$
"lstm_cell_16/clip_by_value/Minimum
lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_16/clip_by_value/y»
lstm_cell_16/clip_by_valueMaximum&lstm_cell_16/clip_by_value/Minimum:z:0%lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_valueq
lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Const_2q
lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_3
lstm_cell_16/Mul_1Mullstm_cell_16/split:output:1lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul_1
lstm_cell_16/Add_2AddV2lstm_cell_16/Mul_1:z:0lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_2
&lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_16/clip_by_value_1/Minimum/yÉ
$lstm_cell_16/clip_by_value_1/MinimumMinimumlstm_cell_16/Add_2:z:0/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_16/clip_by_value_1/Minimum
lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_16/clip_by_value_1/yÃ
lstm_cell_16/clip_by_value_1Maximum(lstm_cell_16/clip_by_value_1/Minimum:z:0'lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_value_1±
!lstm_cell_16/mul_2/ReadVariableOpReadVariableOp*lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02#
!lstm_cell_16/mul_2/ReadVariableOp¥
lstm_cell_16/mul_2Mul lstm_cell_16/clip_by_value_1:z:0)lstm_cell_16/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_2t
lstm_cell_16/TanhTanhlstm_cell_16/split:output:2*
T0*
_output_shapes

:222
lstm_cell_16/Tanh
lstm_cell_16/mul_3Mullstm_cell_16/clip_by_value:z:0lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_3
lstm_cell_16/add_3AddV2lstm_cell_16/mul_2:z:0lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_16/add_3q
lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Const_4q
lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_5
lstm_cell_16/Mul_4Mullstm_cell_16/split:output:3lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul_4
lstm_cell_16/Add_4AddV2lstm_cell_16/Mul_4:z:0lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_4
&lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_16/clip_by_value_2/Minimum/yÉ
$lstm_cell_16/clip_by_value_2/MinimumMinimumlstm_cell_16/Add_4:z:0/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_16/clip_by_value_2/Minimum
lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_16/clip_by_value_2/yÃ
lstm_cell_16/clip_by_value_2Maximum(lstm_cell_16/clip_by_value_2/Minimum:z:0'lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_value_2s
lstm_cell_16/Tanh_1Tanhlstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_16/Tanh_1
lstm_cell_16/mul_5Mul lstm_cell_16/clip_by_value_2:z:0lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp-lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_16_matmul_readvariableop_resource/lstm_cell_16_matmul_1_readvariableop_1_resource,lstm_cell_16_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_53330*
condR
while_cond_53329*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_16_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_16/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_16_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_16/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_16/BiasAdd/ReadVariableOp#^lstm_cell_16/MatMul/ReadVariableOp%^lstm_cell_16/MatMul_1/ReadVariableOp'^lstm_cell_16/MatMul_1/ReadVariableOp_1"^lstm_cell_16/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:2
2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_16/BiasAdd/ReadVariableOp#lstm_cell_16/BiasAdd/ReadVariableOp2H
"lstm_cell_16/MatMul/ReadVariableOp"lstm_cell_16/MatMul/ReadVariableOp2L
$lstm_cell_16/MatMul_1/ReadVariableOp$lstm_cell_16/MatMul_1/ReadVariableOp2P
&lstm_cell_16/MatMul_1/ReadVariableOp_1&lstm_cell_16/MatMul_1/ReadVariableOp_12F
!lstm_cell_16/mul_2/ReadVariableOp!lstm_cell_16/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
¨
¼
while_cond_52973
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_52973___redundant_placeholder03
/while_while_cond_52973___redundant_placeholder13
/while_while_cond_52973___redundant_placeholder23
/while_while_cond_52973___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
ô.
¸
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53990

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
:	2È2
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
$:2:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
È
õ
,__inference_lstm_cell_17_layer_call_fn_53855

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
:22:22:22*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_486762
StatefulPartitionedCallr
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes

:222

Identityv

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*
_output_shapes

:222

Identity_1v

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*
_output_shapes

:222

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
$:2:22:22: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:2
 
_user_specified_nameinputs:HD

_output_shapes

:22
"
_user_specified_name
states/0:HD

_output_shapes

:22
"
_user_specified_name
states/1
¨
¼
while_cond_52557
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_52557___redundant_placeholder03
/while_while_cond_52557___redundant_placeholder13
/while_while_cond_52557___redundant_placeholder23
/while_while_cond_52557___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
ÔY
Ë
while_body_52558
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_17_matmul_readvariableop_resource_0:	ÈH
5while_lstm_cell_17_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_17_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_17_matmul_readvariableop_resource:	ÈF
3while_lstm_cell_17_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_17_biasadd_readvariableop_resource:	È¢)while/lstm_cell_17/BiasAdd/ReadVariableOp¢(while/lstm_cell_17/MatMul/ReadVariableOp¢*while/lstm_cell_17/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02*
(while/lstm_cell_17/MatMul/ReadVariableOpÎ
while/lstm_cell_17/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/MatMulÏ
*while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_17/MatMul_1/ReadVariableOp·
while/lstm_cell_17/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/MatMul_1¯
while/lstm_cell_17/addAddV2#while/lstm_cell_17/MatMul:product:0%while/lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/addÈ
)while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_17/BiasAdd/ReadVariableOp¼
while/lstm_cell_17/BiasAddBiasAddwhile/lstm_cell_17/add:z:01while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/BiasAdd
"while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_17/split/split_dimç
while/lstm_cell_17/splitSplit+while/lstm_cell_17/split/split_dim:output:0#while/lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_17/splity
while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const}
while/lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_1¦
while/lstm_cell_17/MulMul!while/lstm_cell_17/split:output:0!while/lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul§
while/lstm_cell_17/Add_1AddV2while/lstm_cell_17/Mul:z:0#while/lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_1
*while/lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_17/clip_by_value/Minimum/yÛ
(while/lstm_cell_17/clip_by_value/MinimumMinimumwhile/lstm_cell_17/Add_1:z:03while/lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(while/lstm_cell_17/clip_by_value/Minimum
"while/lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_17/clip_by_value/yÓ
 while/lstm_cell_17/clip_by_valueMaximum,while/lstm_cell_17/clip_by_value/Minimum:z:0+while/lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 while/lstm_cell_17/clip_by_value}
while/lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const_2}
while/lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_3¬
while/lstm_cell_17/Mul_1Mul!while/lstm_cell_17/split:output:1#while/lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul_1©
while/lstm_cell_17/Add_2AddV2while/lstm_cell_17/Mul_1:z:0#while/lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_2¡
,while/lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_17/clip_by_value_1/Minimum/yá
*while/lstm_cell_17/clip_by_value_1/MinimumMinimumwhile/lstm_cell_17/Add_2:z:05while/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_17/clip_by_value_1/Minimum
$while/lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_17/clip_by_value_1/yÛ
"while/lstm_cell_17/clip_by_value_1Maximum.while/lstm_cell_17/clip_by_value_1/Minimum:z:0-while/lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_17/clip_by_value_1¡
while/lstm_cell_17/mul_2Mul&while/lstm_cell_17/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_2
while/lstm_cell_17/TanhTanh!while/lstm_cell_17/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_17/Tanh§
while/lstm_cell_17/mul_3Mul$while/lstm_cell_17/clip_by_value:z:0while/lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_3¢
while/lstm_cell_17/add_3AddV2while/lstm_cell_17/mul_2:z:0while/lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_17/add_3}
while/lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const_4}
while/lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_5¬
while/lstm_cell_17/Mul_4Mul!while/lstm_cell_17/split:output:3#while/lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul_4©
while/lstm_cell_17/Add_4AddV2while/lstm_cell_17/Mul_4:z:0#while/lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_4¡
,while/lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_17/clip_by_value_2/Minimum/yá
*while/lstm_cell_17/clip_by_value_2/MinimumMinimumwhile/lstm_cell_17/Add_4:z:05while/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_17/clip_by_value_2/Minimum
$while/lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_17/clip_by_value_2/yÛ
"while/lstm_cell_17/clip_by_value_2Maximum.while/lstm_cell_17/clip_by_value_2/Minimum:z:0-while/lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_17/clip_by_value_2
while/lstm_cell_17/Tanh_1Tanhwhile/lstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Tanh_1«
while/lstm_cell_17/mul_5Mul&while/lstm_cell_17/clip_by_value_2:z:0while/lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_17/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_17/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_17/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*"
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
2while_lstm_cell_17_biasadd_readvariableop_resource4while_lstm_cell_17_biasadd_readvariableop_resource_0"l
3while_lstm_cell_17_matmul_1_readvariableop_resource5while_lstm_cell_17_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_17_matmul_readvariableop_resource3while_lstm_cell_17_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_17/BiasAdd/ReadVariableOp)while/lstm_cell_17/BiasAdd/ReadVariableOp2T
(while/lstm_cell_17/MatMul/ReadVariableOp(while/lstm_cell_17/MatMul/ReadVariableOp2X
*while/lstm_cell_17/MatMul_1/ReadVariableOp*while/lstm_cell_17/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
å$
Ø
while_body_48466
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
while_lstm_cell_17_48543_0:	È-
while_lstm_cell_17_48545_0:	2È)
while_lstm_cell_17_48547_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
while_lstm_cell_17_48543:	È+
while_lstm_cell_17_48545:	2È'
while_lstm_cell_17_48547:	È¢*while/lstm_cell_17/StatefulPartitionedCallÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÂ
*while/lstm_cell_17/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_17_48543_0while_lstm_cell_17_48545_0while_lstm_cell_17_48547_0*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:22:22:22*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_485422,
*while/lstm_cell_17/StatefulPartitionedCall÷
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder3while/lstm_cell_17/StatefulPartitionedCall:output:0*
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
while/Identity_4Identity3while/lstm_cell_17/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identity3while/lstm_cell_17/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5

while/NoOpNoOp+^while/lstm_cell_17/StatefulPartitionedCall*"
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
while_lstm_cell_17_48543while_lstm_cell_17_48543_0"6
while_lstm_cell_17_48545while_lstm_cell_17_48545_0"6
while_lstm_cell_17_48547while_lstm_cell_17_48547_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2X
*while/lstm_cell_17/StatefulPartitionedCall*while/lstm_cell_17/StatefulPartitionedCall: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
æ
õ
,__inference_lstm_cell_16_layer_call_fn_54405

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
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_543922
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
$:22:22:22: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:22
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
ô.
¸
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53916

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
:	2È2
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
$:2:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
ÔY
Ë
while_body_50139
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_17_matmul_readvariableop_resource_0:	ÈH
5while_lstm_cell_17_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_17_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_17_matmul_readvariableop_resource:	ÈF
3while_lstm_cell_17_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_17_biasadd_readvariableop_resource:	È¢)while/lstm_cell_17/BiasAdd/ReadVariableOp¢(while/lstm_cell_17/MatMul/ReadVariableOp¢*while/lstm_cell_17/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02*
(while/lstm_cell_17/MatMul/ReadVariableOpÎ
while/lstm_cell_17/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/MatMulÏ
*while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_17/MatMul_1/ReadVariableOp·
while/lstm_cell_17/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/MatMul_1¯
while/lstm_cell_17/addAddV2#while/lstm_cell_17/MatMul:product:0%while/lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/addÈ
)while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_17/BiasAdd/ReadVariableOp¼
while/lstm_cell_17/BiasAddBiasAddwhile/lstm_cell_17/add:z:01while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/BiasAdd
"while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_17/split/split_dimç
while/lstm_cell_17/splitSplit+while/lstm_cell_17/split/split_dim:output:0#while/lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_17/splity
while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const}
while/lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_1¦
while/lstm_cell_17/MulMul!while/lstm_cell_17/split:output:0!while/lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul§
while/lstm_cell_17/Add_1AddV2while/lstm_cell_17/Mul:z:0#while/lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_1
*while/lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_17/clip_by_value/Minimum/yÛ
(while/lstm_cell_17/clip_by_value/MinimumMinimumwhile/lstm_cell_17/Add_1:z:03while/lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(while/lstm_cell_17/clip_by_value/Minimum
"while/lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_17/clip_by_value/yÓ
 while/lstm_cell_17/clip_by_valueMaximum,while/lstm_cell_17/clip_by_value/Minimum:z:0+while/lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 while/lstm_cell_17/clip_by_value}
while/lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const_2}
while/lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_3¬
while/lstm_cell_17/Mul_1Mul!while/lstm_cell_17/split:output:1#while/lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul_1©
while/lstm_cell_17/Add_2AddV2while/lstm_cell_17/Mul_1:z:0#while/lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_2¡
,while/lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_17/clip_by_value_1/Minimum/yá
*while/lstm_cell_17/clip_by_value_1/MinimumMinimumwhile/lstm_cell_17/Add_2:z:05while/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_17/clip_by_value_1/Minimum
$while/lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_17/clip_by_value_1/yÛ
"while/lstm_cell_17/clip_by_value_1Maximum.while/lstm_cell_17/clip_by_value_1/Minimum:z:0-while/lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_17/clip_by_value_1¡
while/lstm_cell_17/mul_2Mul&while/lstm_cell_17/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_2
while/lstm_cell_17/TanhTanh!while/lstm_cell_17/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_17/Tanh§
while/lstm_cell_17/mul_3Mul$while/lstm_cell_17/clip_by_value:z:0while/lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_3¢
while/lstm_cell_17/add_3AddV2while/lstm_cell_17/mul_2:z:0while/lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_17/add_3}
while/lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const_4}
while/lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_5¬
while/lstm_cell_17/Mul_4Mul!while/lstm_cell_17/split:output:3#while/lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul_4©
while/lstm_cell_17/Add_4AddV2while/lstm_cell_17/Mul_4:z:0#while/lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_4¡
,while/lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_17/clip_by_value_2/Minimum/yá
*while/lstm_cell_17/clip_by_value_2/MinimumMinimumwhile/lstm_cell_17/Add_4:z:05while/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_17/clip_by_value_2/Minimum
$while/lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_17/clip_by_value_2/yÛ
"while/lstm_cell_17/clip_by_value_2Maximum.while/lstm_cell_17/clip_by_value_2/Minimum:z:0-while/lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_17/clip_by_value_2
while/lstm_cell_17/Tanh_1Tanhwhile/lstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Tanh_1«
while/lstm_cell_17/mul_5Mul&while/lstm_cell_17/clip_by_value_2:z:0while/lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_17/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_17/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_17/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*"
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
2while_lstm_cell_17_biasadd_readvariableop_resource4while_lstm_cell_17_biasadd_readvariableop_resource_0"l
3while_lstm_cell_17_matmul_1_readvariableop_resource5while_lstm_cell_17_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_17_matmul_readvariableop_resource3while_lstm_cell_17_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_17/BiasAdd/ReadVariableOp)while/lstm_cell_17/BiasAdd/ReadVariableOp2T
(while/lstm_cell_17/MatMul/ReadVariableOp(while/lstm_cell_17/MatMul/ReadVariableOp2X
*while/lstm_cell_17/MatMul_1/ReadVariableOp*while/lstm_cell_17/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
ï,

G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_48542

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
:	2È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2	
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
(:22:22:22:22*
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

:222
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
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

:222
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:222
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:222
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:222
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:222
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

:222
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:222
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:222
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:222

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
$:2:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:FB

_output_shapes

:22
 
_user_specified_namestates:FB

_output_shapes

:22
 
_user_specified_namestates
¨
¼
while_cond_50327
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_50327___redundant_placeholder03
/while_while_cond_50327___redundant_placeholder13
/while_while_cond_50327___redundant_placeholder23
/while_while_cond_50327___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
¤

ó
B__inference_dense_8_layer_call_and_return_conditional_losses_49945

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
È
õ
,__inference_lstm_cell_17_layer_call_fn_53838

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
:22:22:22*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_485422
StatefulPartitionedCallr
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes

:222

Identityv

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*
_output_shapes

:222

Identity_1v

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*
_output_shapes

:222

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
$:2:22:22: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:2
 
_user_specified_nameinputs:HD

_output_shapes

:22
"
_user_specified_name
states/0:HD

_output_shapes

:22
"
_user_specified_name
states/1
ùn
´
B__inference_lstm_16_layer_call_and_return_conditional_losses_50433

inputs>
+lstm_cell_16_matmul_readvariableop_resource:	2È?
-lstm_cell_16_matmul_1_readvariableop_resource:22B
/lstm_cell_16_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_16_biasadd_readvariableop_resource:	È<
*lstm_cell_16_mul_2_readvariableop_resource:22
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_16/BiasAdd/ReadVariableOp¢"lstm_cell_16/MatMul/ReadVariableOp¢$lstm_cell_16/MatMul_1/ReadVariableOp¢&lstm_cell_16/MatMul_1/ReadVariableOp_1¢!lstm_cell_16/mul_2/ReadVariableOp¢whileu
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
222
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2   2   2
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_16/MatMul/ReadVariableOpReadVariableOp+lstm_cell_16_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02$
"lstm_cell_16/MatMul/ReadVariableOp¤
lstm_cell_16/MatMulMatMulstrided_slice_1:output:0*lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/MatMulº
$lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02&
$lstm_cell_16/MatMul_1/ReadVariableOpÁ
&lstm_cell_16/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_16_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_16/MatMul_1/ReadVariableOp_1À
lstm_cell_16/MatMul_1MatMul,lstm_cell_16/MatMul_1/ReadVariableOp:value:0.lstm_cell_16/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/MatMul_1
lstm_cell_16/addAddV2lstm_cell_16/MatMul:product:0lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/add´
#lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_16_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_16/BiasAdd/ReadVariableOp¤
lstm_cell_16/BiasAddBiasAddlstm_cell_16/add:z:0+lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/BiasAdd~
lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_16/split/split_dimÏ
lstm_cell_16/splitSplit%lstm_cell_16/split/split_dim:output:0lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_16/splitm
lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Constq
lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_1
lstm_cell_16/MulMullstm_cell_16/split:output:0lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul
lstm_cell_16/Add_1AddV2lstm_cell_16/Mul:z:0lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_1
$lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_16/clip_by_value/Minimum/yÃ
"lstm_cell_16/clip_by_value/MinimumMinimumlstm_cell_16/Add_1:z:0-lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222$
"lstm_cell_16/clip_by_value/Minimum
lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_16/clip_by_value/y»
lstm_cell_16/clip_by_valueMaximum&lstm_cell_16/clip_by_value/Minimum:z:0%lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_valueq
lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Const_2q
lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_3
lstm_cell_16/Mul_1Mullstm_cell_16/split:output:1lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul_1
lstm_cell_16/Add_2AddV2lstm_cell_16/Mul_1:z:0lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_2
&lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_16/clip_by_value_1/Minimum/yÉ
$lstm_cell_16/clip_by_value_1/MinimumMinimumlstm_cell_16/Add_2:z:0/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_16/clip_by_value_1/Minimum
lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_16/clip_by_value_1/yÃ
lstm_cell_16/clip_by_value_1Maximum(lstm_cell_16/clip_by_value_1/Minimum:z:0'lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_value_1±
!lstm_cell_16/mul_2/ReadVariableOpReadVariableOp*lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02#
!lstm_cell_16/mul_2/ReadVariableOp¥
lstm_cell_16/mul_2Mul lstm_cell_16/clip_by_value_1:z:0)lstm_cell_16/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_2t
lstm_cell_16/TanhTanhlstm_cell_16/split:output:2*
T0*
_output_shapes

:222
lstm_cell_16/Tanh
lstm_cell_16/mul_3Mullstm_cell_16/clip_by_value:z:0lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_3
lstm_cell_16/add_3AddV2lstm_cell_16/mul_2:z:0lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_16/add_3q
lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Const_4q
lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_5
lstm_cell_16/Mul_4Mullstm_cell_16/split:output:3lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul_4
lstm_cell_16/Add_4AddV2lstm_cell_16/Mul_4:z:0lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_4
&lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_16/clip_by_value_2/Minimum/yÉ
$lstm_cell_16/clip_by_value_2/MinimumMinimumlstm_cell_16/Add_4:z:0/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_16/clip_by_value_2/Minimum
lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_16/clip_by_value_2/yÃ
lstm_cell_16/clip_by_value_2Maximum(lstm_cell_16/clip_by_value_2/Minimum:z:0'lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_value_2s
lstm_cell_16/Tanh_1Tanhlstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_16/Tanh_1
lstm_cell_16/mul_5Mul lstm_cell_16/clip_by_value_2:z:0lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp-lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_16_matmul_readvariableop_resource/lstm_cell_16_matmul_1_readvariableop_1_resource,lstm_cell_16_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_50328*
condR
while_cond_50327*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_16_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_16/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_16_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_16/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_16/BiasAdd/ReadVariableOp#^lstm_cell_16/MatMul/ReadVariableOp%^lstm_cell_16/MatMul_1/ReadVariableOp'^lstm_cell_16/MatMul_1/ReadVariableOp_1"^lstm_cell_16/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:2
2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_16/BiasAdd/ReadVariableOp#lstm_cell_16/BiasAdd/ReadVariableOp2H
"lstm_cell_16/MatMul/ReadVariableOp"lstm_cell_16/MatMul/ReadVariableOp2L
$lstm_cell_16/MatMul_1/ReadVariableOp$lstm_cell_16/MatMul_1/ReadVariableOp2P
&lstm_cell_16/MatMul_1/ReadVariableOp_1&lstm_cell_16/MatMul_1/ReadVariableOp_12F
!lstm_cell_16/mul_2/ReadVariableOp!lstm_cell_16/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
i
Ë

lstm_17_while_body_51238,
(lstm_17_while_lstm_17_while_loop_counter2
.lstm_17_while_lstm_17_while_maximum_iterations
lstm_17_while_placeholder
lstm_17_while_placeholder_1
lstm_17_while_placeholder_2
lstm_17_while_placeholder_3)
%lstm_17_while_lstm_17_strided_slice_0g
clstm_17_while_tensorarrayv2read_tensorlistgetitem_lstm_17_tensorarrayunstack_tensorlistfromtensor_0N
;lstm_17_while_lstm_cell_17_matmul_readvariableop_resource_0:	ÈP
=lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource_0:	2ÈK
<lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource_0:	È
lstm_17_while_identity
lstm_17_while_identity_1
lstm_17_while_identity_2
lstm_17_while_identity_3
lstm_17_while_identity_4
lstm_17_while_identity_5'
#lstm_17_while_lstm_17_strided_slicee
alstm_17_while_tensorarrayv2read_tensorlistgetitem_lstm_17_tensorarrayunstack_tensorlistfromtensorL
9lstm_17_while_lstm_cell_17_matmul_readvariableop_resource:	ÈN
;lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource:	2ÈI
:lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource:	È¢1lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp¢0lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp¢2lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOpÓ
?lstm_17/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      2A
?lstm_17/while/TensorArrayV2Read/TensorListGetItem/element_shapeú
1lstm_17/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemclstm_17_while_tensorarrayv2read_tensorlistgetitem_lstm_17_tensorarrayunstack_tensorlistfromtensor_0lstm_17_while_placeholderHlstm_17/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype023
1lstm_17/while/TensorArrayV2Read/TensorListGetItemá
0lstm_17/while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp;lstm_17_while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype022
0lstm_17/while/lstm_cell_17/MatMul/ReadVariableOpî
!lstm_17/while/lstm_cell_17/MatMulMatMul8lstm_17/while/TensorArrayV2Read/TensorListGetItem:item:08lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2#
!lstm_17/while/lstm_cell_17/MatMulç
2lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp=lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype024
2lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp×
#lstm_17/while/lstm_cell_17/MatMul_1MatMullstm_17_while_placeholder_2:lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2%
#lstm_17/while/lstm_cell_17/MatMul_1Ï
lstm_17/while/lstm_cell_17/addAddV2+lstm_17/while/lstm_cell_17/MatMul:product:0-lstm_17/while/lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2 
lstm_17/while/lstm_cell_17/addà
1lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp<lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype023
1lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOpÜ
"lstm_17/while/lstm_cell_17/BiasAddBiasAdd"lstm_17/while/lstm_cell_17/add:z:09lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2$
"lstm_17/while/lstm_cell_17/BiasAdd
*lstm_17/while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2,
*lstm_17/while/lstm_cell_17/split/split_dim
 lstm_17/while/lstm_cell_17/splitSplit3lstm_17/while/lstm_cell_17/split/split_dim:output:0+lstm_17/while/lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2"
 lstm_17/while/lstm_cell_17/split
 lstm_17/while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2"
 lstm_17/while/lstm_cell_17/Const
"lstm_17/while/lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_17/while/lstm_cell_17/Const_1Æ
lstm_17/while/lstm_cell_17/MulMul)lstm_17/while/lstm_cell_17/split:output:0)lstm_17/while/lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222 
lstm_17/while/lstm_cell_17/MulÇ
 lstm_17/while/lstm_cell_17/Add_1AddV2"lstm_17/while/lstm_cell_17/Mul:z:0+lstm_17/while/lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/Add_1­
2lstm_17/while/lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?24
2lstm_17/while/lstm_cell_17/clip_by_value/Minimum/yû
0lstm_17/while/lstm_cell_17/clip_by_value/MinimumMinimum$lstm_17/while/lstm_cell_17/Add_1:z:0;lstm_17/while/lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:2222
0lstm_17/while/lstm_cell_17/clip_by_value/Minimum
*lstm_17/while/lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_17/while/lstm_cell_17/clip_by_value/yó
(lstm_17/while/lstm_cell_17/clip_by_valueMaximum4lstm_17/while/lstm_cell_17/clip_by_value/Minimum:z:03lstm_17/while/lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222*
(lstm_17/while/lstm_cell_17/clip_by_value
"lstm_17/while/lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_17/while/lstm_cell_17/Const_2
"lstm_17/while/lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_17/while/lstm_cell_17/Const_3Ì
 lstm_17/while/lstm_cell_17/Mul_1Mul)lstm_17/while/lstm_cell_17/split:output:1+lstm_17/while/lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/Mul_1É
 lstm_17/while/lstm_cell_17/Add_2AddV2$lstm_17/while/lstm_cell_17/Mul_1:z:0+lstm_17/while/lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/Add_2±
4lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum/y
2lstm_17/while/lstm_cell_17/clip_by_value_1/MinimumMinimum$lstm_17/while/lstm_cell_17/Add_2:z:0=lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:2224
2lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum¡
,lstm_17/while/lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_17/while/lstm_cell_17/clip_by_value_1/yû
*lstm_17/while/lstm_cell_17/clip_by_value_1Maximum6lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum:z:05lstm_17/while/lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222,
*lstm_17/while/lstm_cell_17/clip_by_value_1Á
 lstm_17/while/lstm_cell_17/mul_2Mul.lstm_17/while/lstm_cell_17/clip_by_value_1:z:0lstm_17_while_placeholder_3*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/mul_2
lstm_17/while/lstm_cell_17/TanhTanh)lstm_17/while/lstm_cell_17/split:output:2*
T0*
_output_shapes

:222!
lstm_17/while/lstm_cell_17/TanhÇ
 lstm_17/while/lstm_cell_17/mul_3Mul,lstm_17/while/lstm_cell_17/clip_by_value:z:0#lstm_17/while/lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/mul_3Â
 lstm_17/while/lstm_cell_17/add_3AddV2$lstm_17/while/lstm_cell_17/mul_2:z:0$lstm_17/while/lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/add_3
"lstm_17/while/lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_17/while/lstm_cell_17/Const_4
"lstm_17/while/lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_17/while/lstm_cell_17/Const_5Ì
 lstm_17/while/lstm_cell_17/Mul_4Mul)lstm_17/while/lstm_cell_17/split:output:3+lstm_17/while/lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/Mul_4É
 lstm_17/while/lstm_cell_17/Add_4AddV2$lstm_17/while/lstm_cell_17/Mul_4:z:0+lstm_17/while/lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/Add_4±
4lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum/y
2lstm_17/while/lstm_cell_17/clip_by_value_2/MinimumMinimum$lstm_17/while/lstm_cell_17/Add_4:z:0=lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:2224
2lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum¡
,lstm_17/while/lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_17/while/lstm_cell_17/clip_by_value_2/yû
*lstm_17/while/lstm_cell_17/clip_by_value_2Maximum6lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum:z:05lstm_17/while/lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222,
*lstm_17/while/lstm_cell_17/clip_by_value_2
!lstm_17/while/lstm_cell_17/Tanh_1Tanh$lstm_17/while/lstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222#
!lstm_17/while/lstm_cell_17/Tanh_1Ë
 lstm_17/while/lstm_cell_17/mul_5Mul.lstm_17/while/lstm_cell_17/clip_by_value_2:z:0%lstm_17/while/lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222"
 lstm_17/while/lstm_cell_17/mul_5
2lstm_17/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_17_while_placeholder_1lstm_17_while_placeholder$lstm_17/while/lstm_cell_17/mul_5:z:0*
_output_shapes
: *
element_dtype024
2lstm_17/while/TensorArrayV2Write/TensorListSetIteml
lstm_17/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_17/while/add/y
lstm_17/while/addAddV2lstm_17_while_placeholderlstm_17/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_17/while/addp
lstm_17/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_17/while/add_1/y
lstm_17/while/add_1AddV2(lstm_17_while_lstm_17_while_loop_counterlstm_17/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_17/while/add_1
lstm_17/while/IdentityIdentitylstm_17/while/add_1:z:0^lstm_17/while/NoOp*
T0*
_output_shapes
: 2
lstm_17/while/Identity¦
lstm_17/while/Identity_1Identity.lstm_17_while_lstm_17_while_maximum_iterations^lstm_17/while/NoOp*
T0*
_output_shapes
: 2
lstm_17/while/Identity_1
lstm_17/while/Identity_2Identitylstm_17/while/add:z:0^lstm_17/while/NoOp*
T0*
_output_shapes
: 2
lstm_17/while/Identity_2º
lstm_17/while/Identity_3IdentityBlstm_17/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_17/while/NoOp*
T0*
_output_shapes
: 2
lstm_17/while/Identity_3¤
lstm_17/while/Identity_4Identity$lstm_17/while/lstm_cell_17/mul_5:z:0^lstm_17/while/NoOp*
T0*
_output_shapes

:222
lstm_17/while/Identity_4¤
lstm_17/while/Identity_5Identity$lstm_17/while/lstm_cell_17/add_3:z:0^lstm_17/while/NoOp*
T0*
_output_shapes

:222
lstm_17/while/Identity_5
lstm_17/while/NoOpNoOp2^lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp1^lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp3^lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_17/while/NoOp"9
lstm_17_while_identitylstm_17/while/Identity:output:0"=
lstm_17_while_identity_1!lstm_17/while/Identity_1:output:0"=
lstm_17_while_identity_2!lstm_17/while/Identity_2:output:0"=
lstm_17_while_identity_3!lstm_17/while/Identity_3:output:0"=
lstm_17_while_identity_4!lstm_17/while/Identity_4:output:0"=
lstm_17_while_identity_5!lstm_17/while/Identity_5:output:0"L
#lstm_17_while_lstm_17_strided_slice%lstm_17_while_lstm_17_strided_slice_0"z
:lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource<lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource_0"|
;lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource=lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource_0"x
9lstm_17_while_lstm_cell_17_matmul_readvariableop_resource;lstm_17_while_lstm_cell_17_matmul_readvariableop_resource_0"È
alstm_17_while_tensorarrayv2read_tensorlistgetitem_lstm_17_tensorarrayunstack_tensorlistfromtensorclstm_17_while_tensorarrayv2read_tensorlistgetitem_lstm_17_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2f
1lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp1lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp2d
0lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp0lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp2h
2lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp2lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
ø9

B__inference_lstm_16_layer_call_and_return_conditional_losses_49681

inputs$
lstm_cell_16_49593:22$
lstm_cell_16_49595:22%
lstm_cell_16_49597:	2È%
lstm_cell_16_49599:	2È!
lstm_cell_16_49601:	È
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢$lstm_cell_16/StatefulPartitionedCall¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ222
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1
$lstm_cell_16/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_16_49593lstm_cell_16_49595lstm_cell_16_49597lstm_cell_16_49599lstm_cell_16_49601*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:22:22:22*'
_read_only_resource_inputs	
*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_495432&
$lstm_cell_16/StatefulPartitionedCall
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOplstm_cell_16_49593*
_output_shapes

:22*
dtype02
ReadVariableOpw
ReadVariableOp_1ReadVariableOplstm_cell_16_49595*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_16_49597lstm_cell_16_49599lstm_cell_16_49601*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_49612*
condR
while_cond_49611*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22*
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

:22*
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
:2ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime·
AssignVariableOpAssignVariableOplstm_cell_16_49593while:output:4^ReadVariableOp%^lstm_cell_16/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp½
AssignVariableOp_1AssignVariableOplstm_cell_16_49595while:output:5^ReadVariableOp_1%^lstm_cell_16/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ22

IdentityÉ
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1%^lstm_cell_16/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2ÿÿÿÿÿÿÿÿÿ2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12L
$lstm_cell_16/StatefulPartitionedCall$lstm_cell_16/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
±0
Ô
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_49222

inputs
states:22
states_1:221
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
:	2È2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:22*
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
:	2È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2	
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
(:22:22:22:22*
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

:222
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
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

:222
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_1u
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes

:22*
dtype02
mul_2/ReadVariableOpq
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:222
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:222
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:222
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

:222
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:222
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:222
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:222

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
:22: : : : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:22
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
ÔY
Ë
while_body_50328
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_16_matmul_readvariableop_resource_0:	2ÈH
5while_lstm_cell_16_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_16_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_16_matmul_readvariableop_resource:	2ÈF
3while_lstm_cell_16_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_16_biasadd_readvariableop_resource:	È¢)while/lstm_cell_16/BiasAdd/ReadVariableOp¢(while/lstm_cell_16/MatMul/ReadVariableOp¢*while/lstm_cell_16/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_16/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_16_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02*
(while/lstm_cell_16/MatMul/ReadVariableOpÎ
while/lstm_cell_16/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/MatMulÏ
*while/lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_16_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_16/MatMul_1/ReadVariableOp·
while/lstm_cell_16/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_16/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/MatMul_1¯
while/lstm_cell_16/addAddV2#while/lstm_cell_16/MatMul:product:0%while/lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/addÈ
)while/lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_16_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_16/BiasAdd/ReadVariableOp¼
while/lstm_cell_16/BiasAddBiasAddwhile/lstm_cell_16/add:z:01while/lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/BiasAdd
"while/lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_16/split/split_dimç
while/lstm_cell_16/splitSplit+while/lstm_cell_16/split/split_dim:output:0#while/lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_16/splity
while/lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const}
while/lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_1¦
while/lstm_cell_16/MulMul!while/lstm_cell_16/split:output:0!while/lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul§
while/lstm_cell_16/Add_1AddV2while/lstm_cell_16/Mul:z:0#while/lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_1
*while/lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_16/clip_by_value/Minimum/yÛ
(while/lstm_cell_16/clip_by_value/MinimumMinimumwhile/lstm_cell_16/Add_1:z:03while/lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(while/lstm_cell_16/clip_by_value/Minimum
"while/lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_16/clip_by_value/yÓ
 while/lstm_cell_16/clip_by_valueMaximum,while/lstm_cell_16/clip_by_value/Minimum:z:0+while/lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 while/lstm_cell_16/clip_by_value}
while/lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const_2}
while/lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_3¬
while/lstm_cell_16/Mul_1Mul!while/lstm_cell_16/split:output:1#while/lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul_1©
while/lstm_cell_16/Add_2AddV2while/lstm_cell_16/Mul_1:z:0#while/lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_2¡
,while/lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_16/clip_by_value_1/Minimum/yá
*while/lstm_cell_16/clip_by_value_1/MinimumMinimumwhile/lstm_cell_16/Add_2:z:05while/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_16/clip_by_value_1/Minimum
$while/lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_16/clip_by_value_1/yÛ
"while/lstm_cell_16/clip_by_value_1Maximum.while/lstm_cell_16/clip_by_value_1/Minimum:z:0-while/lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_16/clip_by_value_1¡
while/lstm_cell_16/mul_2Mul&while/lstm_cell_16/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_2
while/lstm_cell_16/TanhTanh!while/lstm_cell_16/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_16/Tanh§
while/lstm_cell_16/mul_3Mul$while/lstm_cell_16/clip_by_value:z:0while/lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_3¢
while/lstm_cell_16/add_3AddV2while/lstm_cell_16/mul_2:z:0while/lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_16/add_3}
while/lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const_4}
while/lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_5¬
while/lstm_cell_16/Mul_4Mul!while/lstm_cell_16/split:output:3#while/lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul_4©
while/lstm_cell_16/Add_4AddV2while/lstm_cell_16/Mul_4:z:0#while/lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_4¡
,while/lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_16/clip_by_value_2/Minimum/yá
*while/lstm_cell_16/clip_by_value_2/MinimumMinimumwhile/lstm_cell_16/Add_4:z:05while/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_16/clip_by_value_2/Minimum
$while/lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_16/clip_by_value_2/yÛ
"while/lstm_cell_16/clip_by_value_2Maximum.while/lstm_cell_16/clip_by_value_2/Minimum:z:0-while/lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_16/clip_by_value_2
while/lstm_cell_16/Tanh_1Tanhwhile/lstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Tanh_1«
while/lstm_cell_16/mul_5Mul&while/lstm_cell_16/clip_by_value_2:z:0while/lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_16/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_16/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_16/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_16/BiasAdd/ReadVariableOp)^while/lstm_cell_16/MatMul/ReadVariableOp+^while/lstm_cell_16/MatMul_1/ReadVariableOp*"
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
2while_lstm_cell_16_biasadd_readvariableop_resource4while_lstm_cell_16_biasadd_readvariableop_resource_0"l
3while_lstm_cell_16_matmul_1_readvariableop_resource5while_lstm_cell_16_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_16_matmul_readvariableop_resource3while_lstm_cell_16_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_16/BiasAdd/ReadVariableOp)while/lstm_cell_16/BiasAdd/ReadVariableOp2T
(while/lstm_cell_16/MatMul/ReadVariableOp(while/lstm_cell_16/MatMul/ReadVariableOp2X
*while/lstm_cell_16/MatMul_1/ReadVariableOp*while/lstm_cell_16/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 

º
,__inference_sequential_8_layer_call_fn_51922

inputs
unknown:	È
	unknown_0:22
	unknown_1:	2È
	unknown_2:	È
	unknown_3:22
	unknown_4:	2È
	unknown_5:22
	unknown_6:	2È
	unknown_7:	È
	unknown_8:22
	unknown_9:2

unknown_10:
identity¢StatefulPartitionedCallñ
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_sequential_8_layer_call_and_return_conditional_losses_504672
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
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
&:2
: : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
¸

2__inference_time_distributed_8_layer_call_fn_53583

inputs
unknown:2
	unknown_0:
identity¢StatefulPartitionedCall
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
GPU 2J 8 *V
fQRO
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_500042
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
o
¶
B__inference_lstm_16_layer_call_and_return_conditional_losses_53079
inputs_0>
+lstm_cell_16_matmul_readvariableop_resource:	2È?
-lstm_cell_16_matmul_1_readvariableop_resource:22B
/lstm_cell_16_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_16_biasadd_readvariableop_resource:	È<
*lstm_cell_16_mul_2_readvariableop_resource:22
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_16/BiasAdd/ReadVariableOp¢"lstm_cell_16/MatMul/ReadVariableOp¢$lstm_cell_16/MatMul_1/ReadVariableOp¢&lstm_cell_16/MatMul_1/ReadVariableOp_1¢!lstm_cell_16/mul_2/ReadVariableOp¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ222
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_16/MatMul/ReadVariableOpReadVariableOp+lstm_cell_16_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02$
"lstm_cell_16/MatMul/ReadVariableOp¤
lstm_cell_16/MatMulMatMulstrided_slice_1:output:0*lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/MatMulº
$lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02&
$lstm_cell_16/MatMul_1/ReadVariableOpÁ
&lstm_cell_16/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_16_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_16/MatMul_1/ReadVariableOp_1À
lstm_cell_16/MatMul_1MatMul,lstm_cell_16/MatMul_1/ReadVariableOp:value:0.lstm_cell_16/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/MatMul_1
lstm_cell_16/addAddV2lstm_cell_16/MatMul:product:0lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/add´
#lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_16_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_16/BiasAdd/ReadVariableOp¤
lstm_cell_16/BiasAddBiasAddlstm_cell_16/add:z:0+lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/BiasAdd~
lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_16/split/split_dimÏ
lstm_cell_16/splitSplit%lstm_cell_16/split/split_dim:output:0lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_16/splitm
lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Constq
lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_1
lstm_cell_16/MulMullstm_cell_16/split:output:0lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul
lstm_cell_16/Add_1AddV2lstm_cell_16/Mul:z:0lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_1
$lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_16/clip_by_value/Minimum/yÃ
"lstm_cell_16/clip_by_value/MinimumMinimumlstm_cell_16/Add_1:z:0-lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222$
"lstm_cell_16/clip_by_value/Minimum
lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_16/clip_by_value/y»
lstm_cell_16/clip_by_valueMaximum&lstm_cell_16/clip_by_value/Minimum:z:0%lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_valueq
lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Const_2q
lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_3
lstm_cell_16/Mul_1Mullstm_cell_16/split:output:1lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul_1
lstm_cell_16/Add_2AddV2lstm_cell_16/Mul_1:z:0lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_2
&lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_16/clip_by_value_1/Minimum/yÉ
$lstm_cell_16/clip_by_value_1/MinimumMinimumlstm_cell_16/Add_2:z:0/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_16/clip_by_value_1/Minimum
lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_16/clip_by_value_1/yÃ
lstm_cell_16/clip_by_value_1Maximum(lstm_cell_16/clip_by_value_1/Minimum:z:0'lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_value_1±
!lstm_cell_16/mul_2/ReadVariableOpReadVariableOp*lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02#
!lstm_cell_16/mul_2/ReadVariableOp¥
lstm_cell_16/mul_2Mul lstm_cell_16/clip_by_value_1:z:0)lstm_cell_16/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_2t
lstm_cell_16/TanhTanhlstm_cell_16/split:output:2*
T0*
_output_shapes

:222
lstm_cell_16/Tanh
lstm_cell_16/mul_3Mullstm_cell_16/clip_by_value:z:0lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_3
lstm_cell_16/add_3AddV2lstm_cell_16/mul_2:z:0lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_16/add_3q
lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Const_4q
lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_5
lstm_cell_16/Mul_4Mullstm_cell_16/split:output:3lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul_4
lstm_cell_16/Add_4AddV2lstm_cell_16/Mul_4:z:0lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_4
&lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_16/clip_by_value_2/Minimum/yÉ
$lstm_cell_16/clip_by_value_2/MinimumMinimumlstm_cell_16/Add_4:z:0/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_16/clip_by_value_2/Minimum
lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_16/clip_by_value_2/yÃ
lstm_cell_16/clip_by_value_2Maximum(lstm_cell_16/clip_by_value_2/Minimum:z:0'lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_value_2s
lstm_cell_16/Tanh_1Tanhlstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_16/Tanh_1
lstm_cell_16/mul_5Mul lstm_cell_16/clip_by_value_2:z:0lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp-lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_16_matmul_readvariableop_resource/lstm_cell_16_matmul_1_readvariableop_1_resource,lstm_cell_16_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_52974*
condR
while_cond_52973*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22*
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

:22*
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
:2ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_16_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_16/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_16_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_16/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_16/BiasAdd/ReadVariableOp#^lstm_cell_16/MatMul/ReadVariableOp%^lstm_cell_16/MatMul_1/ReadVariableOp'^lstm_cell_16/MatMul_1/ReadVariableOp_1"^lstm_cell_16/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2ÿÿÿÿÿÿÿÿÿ2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_16/BiasAdd/ReadVariableOp#lstm_cell_16/BiasAdd/ReadVariableOp2H
"lstm_cell_16/MatMul/ReadVariableOp"lstm_cell_16/MatMul/ReadVariableOp2L
$lstm_cell_16/MatMul_1/ReadVariableOp$lstm_cell_16/MatMul_1/ReadVariableOp2P
&lstm_cell_16/MatMul_1/ReadVariableOp_1&lstm_cell_16/MatMul_1/ReadVariableOp_12F
!lstm_cell_16/mul_2/ReadVariableOp!lstm_cell_16/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ2
"
_user_specified_name
inputs/0
¬
Á
,__inference_sequential_8_layer_call_fn_50494
lstm_17_input
unknown:	È
	unknown_0:22
	unknown_1:	2È
	unknown_2:	È
	unknown_3:22
	unknown_4:	2È
	unknown_5:22
	unknown_6:	2È
	unknown_7:	È
	unknown_8:22
	unknown_9:2

unknown_10:
identity¢StatefulPartitionedCallø
StatefulPartitionedCallStatefulPartitionedCalllstm_17_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_sequential_8_layer_call_and_return_conditional_losses_504672
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
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
&:2
: : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:Q M
"
_output_shapes
:2

'
_user_specified_namelstm_17_input
Þ
î
'__inference_lstm_17_layer_call_fn_52723

inputs
unknown:	È
	unknown_0:22
	unknown_1:	2È
	unknown_2:	È
	unknown_3:22
identity¢StatefulPartitionedCall
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_17_layer_call_and_return_conditional_losses_509292
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
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
:2
: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
ÔY
Ë
while_body_52974
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_16_matmul_readvariableop_resource_0:	2ÈH
5while_lstm_cell_16_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_16_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_16_matmul_readvariableop_resource:	2ÈF
3while_lstm_cell_16_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_16_biasadd_readvariableop_resource:	È¢)while/lstm_cell_16/BiasAdd/ReadVariableOp¢(while/lstm_cell_16/MatMul/ReadVariableOp¢*while/lstm_cell_16/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_16/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_16_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02*
(while/lstm_cell_16/MatMul/ReadVariableOpÎ
while/lstm_cell_16/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/MatMulÏ
*while/lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_16_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_16/MatMul_1/ReadVariableOp·
while/lstm_cell_16/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_16/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/MatMul_1¯
while/lstm_cell_16/addAddV2#while/lstm_cell_16/MatMul:product:0%while/lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/addÈ
)while/lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_16_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_16/BiasAdd/ReadVariableOp¼
while/lstm_cell_16/BiasAddBiasAddwhile/lstm_cell_16/add:z:01while/lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/BiasAdd
"while/lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_16/split/split_dimç
while/lstm_cell_16/splitSplit+while/lstm_cell_16/split/split_dim:output:0#while/lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_16/splity
while/lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const}
while/lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_1¦
while/lstm_cell_16/MulMul!while/lstm_cell_16/split:output:0!while/lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul§
while/lstm_cell_16/Add_1AddV2while/lstm_cell_16/Mul:z:0#while/lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_1
*while/lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_16/clip_by_value/Minimum/yÛ
(while/lstm_cell_16/clip_by_value/MinimumMinimumwhile/lstm_cell_16/Add_1:z:03while/lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(while/lstm_cell_16/clip_by_value/Minimum
"while/lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_16/clip_by_value/yÓ
 while/lstm_cell_16/clip_by_valueMaximum,while/lstm_cell_16/clip_by_value/Minimum:z:0+while/lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 while/lstm_cell_16/clip_by_value}
while/lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const_2}
while/lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_3¬
while/lstm_cell_16/Mul_1Mul!while/lstm_cell_16/split:output:1#while/lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul_1©
while/lstm_cell_16/Add_2AddV2while/lstm_cell_16/Mul_1:z:0#while/lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_2¡
,while/lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_16/clip_by_value_1/Minimum/yá
*while/lstm_cell_16/clip_by_value_1/MinimumMinimumwhile/lstm_cell_16/Add_2:z:05while/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_16/clip_by_value_1/Minimum
$while/lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_16/clip_by_value_1/yÛ
"while/lstm_cell_16/clip_by_value_1Maximum.while/lstm_cell_16/clip_by_value_1/Minimum:z:0-while/lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_16/clip_by_value_1¡
while/lstm_cell_16/mul_2Mul&while/lstm_cell_16/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_2
while/lstm_cell_16/TanhTanh!while/lstm_cell_16/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_16/Tanh§
while/lstm_cell_16/mul_3Mul$while/lstm_cell_16/clip_by_value:z:0while/lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_3¢
while/lstm_cell_16/add_3AddV2while/lstm_cell_16/mul_2:z:0while/lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_16/add_3}
while/lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const_4}
while/lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_5¬
while/lstm_cell_16/Mul_4Mul!while/lstm_cell_16/split:output:3#while/lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul_4©
while/lstm_cell_16/Add_4AddV2while/lstm_cell_16/Mul_4:z:0#while/lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_4¡
,while/lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_16/clip_by_value_2/Minimum/yá
*while/lstm_cell_16/clip_by_value_2/MinimumMinimumwhile/lstm_cell_16/Add_4:z:05while/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_16/clip_by_value_2/Minimum
$while/lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_16/clip_by_value_2/yÛ
"while/lstm_cell_16/clip_by_value_2Maximum.while/lstm_cell_16/clip_by_value_2/Minimum:z:0-while/lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_16/clip_by_value_2
while/lstm_cell_16/Tanh_1Tanhwhile/lstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Tanh_1«
while/lstm_cell_16/mul_5Mul&while/lstm_cell_16/clip_by_value_2:z:0while/lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_16/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_16/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_16/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_16/BiasAdd/ReadVariableOp)^while/lstm_cell_16/MatMul/ReadVariableOp+^while/lstm_cell_16/MatMul_1/ReadVariableOp*"
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
2while_lstm_cell_16_biasadd_readvariableop_resource4while_lstm_cell_16_biasadd_readvariableop_resource_0"l
3while_lstm_cell_16_matmul_1_readvariableop_resource5while_lstm_cell_16_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_16_matmul_readvariableop_resource3while_lstm_cell_16_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_16/BiasAdd/ReadVariableOp)while/lstm_cell_16/BiasAdd/ReadVariableOp2T
(while/lstm_cell_16/MatMul/ReadVariableOp(while/lstm_cell_16/MatMul/ReadVariableOp2X
*while/lstm_cell_16/MatMul_1/ReadVariableOp*while/lstm_cell_16/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
¸

2__inference_time_distributed_8_layer_call_fn_53574

inputs
unknown:2
	unknown_0:
identity¢StatefulPartitionedCall
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
GPU 2J 8 *V
fQRO
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_499562
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
¤

ó
B__inference_dense_8_layer_call_and_return_conditional_losses_54415

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
÷,

G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54166

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
:	2È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2	
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
(:22:22:22:22*
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

:222
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
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

:222
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:222
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:222
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:222
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:222
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

:222
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:222
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:222
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:222

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
$:22:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:22
 
_user_specified_nameinputs:HD

_output_shapes

:22
"
_user_specified_name
states/0:HD

_output_shapes

:22
"
_user_specified_name
states/1
ùn
´
B__inference_lstm_17_layer_call_and_return_conditional_losses_50244

inputs>
+lstm_cell_17_matmul_readvariableop_resource:	È?
-lstm_cell_17_matmul_1_readvariableop_resource:22B
/lstm_cell_17_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_17_biasadd_readvariableop_resource:	È<
*lstm_cell_17_mul_2_readvariableop_resource:22
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_17/BiasAdd/ReadVariableOp¢"lstm_cell_17/MatMul/ReadVariableOp¢$lstm_cell_17/MatMul_1/ReadVariableOp¢&lstm_cell_17/MatMul_1/ReadVariableOp_1¢!lstm_cell_17/mul_2/ReadVariableOp¢whileu
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
22
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2      2
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_17/MatMul/ReadVariableOpReadVariableOp+lstm_cell_17_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02$
"lstm_cell_17/MatMul/ReadVariableOp¤
lstm_cell_17/MatMulMatMulstrided_slice_1:output:0*lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/MatMulº
$lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02&
$lstm_cell_17/MatMul_1/ReadVariableOpÁ
&lstm_cell_17/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_17_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_17/MatMul_1/ReadVariableOp_1À
lstm_cell_17/MatMul_1MatMul,lstm_cell_17/MatMul_1/ReadVariableOp:value:0.lstm_cell_17/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/MatMul_1
lstm_cell_17/addAddV2lstm_cell_17/MatMul:product:0lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/add´
#lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_17/BiasAdd/ReadVariableOp¤
lstm_cell_17/BiasAddBiasAddlstm_cell_17/add:z:0+lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/BiasAdd~
lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/split/split_dimÏ
lstm_cell_17/splitSplit%lstm_cell_17/split/split_dim:output:0lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_17/splitm
lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Constq
lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_1
lstm_cell_17/MulMullstm_cell_17/split:output:0lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul
lstm_cell_17/Add_1AddV2lstm_cell_17/Mul:z:0lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_1
$lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_17/clip_by_value/Minimum/yÃ
"lstm_cell_17/clip_by_value/MinimumMinimumlstm_cell_17/Add_1:z:0-lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222$
"lstm_cell_17/clip_by_value/Minimum
lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_17/clip_by_value/y»
lstm_cell_17/clip_by_valueMaximum&lstm_cell_17/clip_by_value/Minimum:z:0%lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_valueq
lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Const_2q
lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_3
lstm_cell_17/Mul_1Mullstm_cell_17/split:output:1lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul_1
lstm_cell_17/Add_2AddV2lstm_cell_17/Mul_1:z:0lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_2
&lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_17/clip_by_value_1/Minimum/yÉ
$lstm_cell_17/clip_by_value_1/MinimumMinimumlstm_cell_17/Add_2:z:0/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_17/clip_by_value_1/Minimum
lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_17/clip_by_value_1/yÃ
lstm_cell_17/clip_by_value_1Maximum(lstm_cell_17/clip_by_value_1/Minimum:z:0'lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_value_1±
!lstm_cell_17/mul_2/ReadVariableOpReadVariableOp*lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02#
!lstm_cell_17/mul_2/ReadVariableOp¥
lstm_cell_17/mul_2Mul lstm_cell_17/clip_by_value_1:z:0)lstm_cell_17/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_2t
lstm_cell_17/TanhTanhlstm_cell_17/split:output:2*
T0*
_output_shapes

:222
lstm_cell_17/Tanh
lstm_cell_17/mul_3Mullstm_cell_17/clip_by_value:z:0lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_3
lstm_cell_17/add_3AddV2lstm_cell_17/mul_2:z:0lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_17/add_3q
lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Const_4q
lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_5
lstm_cell_17/Mul_4Mullstm_cell_17/split:output:3lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul_4
lstm_cell_17/Add_4AddV2lstm_cell_17/Mul_4:z:0lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_4
&lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_17/clip_by_value_2/Minimum/yÉ
$lstm_cell_17/clip_by_value_2/MinimumMinimumlstm_cell_17/Add_4:z:0/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_17/clip_by_value_2/Minimum
lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_17/clip_by_value_2/yÃ
lstm_cell_17/clip_by_value_2Maximum(lstm_cell_17/clip_by_value_2/Minimum:z:0'lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_value_2s
lstm_cell_17/Tanh_1Tanhlstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_17/Tanh_1
lstm_cell_17/mul_5Mul lstm_cell_17/clip_by_value_2:z:0lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_17_matmul_readvariableop_resource/lstm_cell_17_matmul_1_readvariableop_1_resource,lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_50139*
condR
while_cond_50138*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_17_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_17_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_17/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_17/BiasAdd/ReadVariableOp#^lstm_cell_17/MatMul/ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp'^lstm_cell_17/MatMul_1/ReadVariableOp_1"^lstm_cell_17/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:2
: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_17/BiasAdd/ReadVariableOp#lstm_cell_17/BiasAdd/ReadVariableOp2H
"lstm_cell_17/MatMul/ReadVariableOp"lstm_cell_17/MatMul/ReadVariableOp2L
$lstm_cell_17/MatMul_1/ReadVariableOp$lstm_cell_17/MatMul_1/ReadVariableOp2P
&lstm_cell_17/MatMul_1/ReadVariableOp_1&lstm_cell_17/MatMul_1/ReadVariableOp_12F
!lstm_cell_17/mul_2/ReadVariableOp!lstm_cell_17/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
ï,

G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_49452

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
:	2È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2	
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
(:22:22:22:22*
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

:222
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
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

:222
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:222
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:222
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:222
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:222
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

:222
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:222
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:222
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:222

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
$:22:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:22
 
_user_specified_nameinputs:FB

_output_shapes

:22
 
_user_specified_namestates:FB

_output_shapes

:22
 
_user_specified_namestates
ÔY
Ë
while_body_53330
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_16_matmul_readvariableop_resource_0:	2ÈH
5while_lstm_cell_16_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_16_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_16_matmul_readvariableop_resource:	2ÈF
3while_lstm_cell_16_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_16_biasadd_readvariableop_resource:	È¢)while/lstm_cell_16/BiasAdd/ReadVariableOp¢(while/lstm_cell_16/MatMul/ReadVariableOp¢*while/lstm_cell_16/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_16/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_16_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02*
(while/lstm_cell_16/MatMul/ReadVariableOpÎ
while/lstm_cell_16/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/MatMulÏ
*while/lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_16_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_16/MatMul_1/ReadVariableOp·
while/lstm_cell_16/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_16/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/MatMul_1¯
while/lstm_cell_16/addAddV2#while/lstm_cell_16/MatMul:product:0%while/lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/addÈ
)while/lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_16_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_16/BiasAdd/ReadVariableOp¼
while/lstm_cell_16/BiasAddBiasAddwhile/lstm_cell_16/add:z:01while/lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/BiasAdd
"while/lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_16/split/split_dimç
while/lstm_cell_16/splitSplit+while/lstm_cell_16/split/split_dim:output:0#while/lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_16/splity
while/lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const}
while/lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_1¦
while/lstm_cell_16/MulMul!while/lstm_cell_16/split:output:0!while/lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul§
while/lstm_cell_16/Add_1AddV2while/lstm_cell_16/Mul:z:0#while/lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_1
*while/lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_16/clip_by_value/Minimum/yÛ
(while/lstm_cell_16/clip_by_value/MinimumMinimumwhile/lstm_cell_16/Add_1:z:03while/lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(while/lstm_cell_16/clip_by_value/Minimum
"while/lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_16/clip_by_value/yÓ
 while/lstm_cell_16/clip_by_valueMaximum,while/lstm_cell_16/clip_by_value/Minimum:z:0+while/lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 while/lstm_cell_16/clip_by_value}
while/lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const_2}
while/lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_3¬
while/lstm_cell_16/Mul_1Mul!while/lstm_cell_16/split:output:1#while/lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul_1©
while/lstm_cell_16/Add_2AddV2while/lstm_cell_16/Mul_1:z:0#while/lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_2¡
,while/lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_16/clip_by_value_1/Minimum/yá
*while/lstm_cell_16/clip_by_value_1/MinimumMinimumwhile/lstm_cell_16/Add_2:z:05while/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_16/clip_by_value_1/Minimum
$while/lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_16/clip_by_value_1/yÛ
"while/lstm_cell_16/clip_by_value_1Maximum.while/lstm_cell_16/clip_by_value_1/Minimum:z:0-while/lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_16/clip_by_value_1¡
while/lstm_cell_16/mul_2Mul&while/lstm_cell_16/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_2
while/lstm_cell_16/TanhTanh!while/lstm_cell_16/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_16/Tanh§
while/lstm_cell_16/mul_3Mul$while/lstm_cell_16/clip_by_value:z:0while/lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_3¢
while/lstm_cell_16/add_3AddV2while/lstm_cell_16/mul_2:z:0while/lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_16/add_3}
while/lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const_4}
while/lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_5¬
while/lstm_cell_16/Mul_4Mul!while/lstm_cell_16/split:output:3#while/lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul_4©
while/lstm_cell_16/Add_4AddV2while/lstm_cell_16/Mul_4:z:0#while/lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_4¡
,while/lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_16/clip_by_value_2/Minimum/yá
*while/lstm_cell_16/clip_by_value_2/MinimumMinimumwhile/lstm_cell_16/Add_4:z:05while/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_16/clip_by_value_2/Minimum
$while/lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_16/clip_by_value_2/yÛ
"while/lstm_cell_16/clip_by_value_2Maximum.while/lstm_cell_16/clip_by_value_2/Minimum:z:0-while/lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_16/clip_by_value_2
while/lstm_cell_16/Tanh_1Tanhwhile/lstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Tanh_1«
while/lstm_cell_16/mul_5Mul&while/lstm_cell_16/clip_by_value_2:z:0while/lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_16/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_16/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_16/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_16/BiasAdd/ReadVariableOp)^while/lstm_cell_16/MatMul/ReadVariableOp+^while/lstm_cell_16/MatMul_1/ReadVariableOp*"
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
2while_lstm_cell_16_biasadd_readvariableop_resource4while_lstm_cell_16_biasadd_readvariableop_resource_0"l
3while_lstm_cell_16_matmul_1_readvariableop_resource5while_lstm_cell_16_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_16_matmul_readvariableop_resource3while_lstm_cell_16_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_16/BiasAdd/ReadVariableOp)while/lstm_cell_16/BiasAdd/ReadVariableOp2T
(while/lstm_cell_16/MatMul/ReadVariableOp(while/lstm_cell_16/MatMul/ReadVariableOp2X
*while/lstm_cell_16/MatMul_1/ReadVariableOp*while/lstm_cell_16/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
ü.
º
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54223

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
:	2È2
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
$:22:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:22
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
¨
¼
while_cond_49241
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_49241___redundant_placeholder03
/while_while_cond_49241___redundant_placeholder13
/while_while_cond_49241___redundant_placeholder23
/while_while_cond_49241___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
®

M__inference_time_distributed_8_layer_call_and_return_conditional_losses_53551

inputs8
&dense_8_matmul_readvariableop_resource:25
'dense_8_biasadd_readvariableop_resource:
identity¢dense_8/BiasAdd/ReadVariableOp¢dense_8/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2
Reshape/shapeg
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes
:	ô22	
Reshape¥
dense_8/MatMul/ReadVariableOpReadVariableOp&dense_8_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_8/MatMul/ReadVariableOp
dense_8/MatMulMatMulReshape:output:0%dense_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô2
dense_8/MatMul¤
dense_8/BiasAdd/ReadVariableOpReadVariableOp'dense_8_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_8/BiasAdd/ReadVariableOp
dense_8/BiasAddBiasAdddense_8/MatMul:product:0&dense_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô2
dense_8/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      2
Reshape_1/shape
	Reshape_1Reshapedense_8/BiasAdd:output:0Reshape_1/shape:output:0*
T0*"
_output_shapes
:2
2
	Reshape_1h
IdentityIdentityReshape_1:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity
NoOpNoOp^dense_8/BiasAdd/ReadVariableOp^dense_8/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:2
2: : 2@
dense_8/BiasAdd/ReadVariableOpdense_8/BiasAdd/ReadVariableOp2>
dense_8/MatMul/ReadVariableOpdense_8/MatMul/ReadVariableOp:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
ÔY
Ë
while_body_52024
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_17_matmul_readvariableop_resource_0:	ÈH
5while_lstm_cell_17_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_17_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_17_matmul_readvariableop_resource:	ÈF
3while_lstm_cell_17_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_17_biasadd_readvariableop_resource:	È¢)while/lstm_cell_17/BiasAdd/ReadVariableOp¢(while/lstm_cell_17/MatMul/ReadVariableOp¢*while/lstm_cell_17/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02*
(while/lstm_cell_17/MatMul/ReadVariableOpÎ
while/lstm_cell_17/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/MatMulÏ
*while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_17/MatMul_1/ReadVariableOp·
while/lstm_cell_17/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/MatMul_1¯
while/lstm_cell_17/addAddV2#while/lstm_cell_17/MatMul:product:0%while/lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/addÈ
)while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_17/BiasAdd/ReadVariableOp¼
while/lstm_cell_17/BiasAddBiasAddwhile/lstm_cell_17/add:z:01while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/BiasAdd
"while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_17/split/split_dimç
while/lstm_cell_17/splitSplit+while/lstm_cell_17/split/split_dim:output:0#while/lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_17/splity
while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const}
while/lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_1¦
while/lstm_cell_17/MulMul!while/lstm_cell_17/split:output:0!while/lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul§
while/lstm_cell_17/Add_1AddV2while/lstm_cell_17/Mul:z:0#while/lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_1
*while/lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_17/clip_by_value/Minimum/yÛ
(while/lstm_cell_17/clip_by_value/MinimumMinimumwhile/lstm_cell_17/Add_1:z:03while/lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(while/lstm_cell_17/clip_by_value/Minimum
"while/lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_17/clip_by_value/yÓ
 while/lstm_cell_17/clip_by_valueMaximum,while/lstm_cell_17/clip_by_value/Minimum:z:0+while/lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 while/lstm_cell_17/clip_by_value}
while/lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const_2}
while/lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_3¬
while/lstm_cell_17/Mul_1Mul!while/lstm_cell_17/split:output:1#while/lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul_1©
while/lstm_cell_17/Add_2AddV2while/lstm_cell_17/Mul_1:z:0#while/lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_2¡
,while/lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_17/clip_by_value_1/Minimum/yá
*while/lstm_cell_17/clip_by_value_1/MinimumMinimumwhile/lstm_cell_17/Add_2:z:05while/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_17/clip_by_value_1/Minimum
$while/lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_17/clip_by_value_1/yÛ
"while/lstm_cell_17/clip_by_value_1Maximum.while/lstm_cell_17/clip_by_value_1/Minimum:z:0-while/lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_17/clip_by_value_1¡
while/lstm_cell_17/mul_2Mul&while/lstm_cell_17/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_2
while/lstm_cell_17/TanhTanh!while/lstm_cell_17/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_17/Tanh§
while/lstm_cell_17/mul_3Mul$while/lstm_cell_17/clip_by_value:z:0while/lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_3¢
while/lstm_cell_17/add_3AddV2while/lstm_cell_17/mul_2:z:0while/lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_17/add_3}
while/lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const_4}
while/lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_5¬
while/lstm_cell_17/Mul_4Mul!while/lstm_cell_17/split:output:3#while/lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul_4©
while/lstm_cell_17/Add_4AddV2while/lstm_cell_17/Mul_4:z:0#while/lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_4¡
,while/lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_17/clip_by_value_2/Minimum/yá
*while/lstm_cell_17/clip_by_value_2/MinimumMinimumwhile/lstm_cell_17/Add_4:z:05while/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_17/clip_by_value_2/Minimum
$while/lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_17/clip_by_value_2/yÛ
"while/lstm_cell_17/clip_by_value_2Maximum.while/lstm_cell_17/clip_by_value_2/Minimum:z:0-while/lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_17/clip_by_value_2
while/lstm_cell_17/Tanh_1Tanhwhile/lstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Tanh_1«
while/lstm_cell_17/mul_5Mul&while/lstm_cell_17/clip_by_value_2:z:0while/lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_17/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_17/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_17/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*"
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
2while_lstm_cell_17_biasadd_readvariableop_resource4while_lstm_cell_17_biasadd_readvariableop_resource_0"l
3while_lstm_cell_17_matmul_1_readvariableop_resource5while_lstm_cell_17_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_17_matmul_readvariableop_resource3while_lstm_cell_17_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_17/BiasAdd/ReadVariableOp)while/lstm_cell_17/BiasAdd/ReadVariableOp2T
(while/lstm_cell_17/MatMul/ReadVariableOp(while/lstm_cell_17/MatMul/ReadVariableOp2X
*while/lstm_cell_17/MatMul_1/ReadVariableOp*while/lstm_cell_17/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
±0
Ô
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_48767

inputs
states:22
states_1:221
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
:	2È2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:22*
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
:	2È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2	
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
(:22:22:22:22*
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

:222
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
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

:222
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_1u
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes

:22*
dtype02
mul_2/ReadVariableOpq
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:222
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:222
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:222
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

:222
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:222
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:222
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:222

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
:2: : : : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
ùn
´
B__inference_lstm_17_layer_call_and_return_conditional_losses_50929

inputs>
+lstm_cell_17_matmul_readvariableop_resource:	È?
-lstm_cell_17_matmul_1_readvariableop_resource:22B
/lstm_cell_17_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_17_biasadd_readvariableop_resource:	È<
*lstm_cell_17_mul_2_readvariableop_resource:22
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_17/BiasAdd/ReadVariableOp¢"lstm_cell_17/MatMul/ReadVariableOp¢$lstm_cell_17/MatMul_1/ReadVariableOp¢&lstm_cell_17/MatMul_1/ReadVariableOp_1¢!lstm_cell_17/mul_2/ReadVariableOp¢whileu
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
22
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2      2
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_17/MatMul/ReadVariableOpReadVariableOp+lstm_cell_17_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02$
"lstm_cell_17/MatMul/ReadVariableOp¤
lstm_cell_17/MatMulMatMulstrided_slice_1:output:0*lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/MatMulº
$lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02&
$lstm_cell_17/MatMul_1/ReadVariableOpÁ
&lstm_cell_17/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_17_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_17/MatMul_1/ReadVariableOp_1À
lstm_cell_17/MatMul_1MatMul,lstm_cell_17/MatMul_1/ReadVariableOp:value:0.lstm_cell_17/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/MatMul_1
lstm_cell_17/addAddV2lstm_cell_17/MatMul:product:0lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/add´
#lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_17/BiasAdd/ReadVariableOp¤
lstm_cell_17/BiasAddBiasAddlstm_cell_17/add:z:0+lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/BiasAdd~
lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/split/split_dimÏ
lstm_cell_17/splitSplit%lstm_cell_17/split/split_dim:output:0lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_17/splitm
lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Constq
lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_1
lstm_cell_17/MulMullstm_cell_17/split:output:0lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul
lstm_cell_17/Add_1AddV2lstm_cell_17/Mul:z:0lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_1
$lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_17/clip_by_value/Minimum/yÃ
"lstm_cell_17/clip_by_value/MinimumMinimumlstm_cell_17/Add_1:z:0-lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222$
"lstm_cell_17/clip_by_value/Minimum
lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_17/clip_by_value/y»
lstm_cell_17/clip_by_valueMaximum&lstm_cell_17/clip_by_value/Minimum:z:0%lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_valueq
lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Const_2q
lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_3
lstm_cell_17/Mul_1Mullstm_cell_17/split:output:1lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul_1
lstm_cell_17/Add_2AddV2lstm_cell_17/Mul_1:z:0lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_2
&lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_17/clip_by_value_1/Minimum/yÉ
$lstm_cell_17/clip_by_value_1/MinimumMinimumlstm_cell_17/Add_2:z:0/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_17/clip_by_value_1/Minimum
lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_17/clip_by_value_1/yÃ
lstm_cell_17/clip_by_value_1Maximum(lstm_cell_17/clip_by_value_1/Minimum:z:0'lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_value_1±
!lstm_cell_17/mul_2/ReadVariableOpReadVariableOp*lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02#
!lstm_cell_17/mul_2/ReadVariableOp¥
lstm_cell_17/mul_2Mul lstm_cell_17/clip_by_value_1:z:0)lstm_cell_17/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_2t
lstm_cell_17/TanhTanhlstm_cell_17/split:output:2*
T0*
_output_shapes

:222
lstm_cell_17/Tanh
lstm_cell_17/mul_3Mullstm_cell_17/clip_by_value:z:0lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_3
lstm_cell_17/add_3AddV2lstm_cell_17/mul_2:z:0lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_17/add_3q
lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Const_4q
lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_5
lstm_cell_17/Mul_4Mullstm_cell_17/split:output:3lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul_4
lstm_cell_17/Add_4AddV2lstm_cell_17/Mul_4:z:0lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_4
&lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_17/clip_by_value_2/Minimum/yÉ
$lstm_cell_17/clip_by_value_2/MinimumMinimumlstm_cell_17/Add_4:z:0/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_17/clip_by_value_2/Minimum
lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_17/clip_by_value_2/yÃ
lstm_cell_17/clip_by_value_2Maximum(lstm_cell_17/clip_by_value_2/Minimum:z:0'lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_value_2s
lstm_cell_17/Tanh_1Tanhlstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_17/Tanh_1
lstm_cell_17/mul_5Mul lstm_cell_17/clip_by_value_2:z:0lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_17_matmul_readvariableop_resource/lstm_cell_17_matmul_1_readvariableop_1_resource,lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_50824*
condR
while_cond_50823*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_17_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_17_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_17/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_17/BiasAdd/ReadVariableOp#^lstm_cell_17/MatMul/ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp'^lstm_cell_17/MatMul_1/ReadVariableOp_1"^lstm_cell_17/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:2
: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_17/BiasAdd/ReadVariableOp#lstm_cell_17/BiasAdd/ReadVariableOp2H
"lstm_cell_17/MatMul/ReadVariableOp"lstm_cell_17/MatMul/ReadVariableOp2L
$lstm_cell_17/MatMul_1/ReadVariableOp$lstm_cell_17/MatMul_1/ReadVariableOp2P
&lstm_cell_17/MatMul_1/ReadVariableOp_1&lstm_cell_17/MatMul_1/ReadVariableOp_12F
!lstm_cell_17/mul_2/ReadVariableOp!lstm_cell_17/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
o
¶
B__inference_lstm_17_layer_call_and_return_conditional_losses_52129
inputs_0>
+lstm_cell_17_matmul_readvariableop_resource:	È?
-lstm_cell_17_matmul_1_readvariableop_resource:22B
/lstm_cell_17_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_17_biasadd_readvariableop_resource:	È<
*lstm_cell_17_mul_2_readvariableop_resource:22
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_17/BiasAdd/ReadVariableOp¢"lstm_cell_17/MatMul/ReadVariableOp¢$lstm_cell_17/MatMul_1/ReadVariableOp¢&lstm_cell_17/MatMul_1/ReadVariableOp_1¢!lstm_cell_17/mul_2/ReadVariableOp¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_17/MatMul/ReadVariableOpReadVariableOp+lstm_cell_17_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02$
"lstm_cell_17/MatMul/ReadVariableOp¤
lstm_cell_17/MatMulMatMulstrided_slice_1:output:0*lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/MatMulº
$lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02&
$lstm_cell_17/MatMul_1/ReadVariableOpÁ
&lstm_cell_17/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_17_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_17/MatMul_1/ReadVariableOp_1À
lstm_cell_17/MatMul_1MatMul,lstm_cell_17/MatMul_1/ReadVariableOp:value:0.lstm_cell_17/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/MatMul_1
lstm_cell_17/addAddV2lstm_cell_17/MatMul:product:0lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/add´
#lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_17/BiasAdd/ReadVariableOp¤
lstm_cell_17/BiasAddBiasAddlstm_cell_17/add:z:0+lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/BiasAdd~
lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/split/split_dimÏ
lstm_cell_17/splitSplit%lstm_cell_17/split/split_dim:output:0lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_17/splitm
lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Constq
lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_1
lstm_cell_17/MulMullstm_cell_17/split:output:0lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul
lstm_cell_17/Add_1AddV2lstm_cell_17/Mul:z:0lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_1
$lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_17/clip_by_value/Minimum/yÃ
"lstm_cell_17/clip_by_value/MinimumMinimumlstm_cell_17/Add_1:z:0-lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222$
"lstm_cell_17/clip_by_value/Minimum
lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_17/clip_by_value/y»
lstm_cell_17/clip_by_valueMaximum&lstm_cell_17/clip_by_value/Minimum:z:0%lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_valueq
lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Const_2q
lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_3
lstm_cell_17/Mul_1Mullstm_cell_17/split:output:1lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul_1
lstm_cell_17/Add_2AddV2lstm_cell_17/Mul_1:z:0lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_2
&lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_17/clip_by_value_1/Minimum/yÉ
$lstm_cell_17/clip_by_value_1/MinimumMinimumlstm_cell_17/Add_2:z:0/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_17/clip_by_value_1/Minimum
lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_17/clip_by_value_1/yÃ
lstm_cell_17/clip_by_value_1Maximum(lstm_cell_17/clip_by_value_1/Minimum:z:0'lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_value_1±
!lstm_cell_17/mul_2/ReadVariableOpReadVariableOp*lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02#
!lstm_cell_17/mul_2/ReadVariableOp¥
lstm_cell_17/mul_2Mul lstm_cell_17/clip_by_value_1:z:0)lstm_cell_17/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_2t
lstm_cell_17/TanhTanhlstm_cell_17/split:output:2*
T0*
_output_shapes

:222
lstm_cell_17/Tanh
lstm_cell_17/mul_3Mullstm_cell_17/clip_by_value:z:0lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_3
lstm_cell_17/add_3AddV2lstm_cell_17/mul_2:z:0lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_17/add_3q
lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Const_4q
lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_5
lstm_cell_17/Mul_4Mullstm_cell_17/split:output:3lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul_4
lstm_cell_17/Add_4AddV2lstm_cell_17/Mul_4:z:0lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_4
&lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_17/clip_by_value_2/Minimum/yÉ
$lstm_cell_17/clip_by_value_2/MinimumMinimumlstm_cell_17/Add_4:z:0/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_17/clip_by_value_2/Minimum
lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_17/clip_by_value_2/yÃ
lstm_cell_17/clip_by_value_2Maximum(lstm_cell_17/clip_by_value_2/Minimum:z:0'lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_value_2s
lstm_cell_17/Tanh_1Tanhlstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_17/Tanh_1
lstm_cell_17/mul_5Mul lstm_cell_17/clip_by_value_2:z:0lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_17_matmul_readvariableop_resource/lstm_cell_17_matmul_1_readvariableop_1_resource,lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_52024*
condR
while_cond_52023*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22*
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

:22*
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
:2ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_17_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_17_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_17/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_17/BiasAdd/ReadVariableOp#^lstm_cell_17/MatMul/ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp'^lstm_cell_17/MatMul_1/ReadVariableOp_1"^lstm_cell_17/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2ÿÿÿÿÿÿÿÿÿ: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_17/BiasAdd/ReadVariableOp#lstm_cell_17/BiasAdd/ReadVariableOp2H
"lstm_cell_17/MatMul/ReadVariableOp"lstm_cell_17/MatMul/ReadVariableOp2L
$lstm_cell_17/MatMul_1/ReadVariableOp$lstm_cell_17/MatMul_1/ReadVariableOp2P
&lstm_cell_17/MatMul_1/ReadVariableOp_1&lstm_cell_17/MatMul_1/ReadVariableOp_12F
!lstm_cell_17/mul_2/ReadVariableOp!lstm_cell_17/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/0

º
,__inference_sequential_8_layer_call_fn_51951

inputs
unknown:	È
	unknown_0:22
	unknown_1:	2È
	unknown_2:	È
	unknown_3:22
	unknown_4:	2È
	unknown_5:22
	unknown_6:	2È
	unknown_7:	È
	unknown_8:22
	unknown_9:2

unknown_10:
identity¢StatefulPartitionedCallñ
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_sequential_8_layer_call_and_return_conditional_losses_510062
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
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
&:2
: : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
±0
Ô
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_49543

inputs
states:22
states_1:221
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
:	2È2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:22*
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
:	2È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2	
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
(:22:22:22:22*
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

:222
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
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

:222
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_1u
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes

:22*
dtype02
mul_2/ReadVariableOpq
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:222
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:222
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:222
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

:222
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:222
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:222
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:222

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
:22: : : : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:22
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
ÔY
Ë
while_body_52796
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_16_matmul_readvariableop_resource_0:	2ÈH
5while_lstm_cell_16_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_16_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_16_matmul_readvariableop_resource:	2ÈF
3while_lstm_cell_16_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_16_biasadd_readvariableop_resource:	È¢)while/lstm_cell_16/BiasAdd/ReadVariableOp¢(while/lstm_cell_16/MatMul/ReadVariableOp¢*while/lstm_cell_16/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_16/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_16_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02*
(while/lstm_cell_16/MatMul/ReadVariableOpÎ
while/lstm_cell_16/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/MatMulÏ
*while/lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_16_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_16/MatMul_1/ReadVariableOp·
while/lstm_cell_16/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_16/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/MatMul_1¯
while/lstm_cell_16/addAddV2#while/lstm_cell_16/MatMul:product:0%while/lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/addÈ
)while/lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_16_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_16/BiasAdd/ReadVariableOp¼
while/lstm_cell_16/BiasAddBiasAddwhile/lstm_cell_16/add:z:01while/lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/BiasAdd
"while/lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_16/split/split_dimç
while/lstm_cell_16/splitSplit+while/lstm_cell_16/split/split_dim:output:0#while/lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_16/splity
while/lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const}
while/lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_1¦
while/lstm_cell_16/MulMul!while/lstm_cell_16/split:output:0!while/lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul§
while/lstm_cell_16/Add_1AddV2while/lstm_cell_16/Mul:z:0#while/lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_1
*while/lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_16/clip_by_value/Minimum/yÛ
(while/lstm_cell_16/clip_by_value/MinimumMinimumwhile/lstm_cell_16/Add_1:z:03while/lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(while/lstm_cell_16/clip_by_value/Minimum
"while/lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_16/clip_by_value/yÓ
 while/lstm_cell_16/clip_by_valueMaximum,while/lstm_cell_16/clip_by_value/Minimum:z:0+while/lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 while/lstm_cell_16/clip_by_value}
while/lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const_2}
while/lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_3¬
while/lstm_cell_16/Mul_1Mul!while/lstm_cell_16/split:output:1#while/lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul_1©
while/lstm_cell_16/Add_2AddV2while/lstm_cell_16/Mul_1:z:0#while/lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_2¡
,while/lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_16/clip_by_value_1/Minimum/yá
*while/lstm_cell_16/clip_by_value_1/MinimumMinimumwhile/lstm_cell_16/Add_2:z:05while/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_16/clip_by_value_1/Minimum
$while/lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_16/clip_by_value_1/yÛ
"while/lstm_cell_16/clip_by_value_1Maximum.while/lstm_cell_16/clip_by_value_1/Minimum:z:0-while/lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_16/clip_by_value_1¡
while/lstm_cell_16/mul_2Mul&while/lstm_cell_16/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_2
while/lstm_cell_16/TanhTanh!while/lstm_cell_16/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_16/Tanh§
while/lstm_cell_16/mul_3Mul$while/lstm_cell_16/clip_by_value:z:0while/lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_3¢
while/lstm_cell_16/add_3AddV2while/lstm_cell_16/mul_2:z:0while/lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_16/add_3}
while/lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const_4}
while/lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_5¬
while/lstm_cell_16/Mul_4Mul!while/lstm_cell_16/split:output:3#while/lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul_4©
while/lstm_cell_16/Add_4AddV2while/lstm_cell_16/Mul_4:z:0#while/lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_4¡
,while/lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_16/clip_by_value_2/Minimum/yá
*while/lstm_cell_16/clip_by_value_2/MinimumMinimumwhile/lstm_cell_16/Add_4:z:05while/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_16/clip_by_value_2/Minimum
$while/lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_16/clip_by_value_2/yÛ
"while/lstm_cell_16/clip_by_value_2Maximum.while/lstm_cell_16/clip_by_value_2/Minimum:z:0-while/lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_16/clip_by_value_2
while/lstm_cell_16/Tanh_1Tanhwhile/lstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Tanh_1«
while/lstm_cell_16/mul_5Mul&while/lstm_cell_16/clip_by_value_2:z:0while/lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_16/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_16/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_16/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_16/BiasAdd/ReadVariableOp)^while/lstm_cell_16/MatMul/ReadVariableOp+^while/lstm_cell_16/MatMul_1/ReadVariableOp*"
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
2while_lstm_cell_16_biasadd_readvariableop_resource4while_lstm_cell_16_biasadd_readvariableop_resource_0"l
3while_lstm_cell_16_matmul_1_readvariableop_resource5while_lstm_cell_16_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_16_matmul_readvariableop_resource3while_lstm_cell_16_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_16/BiasAdd/ReadVariableOp)while/lstm_cell_16/BiasAdd/ReadVariableOp2T
(while/lstm_cell_16/MatMul/ReadVariableOp(while/lstm_cell_16/MatMul/ReadVariableOp2X
*while/lstm_cell_16/MatMul_1/ReadVariableOp*while/lstm_cell_16/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
È
õ
,__inference_lstm_cell_16_layer_call_fn_54240

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
:22:22:22*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_493182
StatefulPartitionedCallr
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes

:222

Identityv

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*
_output_shapes

:222

Identity_1v

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*
_output_shapes

:222

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
$:22:22:22: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:22
 
_user_specified_nameinputs:HD

_output_shapes

:22
"
_user_specified_name
states/0:HD

_output_shapes

:22
"
_user_specified_name
states/1
¨
¼
while_cond_50823
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_50823___redundant_placeholder03
/while_while_cond_50823___redundant_placeholder13
/while_while_cond_50823___redundant_placeholder23
/while_while_cond_50823___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
ô.
¸
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54392

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
:	2È2
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
$:22:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:22
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
÷,

G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53764

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
:	2È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2	
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
(:22:22:22:22*
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

:222
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
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

:222
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:222
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:222
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:222
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:222
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

:222
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:222
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:222
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:222

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
$:2:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:HD

_output_shapes

:22
"
_user_specified_name
states/0:HD

_output_shapes

:22
"
_user_specified_name
states/1
Ò

 __inference__wrapped_model_48369
lstm_17_inputS
@sequential_8_lstm_17_lstm_cell_17_matmul_readvariableop_resource:	ÈT
Bsequential_8_lstm_17_lstm_cell_17_matmul_1_readvariableop_resource:22W
Dsequential_8_lstm_17_lstm_cell_17_matmul_1_readvariableop_1_resource:	2ÈP
Asequential_8_lstm_17_lstm_cell_17_biasadd_readvariableop_resource:	ÈQ
?sequential_8_lstm_17_lstm_cell_17_mul_2_readvariableop_resource:22S
@sequential_8_lstm_16_lstm_cell_16_matmul_readvariableop_resource:	2ÈT
Bsequential_8_lstm_16_lstm_cell_16_matmul_1_readvariableop_resource:22W
Dsequential_8_lstm_16_lstm_cell_16_matmul_1_readvariableop_1_resource:	2ÈP
Asequential_8_lstm_16_lstm_cell_16_biasadd_readvariableop_resource:	ÈQ
?sequential_8_lstm_16_lstm_cell_16_mul_2_readvariableop_resource:22X
Fsequential_8_time_distributed_8_dense_8_matmul_readvariableop_resource:2U
Gsequential_8_time_distributed_8_dense_8_biasadd_readvariableop_resource:
identity¢%sequential_8/lstm_16/AssignVariableOp¢'sequential_8/lstm_16/AssignVariableOp_1¢#sequential_8/lstm_16/ReadVariableOp¢%sequential_8/lstm_16/ReadVariableOp_1¢8sequential_8/lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp¢7sequential_8/lstm_16/lstm_cell_16/MatMul/ReadVariableOp¢9sequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp¢;sequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1¢6sequential_8/lstm_16/lstm_cell_16/mul_2/ReadVariableOp¢sequential_8/lstm_16/while¢%sequential_8/lstm_17/AssignVariableOp¢'sequential_8/lstm_17/AssignVariableOp_1¢#sequential_8/lstm_17/ReadVariableOp¢%sequential_8/lstm_17/ReadVariableOp_1¢8sequential_8/lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp¢7sequential_8/lstm_17/lstm_cell_17/MatMul/ReadVariableOp¢9sequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp¢;sequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1¢6sequential_8/lstm_17/lstm_cell_17/mul_2/ReadVariableOp¢sequential_8/lstm_17/while¢>sequential_8/time_distributed_8/dense_8/BiasAdd/ReadVariableOp¢=sequential_8/time_distributed_8/dense_8/MatMul/ReadVariableOp
#sequential_8/lstm_17/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2%
#sequential_8/lstm_17/transpose/perm·
sequential_8/lstm_17/transpose	Transposelstm_17_input,sequential_8/lstm_17/transpose/perm:output:0*
T0*"
_output_shapes
:
22 
sequential_8/lstm_17/transpose
sequential_8/lstm_17/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2      2
sequential_8/lstm_17/Shape
(sequential_8/lstm_17/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2*
(sequential_8/lstm_17/strided_slice/stack¢
*sequential_8/lstm_17/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2,
*sequential_8/lstm_17/strided_slice/stack_1¢
*sequential_8/lstm_17/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2,
*sequential_8/lstm_17/strided_slice/stack_2à
"sequential_8/lstm_17/strided_sliceStridedSlice#sequential_8/lstm_17/Shape:output:01sequential_8/lstm_17/strided_slice/stack:output:03sequential_8/lstm_17/strided_slice/stack_1:output:03sequential_8/lstm_17/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2$
"sequential_8/lstm_17/strided_slice¯
0sequential_8/lstm_17/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ22
0sequential_8/lstm_17/TensorArrayV2/element_shape
"sequential_8/lstm_17/TensorArrayV2TensorListReserve9sequential_8/lstm_17/TensorArrayV2/element_shape:output:0+sequential_8/lstm_17/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02$
"sequential_8/lstm_17/TensorArrayV2é
Jsequential_8/lstm_17/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      2L
Jsequential_8/lstm_17/TensorArrayUnstack/TensorListFromTensor/element_shapeÌ
<sequential_8/lstm_17/TensorArrayUnstack/TensorListFromTensorTensorListFromTensor"sequential_8/lstm_17/transpose:y:0Ssequential_8/lstm_17/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02>
<sequential_8/lstm_17/TensorArrayUnstack/TensorListFromTensor¢
*sequential_8/lstm_17/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2,
*sequential_8/lstm_17/strided_slice_1/stack¦
,sequential_8/lstm_17/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_8/lstm_17/strided_slice_1/stack_1¦
,sequential_8/lstm_17/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_8/lstm_17/strided_slice_1/stack_2ñ
$sequential_8/lstm_17/strided_slice_1StridedSlice"sequential_8/lstm_17/transpose:y:03sequential_8/lstm_17/strided_slice_1/stack:output:05sequential_8/lstm_17/strided_slice_1/stack_1:output:05sequential_8/lstm_17/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2&
$sequential_8/lstm_17/strided_slice_1ô
7sequential_8/lstm_17/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp@sequential_8_lstm_17_lstm_cell_17_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype029
7sequential_8/lstm_17/lstm_cell_17/MatMul/ReadVariableOpø
(sequential_8/lstm_17/lstm_cell_17/MatMulMatMul-sequential_8/lstm_17/strided_slice_1:output:0?sequential_8/lstm_17/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2*
(sequential_8/lstm_17/lstm_cell_17/MatMulù
9sequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOpBsequential_8_lstm_17_lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02;
9sequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp
;sequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1ReadVariableOpDsequential_8_lstm_17_lstm_cell_17_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02=
;sequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1
*sequential_8/lstm_17/lstm_cell_17/MatMul_1MatMulAsequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp:value:0Csequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2,
*sequential_8/lstm_17/lstm_cell_17/MatMul_1ë
%sequential_8/lstm_17/lstm_cell_17/addAddV22sequential_8/lstm_17/lstm_cell_17/MatMul:product:04sequential_8/lstm_17/lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2'
%sequential_8/lstm_17/lstm_cell_17/addó
8sequential_8/lstm_17/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOpAsequential_8_lstm_17_lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02:
8sequential_8/lstm_17/lstm_cell_17/BiasAdd/ReadVariableOpø
)sequential_8/lstm_17/lstm_cell_17/BiasAddBiasAdd)sequential_8/lstm_17/lstm_cell_17/add:z:0@sequential_8/lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2+
)sequential_8/lstm_17/lstm_cell_17/BiasAdd¨
1sequential_8/lstm_17/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :23
1sequential_8/lstm_17/lstm_cell_17/split/split_dim£
'sequential_8/lstm_17/lstm_cell_17/splitSplit:sequential_8/lstm_17/lstm_cell_17/split/split_dim:output:02sequential_8/lstm_17/lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2)
'sequential_8/lstm_17/lstm_cell_17/split
'sequential_8/lstm_17/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2)
'sequential_8/lstm_17/lstm_cell_17/Const
)sequential_8/lstm_17/lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2+
)sequential_8/lstm_17/lstm_cell_17/Const_1â
%sequential_8/lstm_17/lstm_cell_17/MulMul0sequential_8/lstm_17/lstm_cell_17/split:output:00sequential_8/lstm_17/lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222'
%sequential_8/lstm_17/lstm_cell_17/Mulã
'sequential_8/lstm_17/lstm_cell_17/Add_1AddV2)sequential_8/lstm_17/lstm_cell_17/Mul:z:02sequential_8/lstm_17/lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_17/lstm_cell_17/Add_1»
9sequential_8/lstm_17/lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2;
9sequential_8/lstm_17/lstm_cell_17/clip_by_value/Minimum/y
7sequential_8/lstm_17/lstm_cell_17/clip_by_value/MinimumMinimum+sequential_8/lstm_17/lstm_cell_17/Add_1:z:0Bsequential_8/lstm_17/lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:2229
7sequential_8/lstm_17/lstm_cell_17/clip_by_value/Minimum«
1sequential_8/lstm_17/lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    23
1sequential_8/lstm_17/lstm_cell_17/clip_by_value/y
/sequential_8/lstm_17/lstm_cell_17/clip_by_valueMaximum;sequential_8/lstm_17/lstm_cell_17/clip_by_value/Minimum:z:0:sequential_8/lstm_17/lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:2221
/sequential_8/lstm_17/lstm_cell_17/clip_by_value
)sequential_8/lstm_17/lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2+
)sequential_8/lstm_17/lstm_cell_17/Const_2
)sequential_8/lstm_17/lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2+
)sequential_8/lstm_17/lstm_cell_17/Const_3è
'sequential_8/lstm_17/lstm_cell_17/Mul_1Mul0sequential_8/lstm_17/lstm_cell_17/split:output:12sequential_8/lstm_17/lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_17/lstm_cell_17/Mul_1å
'sequential_8/lstm_17/lstm_cell_17/Add_2AddV2+sequential_8/lstm_17/lstm_cell_17/Mul_1:z:02sequential_8/lstm_17/lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_17/lstm_cell_17/Add_2¿
;sequential_8/lstm_17/lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2=
;sequential_8/lstm_17/lstm_cell_17/clip_by_value_1/Minimum/y
9sequential_8/lstm_17/lstm_cell_17/clip_by_value_1/MinimumMinimum+sequential_8/lstm_17/lstm_cell_17/Add_2:z:0Dsequential_8/lstm_17/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222;
9sequential_8/lstm_17/lstm_cell_17/clip_by_value_1/Minimum¯
3sequential_8/lstm_17/lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    25
3sequential_8/lstm_17/lstm_cell_17/clip_by_value_1/y
1sequential_8/lstm_17/lstm_cell_17/clip_by_value_1Maximum=sequential_8/lstm_17/lstm_cell_17/clip_by_value_1/Minimum:z:0<sequential_8/lstm_17/lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:2223
1sequential_8/lstm_17/lstm_cell_17/clip_by_value_1ð
6sequential_8/lstm_17/lstm_cell_17/mul_2/ReadVariableOpReadVariableOp?sequential_8_lstm_17_lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype028
6sequential_8/lstm_17/lstm_cell_17/mul_2/ReadVariableOpù
'sequential_8/lstm_17/lstm_cell_17/mul_2Mul5sequential_8/lstm_17/lstm_cell_17/clip_by_value_1:z:0>sequential_8/lstm_17/lstm_cell_17/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_17/lstm_cell_17/mul_2³
&sequential_8/lstm_17/lstm_cell_17/TanhTanh0sequential_8/lstm_17/lstm_cell_17/split:output:2*
T0*
_output_shapes

:222(
&sequential_8/lstm_17/lstm_cell_17/Tanhã
'sequential_8/lstm_17/lstm_cell_17/mul_3Mul3sequential_8/lstm_17/lstm_cell_17/clip_by_value:z:0*sequential_8/lstm_17/lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_17/lstm_cell_17/mul_3Þ
'sequential_8/lstm_17/lstm_cell_17/add_3AddV2+sequential_8/lstm_17/lstm_cell_17/mul_2:z:0+sequential_8/lstm_17/lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_17/lstm_cell_17/add_3
)sequential_8/lstm_17/lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2+
)sequential_8/lstm_17/lstm_cell_17/Const_4
)sequential_8/lstm_17/lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2+
)sequential_8/lstm_17/lstm_cell_17/Const_5è
'sequential_8/lstm_17/lstm_cell_17/Mul_4Mul0sequential_8/lstm_17/lstm_cell_17/split:output:32sequential_8/lstm_17/lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_17/lstm_cell_17/Mul_4å
'sequential_8/lstm_17/lstm_cell_17/Add_4AddV2+sequential_8/lstm_17/lstm_cell_17/Mul_4:z:02sequential_8/lstm_17/lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_17/lstm_cell_17/Add_4¿
;sequential_8/lstm_17/lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2=
;sequential_8/lstm_17/lstm_cell_17/clip_by_value_2/Minimum/y
9sequential_8/lstm_17/lstm_cell_17/clip_by_value_2/MinimumMinimum+sequential_8/lstm_17/lstm_cell_17/Add_4:z:0Dsequential_8/lstm_17/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222;
9sequential_8/lstm_17/lstm_cell_17/clip_by_value_2/Minimum¯
3sequential_8/lstm_17/lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    25
3sequential_8/lstm_17/lstm_cell_17/clip_by_value_2/y
1sequential_8/lstm_17/lstm_cell_17/clip_by_value_2Maximum=sequential_8/lstm_17/lstm_cell_17/clip_by_value_2/Minimum:z:0<sequential_8/lstm_17/lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:2223
1sequential_8/lstm_17/lstm_cell_17/clip_by_value_2²
(sequential_8/lstm_17/lstm_cell_17/Tanh_1Tanh+sequential_8/lstm_17/lstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222*
(sequential_8/lstm_17/lstm_cell_17/Tanh_1ç
'sequential_8/lstm_17/lstm_cell_17/mul_5Mul5sequential_8/lstm_17/lstm_cell_17/clip_by_value_2:z:0,sequential_8/lstm_17/lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_17/lstm_cell_17/mul_5¹
2sequential_8/lstm_17/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   24
2sequential_8/lstm_17/TensorArrayV2_1/element_shape
$sequential_8/lstm_17/TensorArrayV2_1TensorListReserve;sequential_8/lstm_17/TensorArrayV2_1/element_shape:output:0+sequential_8/lstm_17/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02&
$sequential_8/lstm_17/TensorArrayV2_1x
sequential_8/lstm_17/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential_8/lstm_17/timeÍ
#sequential_8/lstm_17/ReadVariableOpReadVariableOpBsequential_8_lstm_17_lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#sequential_8/lstm_17/ReadVariableOpÎ
%sequential_8/lstm_17/ReadVariableOp_1ReadVariableOp?sequential_8_lstm_17_lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02'
%sequential_8/lstm_17/ReadVariableOp_1©
-sequential_8/lstm_17/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2/
-sequential_8/lstm_17/while/maximum_iterations
'sequential_8/lstm_17/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2)
'sequential_8/lstm_17/while/loop_counter°
sequential_8/lstm_17/whileWhile0sequential_8/lstm_17/while/loop_counter:output:06sequential_8/lstm_17/while/maximum_iterations:output:0"sequential_8/lstm_17/time:output:0-sequential_8/lstm_17/TensorArrayV2_1:handle:0+sequential_8/lstm_17/ReadVariableOp:value:0-sequential_8/lstm_17/ReadVariableOp_1:value:0+sequential_8/lstm_17/strided_slice:output:0Lsequential_8/lstm_17/TensorArrayUnstack/TensorListFromTensor:output_handle:0@sequential_8_lstm_17_lstm_cell_17_matmul_readvariableop_resourceDsequential_8_lstm_17_lstm_cell_17_matmul_1_readvariableop_1_resourceAsequential_8_lstm_17_lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *1
body)R'
%sequential_8_lstm_17_while_body_48078*1
cond)R'
%sequential_8_lstm_17_while_cond_48077*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
sequential_8/lstm_17/whileß
Esequential_8/lstm_17/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2G
Esequential_8/lstm_17/TensorArrayV2Stack/TensorListStack/element_shape³
7sequential_8/lstm_17/TensorArrayV2Stack/TensorListStackTensorListStack#sequential_8/lstm_17/while:output:3Nsequential_8/lstm_17/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
element_dtype029
7sequential_8/lstm_17/TensorArrayV2Stack/TensorListStack«
*sequential_8/lstm_17/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2,
*sequential_8/lstm_17/strided_slice_2/stack¦
,sequential_8/lstm_17/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2.
,sequential_8/lstm_17/strided_slice_2/stack_1¦
,sequential_8/lstm_17/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_8/lstm_17/strided_slice_2/stack_2
$sequential_8/lstm_17/strided_slice_2StridedSlice@sequential_8/lstm_17/TensorArrayV2Stack/TensorListStack:tensor:03sequential_8/lstm_17/strided_slice_2/stack:output:05sequential_8/lstm_17/strided_slice_2/stack_1:output:05sequential_8/lstm_17/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2&
$sequential_8/lstm_17/strided_slice_2£
%sequential_8/lstm_17/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2'
%sequential_8/lstm_17/transpose_1/permð
 sequential_8/lstm_17/transpose_1	Transpose@sequential_8/lstm_17/TensorArrayV2Stack/TensorListStack:tensor:0.sequential_8/lstm_17/transpose_1/perm:output:0*
T0*"
_output_shapes
:2
22"
 sequential_8/lstm_17/transpose_1
sequential_8/lstm_17/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential_8/lstm_17/runtimeÐ
%sequential_8/lstm_17/AssignVariableOpAssignVariableOpBsequential_8_lstm_17_lstm_cell_17_matmul_1_readvariableop_resource#sequential_8/lstm_17/while:output:4$^sequential_8/lstm_17/ReadVariableOp:^sequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02'
%sequential_8/lstm_17/AssignVariableOpÐ
'sequential_8/lstm_17/AssignVariableOp_1AssignVariableOp?sequential_8_lstm_17_lstm_cell_17_mul_2_readvariableop_resource#sequential_8/lstm_17/while:output:5&^sequential_8/lstm_17/ReadVariableOp_17^sequential_8/lstm_17/lstm_cell_17/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02)
'sequential_8/lstm_17/AssignVariableOp_1
#sequential_8/lstm_16/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2%
#sequential_8/lstm_16/transpose/permÎ
sequential_8/lstm_16/transpose	Transpose$sequential_8/lstm_17/transpose_1:y:0,sequential_8/lstm_16/transpose/perm:output:0*
T0*"
_output_shapes
:
222 
sequential_8/lstm_16/transpose
sequential_8/lstm_16/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2   2   2
sequential_8/lstm_16/Shape
(sequential_8/lstm_16/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2*
(sequential_8/lstm_16/strided_slice/stack¢
*sequential_8/lstm_16/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2,
*sequential_8/lstm_16/strided_slice/stack_1¢
*sequential_8/lstm_16/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2,
*sequential_8/lstm_16/strided_slice/stack_2à
"sequential_8/lstm_16/strided_sliceStridedSlice#sequential_8/lstm_16/Shape:output:01sequential_8/lstm_16/strided_slice/stack:output:03sequential_8/lstm_16/strided_slice/stack_1:output:03sequential_8/lstm_16/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2$
"sequential_8/lstm_16/strided_slice¯
0sequential_8/lstm_16/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ22
0sequential_8/lstm_16/TensorArrayV2/element_shape
"sequential_8/lstm_16/TensorArrayV2TensorListReserve9sequential_8/lstm_16/TensorArrayV2/element_shape:output:0+sequential_8/lstm_16/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02$
"sequential_8/lstm_16/TensorArrayV2é
Jsequential_8/lstm_16/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2L
Jsequential_8/lstm_16/TensorArrayUnstack/TensorListFromTensor/element_shapeÌ
<sequential_8/lstm_16/TensorArrayUnstack/TensorListFromTensorTensorListFromTensor"sequential_8/lstm_16/transpose:y:0Ssequential_8/lstm_16/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02>
<sequential_8/lstm_16/TensorArrayUnstack/TensorListFromTensor¢
*sequential_8/lstm_16/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2,
*sequential_8/lstm_16/strided_slice_1/stack¦
,sequential_8/lstm_16/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_8/lstm_16/strided_slice_1/stack_1¦
,sequential_8/lstm_16/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_8/lstm_16/strided_slice_1/stack_2ñ
$sequential_8/lstm_16/strided_slice_1StridedSlice"sequential_8/lstm_16/transpose:y:03sequential_8/lstm_16/strided_slice_1/stack:output:05sequential_8/lstm_16/strided_slice_1/stack_1:output:05sequential_8/lstm_16/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2&
$sequential_8/lstm_16/strided_slice_1ô
7sequential_8/lstm_16/lstm_cell_16/MatMul/ReadVariableOpReadVariableOp@sequential_8_lstm_16_lstm_cell_16_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype029
7sequential_8/lstm_16/lstm_cell_16/MatMul/ReadVariableOpø
(sequential_8/lstm_16/lstm_cell_16/MatMulMatMul-sequential_8/lstm_16/strided_slice_1:output:0?sequential_8/lstm_16/lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2*
(sequential_8/lstm_16/lstm_cell_16/MatMulù
9sequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOpBsequential_8_lstm_16_lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02;
9sequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp
;sequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1ReadVariableOpDsequential_8_lstm_16_lstm_cell_16_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02=
;sequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1
*sequential_8/lstm_16/lstm_cell_16/MatMul_1MatMulAsequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp:value:0Csequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2,
*sequential_8/lstm_16/lstm_cell_16/MatMul_1ë
%sequential_8/lstm_16/lstm_cell_16/addAddV22sequential_8/lstm_16/lstm_cell_16/MatMul:product:04sequential_8/lstm_16/lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2'
%sequential_8/lstm_16/lstm_cell_16/addó
8sequential_8/lstm_16/lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOpAsequential_8_lstm_16_lstm_cell_16_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02:
8sequential_8/lstm_16/lstm_cell_16/BiasAdd/ReadVariableOpø
)sequential_8/lstm_16/lstm_cell_16/BiasAddBiasAdd)sequential_8/lstm_16/lstm_cell_16/add:z:0@sequential_8/lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2+
)sequential_8/lstm_16/lstm_cell_16/BiasAdd¨
1sequential_8/lstm_16/lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :23
1sequential_8/lstm_16/lstm_cell_16/split/split_dim£
'sequential_8/lstm_16/lstm_cell_16/splitSplit:sequential_8/lstm_16/lstm_cell_16/split/split_dim:output:02sequential_8/lstm_16/lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2)
'sequential_8/lstm_16/lstm_cell_16/split
'sequential_8/lstm_16/lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2)
'sequential_8/lstm_16/lstm_cell_16/Const
)sequential_8/lstm_16/lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2+
)sequential_8/lstm_16/lstm_cell_16/Const_1â
%sequential_8/lstm_16/lstm_cell_16/MulMul0sequential_8/lstm_16/lstm_cell_16/split:output:00sequential_8/lstm_16/lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222'
%sequential_8/lstm_16/lstm_cell_16/Mulã
'sequential_8/lstm_16/lstm_cell_16/Add_1AddV2)sequential_8/lstm_16/lstm_cell_16/Mul:z:02sequential_8/lstm_16/lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_16/lstm_cell_16/Add_1»
9sequential_8/lstm_16/lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2;
9sequential_8/lstm_16/lstm_cell_16/clip_by_value/Minimum/y
7sequential_8/lstm_16/lstm_cell_16/clip_by_value/MinimumMinimum+sequential_8/lstm_16/lstm_cell_16/Add_1:z:0Bsequential_8/lstm_16/lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:2229
7sequential_8/lstm_16/lstm_cell_16/clip_by_value/Minimum«
1sequential_8/lstm_16/lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    23
1sequential_8/lstm_16/lstm_cell_16/clip_by_value/y
/sequential_8/lstm_16/lstm_cell_16/clip_by_valueMaximum;sequential_8/lstm_16/lstm_cell_16/clip_by_value/Minimum:z:0:sequential_8/lstm_16/lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:2221
/sequential_8/lstm_16/lstm_cell_16/clip_by_value
)sequential_8/lstm_16/lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2+
)sequential_8/lstm_16/lstm_cell_16/Const_2
)sequential_8/lstm_16/lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2+
)sequential_8/lstm_16/lstm_cell_16/Const_3è
'sequential_8/lstm_16/lstm_cell_16/Mul_1Mul0sequential_8/lstm_16/lstm_cell_16/split:output:12sequential_8/lstm_16/lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_16/lstm_cell_16/Mul_1å
'sequential_8/lstm_16/lstm_cell_16/Add_2AddV2+sequential_8/lstm_16/lstm_cell_16/Mul_1:z:02sequential_8/lstm_16/lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_16/lstm_cell_16/Add_2¿
;sequential_8/lstm_16/lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2=
;sequential_8/lstm_16/lstm_cell_16/clip_by_value_1/Minimum/y
9sequential_8/lstm_16/lstm_cell_16/clip_by_value_1/MinimumMinimum+sequential_8/lstm_16/lstm_cell_16/Add_2:z:0Dsequential_8/lstm_16/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222;
9sequential_8/lstm_16/lstm_cell_16/clip_by_value_1/Minimum¯
3sequential_8/lstm_16/lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    25
3sequential_8/lstm_16/lstm_cell_16/clip_by_value_1/y
1sequential_8/lstm_16/lstm_cell_16/clip_by_value_1Maximum=sequential_8/lstm_16/lstm_cell_16/clip_by_value_1/Minimum:z:0<sequential_8/lstm_16/lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:2223
1sequential_8/lstm_16/lstm_cell_16/clip_by_value_1ð
6sequential_8/lstm_16/lstm_cell_16/mul_2/ReadVariableOpReadVariableOp?sequential_8_lstm_16_lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype028
6sequential_8/lstm_16/lstm_cell_16/mul_2/ReadVariableOpù
'sequential_8/lstm_16/lstm_cell_16/mul_2Mul5sequential_8/lstm_16/lstm_cell_16/clip_by_value_1:z:0>sequential_8/lstm_16/lstm_cell_16/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_16/lstm_cell_16/mul_2³
&sequential_8/lstm_16/lstm_cell_16/TanhTanh0sequential_8/lstm_16/lstm_cell_16/split:output:2*
T0*
_output_shapes

:222(
&sequential_8/lstm_16/lstm_cell_16/Tanhã
'sequential_8/lstm_16/lstm_cell_16/mul_3Mul3sequential_8/lstm_16/lstm_cell_16/clip_by_value:z:0*sequential_8/lstm_16/lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_16/lstm_cell_16/mul_3Þ
'sequential_8/lstm_16/lstm_cell_16/add_3AddV2+sequential_8/lstm_16/lstm_cell_16/mul_2:z:0+sequential_8/lstm_16/lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_16/lstm_cell_16/add_3
)sequential_8/lstm_16/lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2+
)sequential_8/lstm_16/lstm_cell_16/Const_4
)sequential_8/lstm_16/lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2+
)sequential_8/lstm_16/lstm_cell_16/Const_5è
'sequential_8/lstm_16/lstm_cell_16/Mul_4Mul0sequential_8/lstm_16/lstm_cell_16/split:output:32sequential_8/lstm_16/lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_16/lstm_cell_16/Mul_4å
'sequential_8/lstm_16/lstm_cell_16/Add_4AddV2+sequential_8/lstm_16/lstm_cell_16/Mul_4:z:02sequential_8/lstm_16/lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_16/lstm_cell_16/Add_4¿
;sequential_8/lstm_16/lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2=
;sequential_8/lstm_16/lstm_cell_16/clip_by_value_2/Minimum/y
9sequential_8/lstm_16/lstm_cell_16/clip_by_value_2/MinimumMinimum+sequential_8/lstm_16/lstm_cell_16/Add_4:z:0Dsequential_8/lstm_16/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222;
9sequential_8/lstm_16/lstm_cell_16/clip_by_value_2/Minimum¯
3sequential_8/lstm_16/lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    25
3sequential_8/lstm_16/lstm_cell_16/clip_by_value_2/y
1sequential_8/lstm_16/lstm_cell_16/clip_by_value_2Maximum=sequential_8/lstm_16/lstm_cell_16/clip_by_value_2/Minimum:z:0<sequential_8/lstm_16/lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:2223
1sequential_8/lstm_16/lstm_cell_16/clip_by_value_2²
(sequential_8/lstm_16/lstm_cell_16/Tanh_1Tanh+sequential_8/lstm_16/lstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222*
(sequential_8/lstm_16/lstm_cell_16/Tanh_1ç
'sequential_8/lstm_16/lstm_cell_16/mul_5Mul5sequential_8/lstm_16/lstm_cell_16/clip_by_value_2:z:0,sequential_8/lstm_16/lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222)
'sequential_8/lstm_16/lstm_cell_16/mul_5¹
2sequential_8/lstm_16/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   24
2sequential_8/lstm_16/TensorArrayV2_1/element_shape
$sequential_8/lstm_16/TensorArrayV2_1TensorListReserve;sequential_8/lstm_16/TensorArrayV2_1/element_shape:output:0+sequential_8/lstm_16/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02&
$sequential_8/lstm_16/TensorArrayV2_1x
sequential_8/lstm_16/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential_8/lstm_16/timeÍ
#sequential_8/lstm_16/ReadVariableOpReadVariableOpBsequential_8_lstm_16_lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#sequential_8/lstm_16/ReadVariableOpÎ
%sequential_8/lstm_16/ReadVariableOp_1ReadVariableOp?sequential_8_lstm_16_lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02'
%sequential_8/lstm_16/ReadVariableOp_1©
-sequential_8/lstm_16/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2/
-sequential_8/lstm_16/while/maximum_iterations
'sequential_8/lstm_16/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2)
'sequential_8/lstm_16/while/loop_counter°
sequential_8/lstm_16/whileWhile0sequential_8/lstm_16/while/loop_counter:output:06sequential_8/lstm_16/while/maximum_iterations:output:0"sequential_8/lstm_16/time:output:0-sequential_8/lstm_16/TensorArrayV2_1:handle:0+sequential_8/lstm_16/ReadVariableOp:value:0-sequential_8/lstm_16/ReadVariableOp_1:value:0+sequential_8/lstm_16/strided_slice:output:0Lsequential_8/lstm_16/TensorArrayUnstack/TensorListFromTensor:output_handle:0@sequential_8_lstm_16_lstm_cell_16_matmul_readvariableop_resourceDsequential_8_lstm_16_lstm_cell_16_matmul_1_readvariableop_1_resourceAsequential_8_lstm_16_lstm_cell_16_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *1
body)R'
%sequential_8_lstm_16_while_body_48252*1
cond)R'
%sequential_8_lstm_16_while_cond_48251*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
sequential_8/lstm_16/whileß
Esequential_8/lstm_16/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2G
Esequential_8/lstm_16/TensorArrayV2Stack/TensorListStack/element_shape³
7sequential_8/lstm_16/TensorArrayV2Stack/TensorListStackTensorListStack#sequential_8/lstm_16/while:output:3Nsequential_8/lstm_16/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
element_dtype029
7sequential_8/lstm_16/TensorArrayV2Stack/TensorListStack«
*sequential_8/lstm_16/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2,
*sequential_8/lstm_16/strided_slice_2/stack¦
,sequential_8/lstm_16/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2.
,sequential_8/lstm_16/strided_slice_2/stack_1¦
,sequential_8/lstm_16/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_8/lstm_16/strided_slice_2/stack_2
$sequential_8/lstm_16/strided_slice_2StridedSlice@sequential_8/lstm_16/TensorArrayV2Stack/TensorListStack:tensor:03sequential_8/lstm_16/strided_slice_2/stack:output:05sequential_8/lstm_16/strided_slice_2/stack_1:output:05sequential_8/lstm_16/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2&
$sequential_8/lstm_16/strided_slice_2£
%sequential_8/lstm_16/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2'
%sequential_8/lstm_16/transpose_1/permð
 sequential_8/lstm_16/transpose_1	Transpose@sequential_8/lstm_16/TensorArrayV2Stack/TensorListStack:tensor:0.sequential_8/lstm_16/transpose_1/perm:output:0*
T0*"
_output_shapes
:2
22"
 sequential_8/lstm_16/transpose_1
sequential_8/lstm_16/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential_8/lstm_16/runtimeÐ
%sequential_8/lstm_16/AssignVariableOpAssignVariableOpBsequential_8_lstm_16_lstm_cell_16_matmul_1_readvariableop_resource#sequential_8/lstm_16/while:output:4$^sequential_8/lstm_16/ReadVariableOp:^sequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02'
%sequential_8/lstm_16/AssignVariableOpÐ
'sequential_8/lstm_16/AssignVariableOp_1AssignVariableOp?sequential_8_lstm_16_lstm_cell_16_mul_2_readvariableop_resource#sequential_8/lstm_16/while:output:5&^sequential_8/lstm_16/ReadVariableOp_17^sequential_8/lstm_16/lstm_cell_16/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02)
'sequential_8/lstm_16/AssignVariableOp_1¯
-sequential_8/time_distributed_8/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2/
-sequential_8/time_distributed_8/Reshape/shapeå
'sequential_8/time_distributed_8/ReshapeReshape$sequential_8/lstm_16/transpose_1:y:06sequential_8/time_distributed_8/Reshape/shape:output:0*
T0*
_output_shapes
:	ô22)
'sequential_8/time_distributed_8/Reshape
=sequential_8/time_distributed_8/dense_8/MatMul/ReadVariableOpReadVariableOpFsequential_8_time_distributed_8_dense_8_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02?
=sequential_8/time_distributed_8/dense_8/MatMul/ReadVariableOp
.sequential_8/time_distributed_8/dense_8/MatMulMatMul0sequential_8/time_distributed_8/Reshape:output:0Esequential_8/time_distributed_8/dense_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô20
.sequential_8/time_distributed_8/dense_8/MatMul
>sequential_8/time_distributed_8/dense_8/BiasAdd/ReadVariableOpReadVariableOpGsequential_8_time_distributed_8_dense_8_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02@
>sequential_8/time_distributed_8/dense_8/BiasAdd/ReadVariableOp
/sequential_8/time_distributed_8/dense_8/BiasAddBiasAdd8sequential_8/time_distributed_8/dense_8/MatMul:product:0Fsequential_8/time_distributed_8/dense_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô21
/sequential_8/time_distributed_8/dense_8/BiasAdd·
/sequential_8/time_distributed_8/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      21
/sequential_8/time_distributed_8/Reshape_1/shape
)sequential_8/time_distributed_8/Reshape_1Reshape8sequential_8/time_distributed_8/dense_8/BiasAdd:output:08sequential_8/time_distributed_8/Reshape_1/shape:output:0*
T0*"
_output_shapes
:2
2+
)sequential_8/time_distributed_8/Reshape_1³
/sequential_8/time_distributed_8/Reshape_2/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   21
/sequential_8/time_distributed_8/Reshape_2/shapeë
)sequential_8/time_distributed_8/Reshape_2Reshape$sequential_8/lstm_16/transpose_1:y:08sequential_8/time_distributed_8/Reshape_2/shape:output:0*
T0*
_output_shapes
:	ô22+
)sequential_8/time_distributed_8/Reshape_2
IdentityIdentity2sequential_8/time_distributed_8/Reshape_1:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity	
NoOpNoOp&^sequential_8/lstm_16/AssignVariableOp(^sequential_8/lstm_16/AssignVariableOp_1$^sequential_8/lstm_16/ReadVariableOp&^sequential_8/lstm_16/ReadVariableOp_19^sequential_8/lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp8^sequential_8/lstm_16/lstm_cell_16/MatMul/ReadVariableOp:^sequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp<^sequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_17^sequential_8/lstm_16/lstm_cell_16/mul_2/ReadVariableOp^sequential_8/lstm_16/while&^sequential_8/lstm_17/AssignVariableOp(^sequential_8/lstm_17/AssignVariableOp_1$^sequential_8/lstm_17/ReadVariableOp&^sequential_8/lstm_17/ReadVariableOp_19^sequential_8/lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp8^sequential_8/lstm_17/lstm_cell_17/MatMul/ReadVariableOp:^sequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp<^sequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_17^sequential_8/lstm_17/lstm_cell_17/mul_2/ReadVariableOp^sequential_8/lstm_17/while?^sequential_8/time_distributed_8/dense_8/BiasAdd/ReadVariableOp>^sequential_8/time_distributed_8/dense_8/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 2N
%sequential_8/lstm_16/AssignVariableOp%sequential_8/lstm_16/AssignVariableOp2R
'sequential_8/lstm_16/AssignVariableOp_1'sequential_8/lstm_16/AssignVariableOp_12J
#sequential_8/lstm_16/ReadVariableOp#sequential_8/lstm_16/ReadVariableOp2N
%sequential_8/lstm_16/ReadVariableOp_1%sequential_8/lstm_16/ReadVariableOp_12t
8sequential_8/lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp8sequential_8/lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp2r
7sequential_8/lstm_16/lstm_cell_16/MatMul/ReadVariableOp7sequential_8/lstm_16/lstm_cell_16/MatMul/ReadVariableOp2v
9sequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp9sequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp2z
;sequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1;sequential_8/lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_12p
6sequential_8/lstm_16/lstm_cell_16/mul_2/ReadVariableOp6sequential_8/lstm_16/lstm_cell_16/mul_2/ReadVariableOp28
sequential_8/lstm_16/whilesequential_8/lstm_16/while2N
%sequential_8/lstm_17/AssignVariableOp%sequential_8/lstm_17/AssignVariableOp2R
'sequential_8/lstm_17/AssignVariableOp_1'sequential_8/lstm_17/AssignVariableOp_12J
#sequential_8/lstm_17/ReadVariableOp#sequential_8/lstm_17/ReadVariableOp2N
%sequential_8/lstm_17/ReadVariableOp_1%sequential_8/lstm_17/ReadVariableOp_12t
8sequential_8/lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp8sequential_8/lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp2r
7sequential_8/lstm_17/lstm_cell_17/MatMul/ReadVariableOp7sequential_8/lstm_17/lstm_cell_17/MatMul/ReadVariableOp2v
9sequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp9sequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp2z
;sequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1;sequential_8/lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_12p
6sequential_8/lstm_17/lstm_cell_17/mul_2/ReadVariableOp6sequential_8/lstm_17/lstm_cell_17/mul_2/ReadVariableOp28
sequential_8/lstm_17/whilesequential_8/lstm_17/while2
>sequential_8/time_distributed_8/dense_8/BiasAdd/ReadVariableOp>sequential_8/time_distributed_8/dense_8/BiasAdd/ReadVariableOp2~
=sequential_8/time_distributed_8/dense_8/MatMul/ReadVariableOp=sequential_8/time_distributed_8/dense_8/MatMul/ReadVariableOp:Q M
"
_output_shapes
:2

'
_user_specified_namelstm_17_input
Î
ó
G__inference_sequential_8_layer_call_and_return_conditional_losses_51006

inputs 
lstm_17_50976:	È
lstm_17_50978:22 
lstm_17_50980:	2È
lstm_17_50982:	È
lstm_17_50984:22 
lstm_16_50987:	2È
lstm_16_50989:22 
lstm_16_50991:	2È
lstm_16_50993:	È
lstm_16_50995:22*
time_distributed_8_50998:2&
time_distributed_8_51000:
identity¢lstm_16/StatefulPartitionedCall¢lstm_17/StatefulPartitionedCall¢*time_distributed_8/StatefulPartitionedCall¸
lstm_17/StatefulPartitionedCallStatefulPartitionedCallinputslstm_17_50976lstm_17_50978lstm_17_50980lstm_17_50982lstm_17_50984*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_17_layer_call_and_return_conditional_losses_509292!
lstm_17/StatefulPartitionedCallÚ
lstm_16/StatefulPartitionedCallStatefulPartitionedCall(lstm_17/StatefulPartitionedCall:output:0lstm_16_50987lstm_16_50989lstm_16_50991lstm_16_50993lstm_16_50995*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_16_layer_call_and_return_conditional_losses_507212!
lstm_16/StatefulPartitionedCallà
*time_distributed_8/StatefulPartitionedCallStatefulPartitionedCall(lstm_16/StatefulPartitionedCall:output:0time_distributed_8_50998time_distributed_8_51000*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *V
fQRO
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_505192,
*time_distributed_8/StatefulPartitionedCall
 time_distributed_8/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2"
 time_distributed_8/Reshape/shapeÂ
time_distributed_8/ReshapeReshape(lstm_16/StatefulPartitionedCall:output:0)time_distributed_8/Reshape/shape:output:0*
T0*
_output_shapes
:	ô22
time_distributed_8/Reshape
IdentityIdentity3time_distributed_8/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity¿
NoOpNoOp ^lstm_16/StatefulPartitionedCall ^lstm_17/StatefulPartitionedCall+^time_distributed_8/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 2B
lstm_16/StatefulPartitionedCalllstm_16/StatefulPartitionedCall2B
lstm_17/StatefulPartitionedCalllstm_17/StatefulPartitionedCall2X
*time_distributed_8/StatefulPartitionedCall*time_distributed_8/StatefulPartitionedCall:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
í£
ù
!__inference__traced_restore_54679
file_prefix$
assignvariableop_adam_iter:	 (
assignvariableop_1_adam_beta_1: (
assignvariableop_2_adam_beta_2: '
assignvariableop_3_adam_decay: /
%assignvariableop_4_adam_learning_rate: A
.assignvariableop_5_lstm_17_lstm_cell_17_kernel:	ÈK
8assignvariableop_6_lstm_17_lstm_cell_17_recurrent_kernel:	2È;
,assignvariableop_7_lstm_17_lstm_cell_17_bias:	ÈA
.assignvariableop_8_lstm_16_lstm_cell_16_kernel:	2ÈK
8assignvariableop_9_lstm_16_lstm_cell_16_recurrent_kernel:	2È<
-assignvariableop_10_lstm_16_lstm_cell_16_bias:	È?
-assignvariableop_11_time_distributed_8_kernel:29
+assignvariableop_12_time_distributed_8_bias:6
$assignvariableop_13_lstm_17_variable:228
&assignvariableop_14_lstm_17_variable_1:226
$assignvariableop_15_lstm_16_variable:228
&assignvariableop_16_lstm_16_variable_1:22#
assignvariableop_17_total: #
assignvariableop_18_count: %
assignvariableop_19_total_1: %
assignvariableop_20_count_1: I
6assignvariableop_21_adam_lstm_17_lstm_cell_17_kernel_m:	ÈS
@assignvariableop_22_adam_lstm_17_lstm_cell_17_recurrent_kernel_m:	2ÈC
4assignvariableop_23_adam_lstm_17_lstm_cell_17_bias_m:	ÈI
6assignvariableop_24_adam_lstm_16_lstm_cell_16_kernel_m:	2ÈS
@assignvariableop_25_adam_lstm_16_lstm_cell_16_recurrent_kernel_m:	2ÈC
4assignvariableop_26_adam_lstm_16_lstm_cell_16_bias_m:	ÈF
4assignvariableop_27_adam_time_distributed_8_kernel_m:2@
2assignvariableop_28_adam_time_distributed_8_bias_m:I
6assignvariableop_29_adam_lstm_17_lstm_cell_17_kernel_v:	ÈS
@assignvariableop_30_adam_lstm_17_lstm_cell_17_recurrent_kernel_v:	2ÈC
4assignvariableop_31_adam_lstm_17_lstm_cell_17_bias_v:	ÈI
6assignvariableop_32_adam_lstm_16_lstm_cell_16_kernel_v:	2ÈS
@assignvariableop_33_adam_lstm_16_lstm_cell_16_recurrent_kernel_v:	2ÈC
4assignvariableop_34_adam_lstm_16_lstm_cell_16_bias_v:	ÈF
4assignvariableop_35_adam_time_distributed_8_kernel_v:2@
2assignvariableop_36_adam_time_distributed_8_bias_v:
identity_38¢AssignVariableOp¢AssignVariableOp_1¢AssignVariableOp_10¢AssignVariableOp_11¢AssignVariableOp_12¢AssignVariableOp_13¢AssignVariableOp_14¢AssignVariableOp_15¢AssignVariableOp_16¢AssignVariableOp_17¢AssignVariableOp_18¢AssignVariableOp_19¢AssignVariableOp_2¢AssignVariableOp_20¢AssignVariableOp_21¢AssignVariableOp_22¢AssignVariableOp_23¢AssignVariableOp_24¢AssignVariableOp_25¢AssignVariableOp_26¢AssignVariableOp_27¢AssignVariableOp_28¢AssignVariableOp_29¢AssignVariableOp_3¢AssignVariableOp_30¢AssignVariableOp_31¢AssignVariableOp_32¢AssignVariableOp_33¢AssignVariableOp_34¢AssignVariableOp_35¢AssignVariableOp_36¢AssignVariableOp_4¢AssignVariableOp_5¢AssignVariableOp_6¢AssignVariableOp_7¢AssignVariableOp_8¢AssignVariableOp_9ä
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:&*
dtype0*ð
valueæBã&B)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/0/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/1/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/2/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/3/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/4/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/5/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/6/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/7/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/0/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/1/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/2/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/3/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/4/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/5/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/6/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/7/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/0/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/1/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/2/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/3/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/4/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/5/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/6/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/7/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
RestoreV2/tensor_namesÚ
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:&*
dtype0*_
valueVBT&B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B 2
RestoreV2/shape_and_slicesì
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*®
_output_shapes
::::::::::::::::::::::::::::::::::::::*4
dtypes*
(2&	2
	RestoreV2g
IdentityIdentityRestoreV2:tensors:0"/device:CPU:0*
T0	*
_output_shapes
:2

Identity
AssignVariableOpAssignVariableOpassignvariableop_adam_iterIdentity:output:0"/device:CPU:0*
_output_shapes
 *
dtype0	2
AssignVariableOpk

Identity_1IdentityRestoreV2:tensors:1"/device:CPU:0*
T0*
_output_shapes
:2

Identity_1£
AssignVariableOp_1AssignVariableOpassignvariableop_1_adam_beta_1Identity_1:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_1k

Identity_2IdentityRestoreV2:tensors:2"/device:CPU:0*
T0*
_output_shapes
:2

Identity_2£
AssignVariableOp_2AssignVariableOpassignvariableop_2_adam_beta_2Identity_2:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_2k

Identity_3IdentityRestoreV2:tensors:3"/device:CPU:0*
T0*
_output_shapes
:2

Identity_3¢
AssignVariableOp_3AssignVariableOpassignvariableop_3_adam_decayIdentity_3:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_3k

Identity_4IdentityRestoreV2:tensors:4"/device:CPU:0*
T0*
_output_shapes
:2

Identity_4ª
AssignVariableOp_4AssignVariableOp%assignvariableop_4_adam_learning_rateIdentity_4:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_4k

Identity_5IdentityRestoreV2:tensors:5"/device:CPU:0*
T0*
_output_shapes
:2

Identity_5³
AssignVariableOp_5AssignVariableOp.assignvariableop_5_lstm_17_lstm_cell_17_kernelIdentity_5:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_5k

Identity_6IdentityRestoreV2:tensors:6"/device:CPU:0*
T0*
_output_shapes
:2

Identity_6½
AssignVariableOp_6AssignVariableOp8assignvariableop_6_lstm_17_lstm_cell_17_recurrent_kernelIdentity_6:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_6k

Identity_7IdentityRestoreV2:tensors:7"/device:CPU:0*
T0*
_output_shapes
:2

Identity_7±
AssignVariableOp_7AssignVariableOp,assignvariableop_7_lstm_17_lstm_cell_17_biasIdentity_7:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_7k

Identity_8IdentityRestoreV2:tensors:8"/device:CPU:0*
T0*
_output_shapes
:2

Identity_8³
AssignVariableOp_8AssignVariableOp.assignvariableop_8_lstm_16_lstm_cell_16_kernelIdentity_8:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_8k

Identity_9IdentityRestoreV2:tensors:9"/device:CPU:0*
T0*
_output_shapes
:2

Identity_9½
AssignVariableOp_9AssignVariableOp8assignvariableop_9_lstm_16_lstm_cell_16_recurrent_kernelIdentity_9:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_9n
Identity_10IdentityRestoreV2:tensors:10"/device:CPU:0*
T0*
_output_shapes
:2
Identity_10µ
AssignVariableOp_10AssignVariableOp-assignvariableop_10_lstm_16_lstm_cell_16_biasIdentity_10:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_10n
Identity_11IdentityRestoreV2:tensors:11"/device:CPU:0*
T0*
_output_shapes
:2
Identity_11µ
AssignVariableOp_11AssignVariableOp-assignvariableop_11_time_distributed_8_kernelIdentity_11:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_11n
Identity_12IdentityRestoreV2:tensors:12"/device:CPU:0*
T0*
_output_shapes
:2
Identity_12³
AssignVariableOp_12AssignVariableOp+assignvariableop_12_time_distributed_8_biasIdentity_12:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_12n
Identity_13IdentityRestoreV2:tensors:13"/device:CPU:0*
T0*
_output_shapes
:2
Identity_13¬
AssignVariableOp_13AssignVariableOp$assignvariableop_13_lstm_17_variableIdentity_13:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_13n
Identity_14IdentityRestoreV2:tensors:14"/device:CPU:0*
T0*
_output_shapes
:2
Identity_14®
AssignVariableOp_14AssignVariableOp&assignvariableop_14_lstm_17_variable_1Identity_14:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_14n
Identity_15IdentityRestoreV2:tensors:15"/device:CPU:0*
T0*
_output_shapes
:2
Identity_15¬
AssignVariableOp_15AssignVariableOp$assignvariableop_15_lstm_16_variableIdentity_15:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_15n
Identity_16IdentityRestoreV2:tensors:16"/device:CPU:0*
T0*
_output_shapes
:2
Identity_16®
AssignVariableOp_16AssignVariableOp&assignvariableop_16_lstm_16_variable_1Identity_16:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_16n
Identity_17IdentityRestoreV2:tensors:17"/device:CPU:0*
T0*
_output_shapes
:2
Identity_17¡
AssignVariableOp_17AssignVariableOpassignvariableop_17_totalIdentity_17:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_17n
Identity_18IdentityRestoreV2:tensors:18"/device:CPU:0*
T0*
_output_shapes
:2
Identity_18¡
AssignVariableOp_18AssignVariableOpassignvariableop_18_countIdentity_18:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_18n
Identity_19IdentityRestoreV2:tensors:19"/device:CPU:0*
T0*
_output_shapes
:2
Identity_19£
AssignVariableOp_19AssignVariableOpassignvariableop_19_total_1Identity_19:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_19n
Identity_20IdentityRestoreV2:tensors:20"/device:CPU:0*
T0*
_output_shapes
:2
Identity_20£
AssignVariableOp_20AssignVariableOpassignvariableop_20_count_1Identity_20:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_20n
Identity_21IdentityRestoreV2:tensors:21"/device:CPU:0*
T0*
_output_shapes
:2
Identity_21¾
AssignVariableOp_21AssignVariableOp6assignvariableop_21_adam_lstm_17_lstm_cell_17_kernel_mIdentity_21:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_21n
Identity_22IdentityRestoreV2:tensors:22"/device:CPU:0*
T0*
_output_shapes
:2
Identity_22È
AssignVariableOp_22AssignVariableOp@assignvariableop_22_adam_lstm_17_lstm_cell_17_recurrent_kernel_mIdentity_22:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_22n
Identity_23IdentityRestoreV2:tensors:23"/device:CPU:0*
T0*
_output_shapes
:2
Identity_23¼
AssignVariableOp_23AssignVariableOp4assignvariableop_23_adam_lstm_17_lstm_cell_17_bias_mIdentity_23:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_23n
Identity_24IdentityRestoreV2:tensors:24"/device:CPU:0*
T0*
_output_shapes
:2
Identity_24¾
AssignVariableOp_24AssignVariableOp6assignvariableop_24_adam_lstm_16_lstm_cell_16_kernel_mIdentity_24:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_24n
Identity_25IdentityRestoreV2:tensors:25"/device:CPU:0*
T0*
_output_shapes
:2
Identity_25È
AssignVariableOp_25AssignVariableOp@assignvariableop_25_adam_lstm_16_lstm_cell_16_recurrent_kernel_mIdentity_25:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_25n
Identity_26IdentityRestoreV2:tensors:26"/device:CPU:0*
T0*
_output_shapes
:2
Identity_26¼
AssignVariableOp_26AssignVariableOp4assignvariableop_26_adam_lstm_16_lstm_cell_16_bias_mIdentity_26:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_26n
Identity_27IdentityRestoreV2:tensors:27"/device:CPU:0*
T0*
_output_shapes
:2
Identity_27¼
AssignVariableOp_27AssignVariableOp4assignvariableop_27_adam_time_distributed_8_kernel_mIdentity_27:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_27n
Identity_28IdentityRestoreV2:tensors:28"/device:CPU:0*
T0*
_output_shapes
:2
Identity_28º
AssignVariableOp_28AssignVariableOp2assignvariableop_28_adam_time_distributed_8_bias_mIdentity_28:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_28n
Identity_29IdentityRestoreV2:tensors:29"/device:CPU:0*
T0*
_output_shapes
:2
Identity_29¾
AssignVariableOp_29AssignVariableOp6assignvariableop_29_adam_lstm_17_lstm_cell_17_kernel_vIdentity_29:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_29n
Identity_30IdentityRestoreV2:tensors:30"/device:CPU:0*
T0*
_output_shapes
:2
Identity_30È
AssignVariableOp_30AssignVariableOp@assignvariableop_30_adam_lstm_17_lstm_cell_17_recurrent_kernel_vIdentity_30:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_30n
Identity_31IdentityRestoreV2:tensors:31"/device:CPU:0*
T0*
_output_shapes
:2
Identity_31¼
AssignVariableOp_31AssignVariableOp4assignvariableop_31_adam_lstm_17_lstm_cell_17_bias_vIdentity_31:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_31n
Identity_32IdentityRestoreV2:tensors:32"/device:CPU:0*
T0*
_output_shapes
:2
Identity_32¾
AssignVariableOp_32AssignVariableOp6assignvariableop_32_adam_lstm_16_lstm_cell_16_kernel_vIdentity_32:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_32n
Identity_33IdentityRestoreV2:tensors:33"/device:CPU:0*
T0*
_output_shapes
:2
Identity_33È
AssignVariableOp_33AssignVariableOp@assignvariableop_33_adam_lstm_16_lstm_cell_16_recurrent_kernel_vIdentity_33:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_33n
Identity_34IdentityRestoreV2:tensors:34"/device:CPU:0*
T0*
_output_shapes
:2
Identity_34¼
AssignVariableOp_34AssignVariableOp4assignvariableop_34_adam_lstm_16_lstm_cell_16_bias_vIdentity_34:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_34n
Identity_35IdentityRestoreV2:tensors:35"/device:CPU:0*
T0*
_output_shapes
:2
Identity_35¼
AssignVariableOp_35AssignVariableOp4assignvariableop_35_adam_time_distributed_8_kernel_vIdentity_35:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_35n
Identity_36IdentityRestoreV2:tensors:36"/device:CPU:0*
T0*
_output_shapes
:2
Identity_36º
AssignVariableOp_36AssignVariableOp2assignvariableop_36_adam_time_distributed_8_bias_vIdentity_36:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_369
NoOpNoOp"/device:CPU:0*
_output_shapes
 2
NoOp
Identity_37Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_23^AssignVariableOp_24^AssignVariableOp_25^AssignVariableOp_26^AssignVariableOp_27^AssignVariableOp_28^AssignVariableOp_29^AssignVariableOp_3^AssignVariableOp_30^AssignVariableOp_31^AssignVariableOp_32^AssignVariableOp_33^AssignVariableOp_34^AssignVariableOp_35^AssignVariableOp_36^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9^NoOp"/device:CPU:0*
T0*
_output_shapes
: 2
Identity_37f
Identity_38IdentityIdentity_37:output:0^NoOp_1*
T0*
_output_shapes
: 2
Identity_38ô
NoOp_1NoOp^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_23^AssignVariableOp_24^AssignVariableOp_25^AssignVariableOp_26^AssignVariableOp_27^AssignVariableOp_28^AssignVariableOp_29^AssignVariableOp_3^AssignVariableOp_30^AssignVariableOp_31^AssignVariableOp_32^AssignVariableOp_33^AssignVariableOp_34^AssignVariableOp_35^AssignVariableOp_36^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9*"
_acd_function_control_output(*
_output_shapes
 2
NoOp_1"#
identity_38Identity_38:output:0*_
_input_shapesN
L: : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12*
AssignVariableOp_10AssignVariableOp_102*
AssignVariableOp_11AssignVariableOp_112*
AssignVariableOp_12AssignVariableOp_122*
AssignVariableOp_13AssignVariableOp_132*
AssignVariableOp_14AssignVariableOp_142*
AssignVariableOp_15AssignVariableOp_152*
AssignVariableOp_16AssignVariableOp_162*
AssignVariableOp_17AssignVariableOp_172*
AssignVariableOp_18AssignVariableOp_182*
AssignVariableOp_19AssignVariableOp_192(
AssignVariableOp_2AssignVariableOp_22*
AssignVariableOp_20AssignVariableOp_202*
AssignVariableOp_21AssignVariableOp_212*
AssignVariableOp_22AssignVariableOp_222*
AssignVariableOp_23AssignVariableOp_232*
AssignVariableOp_24AssignVariableOp_242*
AssignVariableOp_25AssignVariableOp_252*
AssignVariableOp_26AssignVariableOp_262*
AssignVariableOp_27AssignVariableOp_272*
AssignVariableOp_28AssignVariableOp_282*
AssignVariableOp_29AssignVariableOp_292(
AssignVariableOp_3AssignVariableOp_32*
AssignVariableOp_30AssignVariableOp_302*
AssignVariableOp_31AssignVariableOp_312*
AssignVariableOp_32AssignVariableOp_322*
AssignVariableOp_33AssignVariableOp_332*
AssignVariableOp_34AssignVariableOp_342*
AssignVariableOp_35AssignVariableOp_352*
AssignVariableOp_36AssignVariableOp_362(
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
¨
¼
while_cond_50615
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_50615___redundant_placeholder03
/while_while_cond_50615___redundant_placeholder13
/while_while_cond_50615___redundant_placeholder23
/while_while_cond_50615___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
¨
¼
while_cond_52795
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_52795___redundant_placeholder03
/while_while_cond_52795___redundant_placeholder13
/while_while_cond_52795___redundant_placeholder23
/while_while_cond_52795___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:


Ü
lstm_16_while_cond_51411,
(lstm_16_while_lstm_16_while_loop_counter2
.lstm_16_while_lstm_16_while_maximum_iterations
lstm_16_while_placeholder
lstm_16_while_placeholder_1
lstm_16_while_placeholder_2
lstm_16_while_placeholder_3,
(lstm_16_while_less_lstm_16_strided_sliceC
?lstm_16_while_lstm_16_while_cond_51411___redundant_placeholder0C
?lstm_16_while_lstm_16_while_cond_51411___redundant_placeholder1C
?lstm_16_while_lstm_16_while_cond_51411___redundant_placeholder2C
?lstm_16_while_lstm_16_while_cond_51411___redundant_placeholder3
lstm_16_while_identity

lstm_16/while/LessLesslstm_16_while_placeholder(lstm_16_while_less_lstm_16_strided_slice*
T0*
_output_shapes
: 2
lstm_16/while/Lessu
lstm_16/while/IdentityIdentitylstm_16/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_16/while/Identity"9
lstm_16_while_identitylstm_16/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
¨
¼
while_cond_48465
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_48465___redundant_placeholder03
/while_while_cond_48465___redundant_placeholder13
/while_while_cond_48465___redundant_placeholder23
/while_while_cond_48465___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
á
ú
G__inference_sequential_8_layer_call_and_return_conditional_losses_51893

inputsF
3lstm_17_lstm_cell_17_matmul_readvariableop_resource:	ÈG
5lstm_17_lstm_cell_17_matmul_1_readvariableop_resource:22J
7lstm_17_lstm_cell_17_matmul_1_readvariableop_1_resource:	2ÈC
4lstm_17_lstm_cell_17_biasadd_readvariableop_resource:	ÈD
2lstm_17_lstm_cell_17_mul_2_readvariableop_resource:22F
3lstm_16_lstm_cell_16_matmul_readvariableop_resource:	2ÈG
5lstm_16_lstm_cell_16_matmul_1_readvariableop_resource:22J
7lstm_16_lstm_cell_16_matmul_1_readvariableop_1_resource:	2ÈC
4lstm_16_lstm_cell_16_biasadd_readvariableop_resource:	ÈD
2lstm_16_lstm_cell_16_mul_2_readvariableop_resource:22K
9time_distributed_8_dense_8_matmul_readvariableop_resource:2H
:time_distributed_8_dense_8_biasadd_readvariableop_resource:
identity¢lstm_16/AssignVariableOp¢lstm_16/AssignVariableOp_1¢lstm_16/ReadVariableOp¢lstm_16/ReadVariableOp_1¢+lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp¢*lstm_16/lstm_cell_16/MatMul/ReadVariableOp¢,lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp¢.lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1¢)lstm_16/lstm_cell_16/mul_2/ReadVariableOp¢lstm_16/while¢lstm_17/AssignVariableOp¢lstm_17/AssignVariableOp_1¢lstm_17/ReadVariableOp¢lstm_17/ReadVariableOp_1¢+lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp¢*lstm_17/lstm_cell_17/MatMul/ReadVariableOp¢,lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp¢.lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1¢)lstm_17/lstm_cell_17/mul_2/ReadVariableOp¢lstm_17/while¢1time_distributed_8/dense_8/BiasAdd/ReadVariableOp¢0time_distributed_8/dense_8/MatMul/ReadVariableOp
lstm_17/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_17/transpose/perm
lstm_17/transpose	Transposeinputslstm_17/transpose/perm:output:0*
T0*"
_output_shapes
:
22
lstm_17/transposes
lstm_17/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2      2
lstm_17/Shape
lstm_17/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_17/strided_slice/stack
lstm_17/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_17/strided_slice/stack_1
lstm_17/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_17/strided_slice/stack_2
lstm_17/strided_sliceStridedSlicelstm_17/Shape:output:0$lstm_17/strided_slice/stack:output:0&lstm_17/strided_slice/stack_1:output:0&lstm_17/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_17/strided_slice
#lstm_17/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2%
#lstm_17/TensorArrayV2/element_shapeÐ
lstm_17/TensorArrayV2TensorListReserve,lstm_17/TensorArrayV2/element_shape:output:0lstm_17/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_17/TensorArrayV2Ï
=lstm_17/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      2?
=lstm_17/TensorArrayUnstack/TensorListFromTensor/element_shape
/lstm_17/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_17/transpose:y:0Flstm_17/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type021
/lstm_17/TensorArrayUnstack/TensorListFromTensor
lstm_17/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_17/strided_slice_1/stack
lstm_17/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_17/strided_slice_1/stack_1
lstm_17/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_17/strided_slice_1/stack_2£
lstm_17/strided_slice_1StridedSlicelstm_17/transpose:y:0&lstm_17/strided_slice_1/stack:output:0(lstm_17/strided_slice_1/stack_1:output:0(lstm_17/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_17/strided_slice_1Í
*lstm_17/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3lstm_17_lstm_cell_17_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02,
*lstm_17/lstm_cell_17/MatMul/ReadVariableOpÄ
lstm_17/lstm_cell_17/MatMulMatMul lstm_17/strided_slice_1:output:02lstm_17/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_17/lstm_cell_17/MatMulÒ
,lstm_17/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5lstm_17_lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02.
,lstm_17/lstm_cell_17/MatMul_1/ReadVariableOpÙ
.lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1ReadVariableOp7lstm_17_lstm_cell_17_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype020
.lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1à
lstm_17/lstm_cell_17/MatMul_1MatMul4lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp:value:06lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_17/lstm_cell_17/MatMul_1·
lstm_17/lstm_cell_17/addAddV2%lstm_17/lstm_cell_17/MatMul:product:0'lstm_17/lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_17/lstm_cell_17/addÌ
+lstm_17/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4lstm_17_lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02-
+lstm_17/lstm_cell_17/BiasAdd/ReadVariableOpÄ
lstm_17/lstm_cell_17/BiasAddBiasAddlstm_17/lstm_cell_17/add:z:03lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_17/lstm_cell_17/BiasAdd
$lstm_17/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2&
$lstm_17/lstm_cell_17/split/split_dimï
lstm_17/lstm_cell_17/splitSplit-lstm_17/lstm_cell_17/split/split_dim:output:0%lstm_17/lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_17/lstm_cell_17/split}
lstm_17/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_17/lstm_cell_17/Const
lstm_17/lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_17/lstm_cell_17/Const_1®
lstm_17/lstm_cell_17/MulMul#lstm_17/lstm_cell_17/split:output:0#lstm_17/lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Mul¯
lstm_17/lstm_cell_17/Add_1AddV2lstm_17/lstm_cell_17/Mul:z:0%lstm_17/lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Add_1¡
,lstm_17/lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,lstm_17/lstm_cell_17/clip_by_value/Minimum/yã
*lstm_17/lstm_cell_17/clip_by_value/MinimumMinimumlstm_17/lstm_cell_17/Add_1:z:05lstm_17/lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222,
*lstm_17/lstm_cell_17/clip_by_value/Minimum
$lstm_17/lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_17/lstm_cell_17/clip_by_value/yÛ
"lstm_17/lstm_cell_17/clip_by_valueMaximum.lstm_17/lstm_cell_17/clip_by_value/Minimum:z:0-lstm_17/lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222$
"lstm_17/lstm_cell_17/clip_by_value
lstm_17/lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_17/lstm_cell_17/Const_2
lstm_17/lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_17/lstm_cell_17/Const_3´
lstm_17/lstm_cell_17/Mul_1Mul#lstm_17/lstm_cell_17/split:output:1%lstm_17/lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Mul_1±
lstm_17/lstm_cell_17/Add_2AddV2lstm_17/lstm_cell_17/Mul_1:z:0%lstm_17/lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Add_2¥
.lstm_17/lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_17/lstm_cell_17/clip_by_value_1/Minimum/yé
,lstm_17/lstm_cell_17/clip_by_value_1/MinimumMinimumlstm_17/lstm_cell_17/Add_2:z:07lstm_17/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222.
,lstm_17/lstm_cell_17/clip_by_value_1/Minimum
&lstm_17/lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_17/lstm_cell_17/clip_by_value_1/yã
$lstm_17/lstm_cell_17/clip_by_value_1Maximum0lstm_17/lstm_cell_17/clip_by_value_1/Minimum:z:0/lstm_17/lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222&
$lstm_17/lstm_cell_17/clip_by_value_1É
)lstm_17/lstm_cell_17/mul_2/ReadVariableOpReadVariableOp2lstm_17_lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02+
)lstm_17/lstm_cell_17/mul_2/ReadVariableOpÅ
lstm_17/lstm_cell_17/mul_2Mul(lstm_17/lstm_cell_17/clip_by_value_1:z:01lstm_17/lstm_cell_17/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/mul_2
lstm_17/lstm_cell_17/TanhTanh#lstm_17/lstm_cell_17/split:output:2*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Tanh¯
lstm_17/lstm_cell_17/mul_3Mul&lstm_17/lstm_cell_17/clip_by_value:z:0lstm_17/lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/mul_3ª
lstm_17/lstm_cell_17/add_3AddV2lstm_17/lstm_cell_17/mul_2:z:0lstm_17/lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/add_3
lstm_17/lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_17/lstm_cell_17/Const_4
lstm_17/lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_17/lstm_cell_17/Const_5´
lstm_17/lstm_cell_17/Mul_4Mul#lstm_17/lstm_cell_17/split:output:3%lstm_17/lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Mul_4±
lstm_17/lstm_cell_17/Add_4AddV2lstm_17/lstm_cell_17/Mul_4:z:0%lstm_17/lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Add_4¥
.lstm_17/lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_17/lstm_cell_17/clip_by_value_2/Minimum/yé
,lstm_17/lstm_cell_17/clip_by_value_2/MinimumMinimumlstm_17/lstm_cell_17/Add_4:z:07lstm_17/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222.
,lstm_17/lstm_cell_17/clip_by_value_2/Minimum
&lstm_17/lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_17/lstm_cell_17/clip_by_value_2/yã
$lstm_17/lstm_cell_17/clip_by_value_2Maximum0lstm_17/lstm_cell_17/clip_by_value_2/Minimum:z:0/lstm_17/lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222&
$lstm_17/lstm_cell_17/clip_by_value_2
lstm_17/lstm_cell_17/Tanh_1Tanhlstm_17/lstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Tanh_1³
lstm_17/lstm_cell_17/mul_5Mul(lstm_17/lstm_cell_17/clip_by_value_2:z:0lstm_17/lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/mul_5
%lstm_17/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2'
%lstm_17/TensorArrayV2_1/element_shapeÖ
lstm_17/TensorArrayV2_1TensorListReserve.lstm_17/TensorArrayV2_1/element_shape:output:0lstm_17/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_17/TensorArrayV2_1^
lstm_17/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_17/time¦
lstm_17/ReadVariableOpReadVariableOp5lstm_17_lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_17/ReadVariableOp§
lstm_17/ReadVariableOp_1ReadVariableOp2lstm_17_lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_17/ReadVariableOp_1
 lstm_17/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2"
 lstm_17/while/maximum_iterationsz
lstm_17/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_17/while/loop_counterí
lstm_17/whileWhile#lstm_17/while/loop_counter:output:0)lstm_17/while/maximum_iterations:output:0lstm_17/time:output:0 lstm_17/TensorArrayV2_1:handle:0lstm_17/ReadVariableOp:value:0 lstm_17/ReadVariableOp_1:value:0lstm_17/strided_slice:output:0?lstm_17/TensorArrayUnstack/TensorListFromTensor:output_handle:03lstm_17_lstm_cell_17_matmul_readvariableop_resource7lstm_17_lstm_cell_17_matmul_1_readvariableop_1_resource4lstm_17_lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *$
bodyR
lstm_17_while_body_51602*$
condR
lstm_17_while_cond_51601*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
lstm_17/whileÅ
8lstm_17/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2:
8lstm_17/TensorArrayV2Stack/TensorListStack/element_shapeÿ
*lstm_17/TensorArrayV2Stack/TensorListStackTensorListStacklstm_17/while:output:3Alstm_17/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
element_dtype02,
*lstm_17/TensorArrayV2Stack/TensorListStack
lstm_17/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
lstm_17/strided_slice_2/stack
lstm_17/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2!
lstm_17/strided_slice_2/stack_1
lstm_17/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_17/strided_slice_2/stack_2Á
lstm_17/strided_slice_2StridedSlice3lstm_17/TensorArrayV2Stack/TensorListStack:tensor:0&lstm_17/strided_slice_2/stack:output:0(lstm_17/strided_slice_2/stack_1:output:0(lstm_17/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2
lstm_17/strided_slice_2
lstm_17/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_17/transpose_1/perm¼
lstm_17/transpose_1	Transpose3lstm_17/TensorArrayV2Stack/TensorListStack:tensor:0!lstm_17/transpose_1/perm:output:0*
T0*"
_output_shapes
:2
22
lstm_17/transpose_1v
lstm_17/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_17/runtime
lstm_17/AssignVariableOpAssignVariableOp5lstm_17_lstm_cell_17_matmul_1_readvariableop_resourcelstm_17/while:output:4^lstm_17/ReadVariableOp-^lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_17/AssignVariableOp
lstm_17/AssignVariableOp_1AssignVariableOp2lstm_17_lstm_cell_17_mul_2_readvariableop_resourcelstm_17/while:output:5^lstm_17/ReadVariableOp_1*^lstm_17/lstm_cell_17/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_17/AssignVariableOp_1
lstm_16/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_16/transpose/perm
lstm_16/transpose	Transposelstm_17/transpose_1:y:0lstm_16/transpose/perm:output:0*
T0*"
_output_shapes
:
222
lstm_16/transposes
lstm_16/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2   2   2
lstm_16/Shape
lstm_16/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_16/strided_slice/stack
lstm_16/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_16/strided_slice/stack_1
lstm_16/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_16/strided_slice/stack_2
lstm_16/strided_sliceStridedSlicelstm_16/Shape:output:0$lstm_16/strided_slice/stack:output:0&lstm_16/strided_slice/stack_1:output:0&lstm_16/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_16/strided_slice
#lstm_16/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2%
#lstm_16/TensorArrayV2/element_shapeÐ
lstm_16/TensorArrayV2TensorListReserve,lstm_16/TensorArrayV2/element_shape:output:0lstm_16/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_16/TensorArrayV2Ï
=lstm_16/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2?
=lstm_16/TensorArrayUnstack/TensorListFromTensor/element_shape
/lstm_16/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_16/transpose:y:0Flstm_16/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type021
/lstm_16/TensorArrayUnstack/TensorListFromTensor
lstm_16/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_16/strided_slice_1/stack
lstm_16/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_16/strided_slice_1/stack_1
lstm_16/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_16/strided_slice_1/stack_2£
lstm_16/strided_slice_1StridedSlicelstm_16/transpose:y:0&lstm_16/strided_slice_1/stack:output:0(lstm_16/strided_slice_1/stack_1:output:0(lstm_16/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2
lstm_16/strided_slice_1Í
*lstm_16/lstm_cell_16/MatMul/ReadVariableOpReadVariableOp3lstm_16_lstm_cell_16_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02,
*lstm_16/lstm_cell_16/MatMul/ReadVariableOpÄ
lstm_16/lstm_cell_16/MatMulMatMul lstm_16/strided_slice_1:output:02lstm_16/lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_16/lstm_cell_16/MatMulÒ
,lstm_16/lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp5lstm_16_lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02.
,lstm_16/lstm_cell_16/MatMul_1/ReadVariableOpÙ
.lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1ReadVariableOp7lstm_16_lstm_cell_16_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype020
.lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1à
lstm_16/lstm_cell_16/MatMul_1MatMul4lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp:value:06lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_16/lstm_cell_16/MatMul_1·
lstm_16/lstm_cell_16/addAddV2%lstm_16/lstm_cell_16/MatMul:product:0'lstm_16/lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_16/lstm_cell_16/addÌ
+lstm_16/lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp4lstm_16_lstm_cell_16_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02-
+lstm_16/lstm_cell_16/BiasAdd/ReadVariableOpÄ
lstm_16/lstm_cell_16/BiasAddBiasAddlstm_16/lstm_cell_16/add:z:03lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_16/lstm_cell_16/BiasAdd
$lstm_16/lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2&
$lstm_16/lstm_cell_16/split/split_dimï
lstm_16/lstm_cell_16/splitSplit-lstm_16/lstm_cell_16/split/split_dim:output:0%lstm_16/lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_16/lstm_cell_16/split}
lstm_16/lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_16/lstm_cell_16/Const
lstm_16/lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_16/lstm_cell_16/Const_1®
lstm_16/lstm_cell_16/MulMul#lstm_16/lstm_cell_16/split:output:0#lstm_16/lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Mul¯
lstm_16/lstm_cell_16/Add_1AddV2lstm_16/lstm_cell_16/Mul:z:0%lstm_16/lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Add_1¡
,lstm_16/lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,lstm_16/lstm_cell_16/clip_by_value/Minimum/yã
*lstm_16/lstm_cell_16/clip_by_value/MinimumMinimumlstm_16/lstm_cell_16/Add_1:z:05lstm_16/lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222,
*lstm_16/lstm_cell_16/clip_by_value/Minimum
$lstm_16/lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_16/lstm_cell_16/clip_by_value/yÛ
"lstm_16/lstm_cell_16/clip_by_valueMaximum.lstm_16/lstm_cell_16/clip_by_value/Minimum:z:0-lstm_16/lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222$
"lstm_16/lstm_cell_16/clip_by_value
lstm_16/lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_16/lstm_cell_16/Const_2
lstm_16/lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_16/lstm_cell_16/Const_3´
lstm_16/lstm_cell_16/Mul_1Mul#lstm_16/lstm_cell_16/split:output:1%lstm_16/lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Mul_1±
lstm_16/lstm_cell_16/Add_2AddV2lstm_16/lstm_cell_16/Mul_1:z:0%lstm_16/lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Add_2¥
.lstm_16/lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_16/lstm_cell_16/clip_by_value_1/Minimum/yé
,lstm_16/lstm_cell_16/clip_by_value_1/MinimumMinimumlstm_16/lstm_cell_16/Add_2:z:07lstm_16/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222.
,lstm_16/lstm_cell_16/clip_by_value_1/Minimum
&lstm_16/lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_16/lstm_cell_16/clip_by_value_1/yã
$lstm_16/lstm_cell_16/clip_by_value_1Maximum0lstm_16/lstm_cell_16/clip_by_value_1/Minimum:z:0/lstm_16/lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222&
$lstm_16/lstm_cell_16/clip_by_value_1É
)lstm_16/lstm_cell_16/mul_2/ReadVariableOpReadVariableOp2lstm_16_lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02+
)lstm_16/lstm_cell_16/mul_2/ReadVariableOpÅ
lstm_16/lstm_cell_16/mul_2Mul(lstm_16/lstm_cell_16/clip_by_value_1:z:01lstm_16/lstm_cell_16/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/mul_2
lstm_16/lstm_cell_16/TanhTanh#lstm_16/lstm_cell_16/split:output:2*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Tanh¯
lstm_16/lstm_cell_16/mul_3Mul&lstm_16/lstm_cell_16/clip_by_value:z:0lstm_16/lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/mul_3ª
lstm_16/lstm_cell_16/add_3AddV2lstm_16/lstm_cell_16/mul_2:z:0lstm_16/lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/add_3
lstm_16/lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_16/lstm_cell_16/Const_4
lstm_16/lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_16/lstm_cell_16/Const_5´
lstm_16/lstm_cell_16/Mul_4Mul#lstm_16/lstm_cell_16/split:output:3%lstm_16/lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Mul_4±
lstm_16/lstm_cell_16/Add_4AddV2lstm_16/lstm_cell_16/Mul_4:z:0%lstm_16/lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Add_4¥
.lstm_16/lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_16/lstm_cell_16/clip_by_value_2/Minimum/yé
,lstm_16/lstm_cell_16/clip_by_value_2/MinimumMinimumlstm_16/lstm_cell_16/Add_4:z:07lstm_16/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222.
,lstm_16/lstm_cell_16/clip_by_value_2/Minimum
&lstm_16/lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_16/lstm_cell_16/clip_by_value_2/yã
$lstm_16/lstm_cell_16/clip_by_value_2Maximum0lstm_16/lstm_cell_16/clip_by_value_2/Minimum:z:0/lstm_16/lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222&
$lstm_16/lstm_cell_16/clip_by_value_2
lstm_16/lstm_cell_16/Tanh_1Tanhlstm_16/lstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Tanh_1³
lstm_16/lstm_cell_16/mul_5Mul(lstm_16/lstm_cell_16/clip_by_value_2:z:0lstm_16/lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/mul_5
%lstm_16/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2'
%lstm_16/TensorArrayV2_1/element_shapeÖ
lstm_16/TensorArrayV2_1TensorListReserve.lstm_16/TensorArrayV2_1/element_shape:output:0lstm_16/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_16/TensorArrayV2_1^
lstm_16/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_16/time¦
lstm_16/ReadVariableOpReadVariableOp5lstm_16_lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_16/ReadVariableOp§
lstm_16/ReadVariableOp_1ReadVariableOp2lstm_16_lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_16/ReadVariableOp_1
 lstm_16/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2"
 lstm_16/while/maximum_iterationsz
lstm_16/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_16/while/loop_counterí
lstm_16/whileWhile#lstm_16/while/loop_counter:output:0)lstm_16/while/maximum_iterations:output:0lstm_16/time:output:0 lstm_16/TensorArrayV2_1:handle:0lstm_16/ReadVariableOp:value:0 lstm_16/ReadVariableOp_1:value:0lstm_16/strided_slice:output:0?lstm_16/TensorArrayUnstack/TensorListFromTensor:output_handle:03lstm_16_lstm_cell_16_matmul_readvariableop_resource7lstm_16_lstm_cell_16_matmul_1_readvariableop_1_resource4lstm_16_lstm_cell_16_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *$
bodyR
lstm_16_while_body_51776*$
condR
lstm_16_while_cond_51775*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
lstm_16/whileÅ
8lstm_16/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2:
8lstm_16/TensorArrayV2Stack/TensorListStack/element_shapeÿ
*lstm_16/TensorArrayV2Stack/TensorListStackTensorListStacklstm_16/while:output:3Alstm_16/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
element_dtype02,
*lstm_16/TensorArrayV2Stack/TensorListStack
lstm_16/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
lstm_16/strided_slice_2/stack
lstm_16/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2!
lstm_16/strided_slice_2/stack_1
lstm_16/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_16/strided_slice_2/stack_2Á
lstm_16/strided_slice_2StridedSlice3lstm_16/TensorArrayV2Stack/TensorListStack:tensor:0&lstm_16/strided_slice_2/stack:output:0(lstm_16/strided_slice_2/stack_1:output:0(lstm_16/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2
lstm_16/strided_slice_2
lstm_16/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_16/transpose_1/perm¼
lstm_16/transpose_1	Transpose3lstm_16/TensorArrayV2Stack/TensorListStack:tensor:0!lstm_16/transpose_1/perm:output:0*
T0*"
_output_shapes
:2
22
lstm_16/transpose_1v
lstm_16/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_16/runtime
lstm_16/AssignVariableOpAssignVariableOp5lstm_16_lstm_cell_16_matmul_1_readvariableop_resourcelstm_16/while:output:4^lstm_16/ReadVariableOp-^lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_16/AssignVariableOp
lstm_16/AssignVariableOp_1AssignVariableOp2lstm_16_lstm_cell_16_mul_2_readvariableop_resourcelstm_16/while:output:5^lstm_16/ReadVariableOp_1*^lstm_16/lstm_cell_16/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_16/AssignVariableOp_1
 time_distributed_8/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2"
 time_distributed_8/Reshape/shape±
time_distributed_8/ReshapeReshapelstm_16/transpose_1:y:0)time_distributed_8/Reshape/shape:output:0*
T0*
_output_shapes
:	ô22
time_distributed_8/ReshapeÞ
0time_distributed_8/dense_8/MatMul/ReadVariableOpReadVariableOp9time_distributed_8_dense_8_matmul_readvariableop_resource*
_output_shapes

:2*
dtype022
0time_distributed_8/dense_8/MatMul/ReadVariableOpÙ
!time_distributed_8/dense_8/MatMulMatMul#time_distributed_8/Reshape:output:08time_distributed_8/dense_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô2#
!time_distributed_8/dense_8/MatMulÝ
1time_distributed_8/dense_8/BiasAdd/ReadVariableOpReadVariableOp:time_distributed_8_dense_8_biasadd_readvariableop_resource*
_output_shapes
:*
dtype023
1time_distributed_8/dense_8/BiasAdd/ReadVariableOpå
"time_distributed_8/dense_8/BiasAddBiasAdd+time_distributed_8/dense_8/MatMul:product:09time_distributed_8/dense_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô2$
"time_distributed_8/dense_8/BiasAdd
"time_distributed_8/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      2$
"time_distributed_8/Reshape_1/shapeÎ
time_distributed_8/Reshape_1Reshape+time_distributed_8/dense_8/BiasAdd:output:0+time_distributed_8/Reshape_1/shape:output:0*
T0*"
_output_shapes
:2
2
time_distributed_8/Reshape_1
"time_distributed_8/Reshape_2/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2$
"time_distributed_8/Reshape_2/shape·
time_distributed_8/Reshape_2Reshapelstm_16/transpose_1:y:0+time_distributed_8/Reshape_2/shape:output:0*
T0*
_output_shapes
:	ô22
time_distributed_8/Reshape_2{
IdentityIdentity%time_distributed_8/Reshape_1:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identityû
NoOpNoOp^lstm_16/AssignVariableOp^lstm_16/AssignVariableOp_1^lstm_16/ReadVariableOp^lstm_16/ReadVariableOp_1,^lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp+^lstm_16/lstm_cell_16/MatMul/ReadVariableOp-^lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp/^lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1*^lstm_16/lstm_cell_16/mul_2/ReadVariableOp^lstm_16/while^lstm_17/AssignVariableOp^lstm_17/AssignVariableOp_1^lstm_17/ReadVariableOp^lstm_17/ReadVariableOp_1,^lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp+^lstm_17/lstm_cell_17/MatMul/ReadVariableOp-^lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp/^lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1*^lstm_17/lstm_cell_17/mul_2/ReadVariableOp^lstm_17/while2^time_distributed_8/dense_8/BiasAdd/ReadVariableOp1^time_distributed_8/dense_8/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 24
lstm_16/AssignVariableOplstm_16/AssignVariableOp28
lstm_16/AssignVariableOp_1lstm_16/AssignVariableOp_120
lstm_16/ReadVariableOplstm_16/ReadVariableOp24
lstm_16/ReadVariableOp_1lstm_16/ReadVariableOp_12Z
+lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp+lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp2X
*lstm_16/lstm_cell_16/MatMul/ReadVariableOp*lstm_16/lstm_cell_16/MatMul/ReadVariableOp2\
,lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp,lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp2`
.lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1.lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_12V
)lstm_16/lstm_cell_16/mul_2/ReadVariableOp)lstm_16/lstm_cell_16/mul_2/ReadVariableOp2
lstm_16/whilelstm_16/while24
lstm_17/AssignVariableOplstm_17/AssignVariableOp28
lstm_17/AssignVariableOp_1lstm_17/AssignVariableOp_120
lstm_17/ReadVariableOplstm_17/ReadVariableOp24
lstm_17/ReadVariableOp_1lstm_17/ReadVariableOp_12Z
+lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp+lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp2X
*lstm_17/lstm_cell_17/MatMul/ReadVariableOp*lstm_17/lstm_cell_17/MatMul/ReadVariableOp2\
,lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp,lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp2`
.lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1.lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_12V
)lstm_17/lstm_cell_17/mul_2/ReadVariableOp)lstm_17/lstm_cell_17/mul_2/ReadVariableOp2
lstm_17/whilelstm_17/while2f
1time_distributed_8/dense_8/BiasAdd/ReadVariableOp1time_distributed_8/dense_8/BiasAdd/ReadVariableOp2d
0time_distributed_8/dense_8/MatMul/ReadVariableOp0time_distributed_8/dense_8/MatMul/ReadVariableOp:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
ï,

G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_48676

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
:	2È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2	
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
(:22:22:22:22*
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

:222
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
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

:222
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:222
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:222
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:222
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:222
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

:222
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:222
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:222
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:222

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
$:2:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:FB

_output_shapes

:22
 
_user_specified_namestates:FB

_output_shapes

:22
 
_user_specified_namestates
¨
¼
while_cond_48835
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_48835___redundant_placeholder03
/while_while_cond_48835___redundant_placeholder13
/while_while_cond_48835___redundant_placeholder23
/while_while_cond_48835___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
ô.
¸
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54318

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
:	2È2
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
$:22:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:22
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
¨
¼
while_cond_52023
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_52023___redundant_placeholder03
/while_while_cond_52023___redundant_placeholder13
/while_while_cond_52023___redundant_placeholder23
/while_while_cond_52023___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:


Ü
lstm_16_while_cond_51775,
(lstm_16_while_lstm_16_while_loop_counter2
.lstm_16_while_lstm_16_while_maximum_iterations
lstm_16_while_placeholder
lstm_16_while_placeholder_1
lstm_16_while_placeholder_2
lstm_16_while_placeholder_3,
(lstm_16_while_less_lstm_16_strided_sliceC
?lstm_16_while_lstm_16_while_cond_51775___redundant_placeholder0C
?lstm_16_while_lstm_16_while_cond_51775___redundant_placeholder1C
?lstm_16_while_lstm_16_while_cond_51775___redundant_placeholder2C
?lstm_16_while_lstm_16_while_cond_51775___redundant_placeholder3
lstm_16_while_identity

lstm_16/while/LessLesslstm_16_while_placeholder(lstm_16_while_less_lstm_16_strided_slice*
T0*
_output_shapes
: 2
lstm_16/while/Lessu
lstm_16/while/IdentityIdentitylstm_16/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_16/while/Identity"9
lstm_16_while_identitylstm_16/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
®

M__inference_time_distributed_8_layer_call_and_return_conditional_losses_50458

inputs8
&dense_8_matmul_readvariableop_resource:25
'dense_8_biasadd_readvariableop_resource:
identity¢dense_8/BiasAdd/ReadVariableOp¢dense_8/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2
Reshape/shapeg
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes
:	ô22	
Reshape¥
dense_8/MatMul/ReadVariableOpReadVariableOp&dense_8_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_8/MatMul/ReadVariableOp
dense_8/MatMulMatMulReshape:output:0%dense_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô2
dense_8/MatMul¤
dense_8/BiasAdd/ReadVariableOpReadVariableOp'dense_8_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_8/BiasAdd/ReadVariableOp
dense_8/BiasAddBiasAdddense_8/MatMul:product:0&dense_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô2
dense_8/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      2
Reshape_1/shape
	Reshape_1Reshapedense_8/BiasAdd:output:0Reshape_1/shape:output:0*
T0*"
_output_shapes
:2
2
	Reshape_1h
IdentityIdentityReshape_1:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity
NoOpNoOp^dense_8/BiasAdd/ReadVariableOp^dense_8/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:2
2: : 2@
dense_8/BiasAdd/ReadVariableOpdense_8/BiasAdd/ReadVariableOp2>
dense_8/MatMul/ReadVariableOpdense_8/MatMul/ReadVariableOp:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
®

M__inference_time_distributed_8_layer_call_and_return_conditional_losses_50519

inputs8
&dense_8_matmul_readvariableop_resource:25
'dense_8_biasadd_readvariableop_resource:
identity¢dense_8/BiasAdd/ReadVariableOp¢dense_8/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2
Reshape/shapeg
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes
:	ô22	
Reshape¥
dense_8/MatMul/ReadVariableOpReadVariableOp&dense_8_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_8/MatMul/ReadVariableOp
dense_8/MatMulMatMulReshape:output:0%dense_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô2
dense_8/MatMul¤
dense_8/BiasAdd/ReadVariableOpReadVariableOp'dense_8_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_8/BiasAdd/ReadVariableOp
dense_8/BiasAddBiasAdddense_8/MatMul:product:0&dense_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô2
dense_8/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      2
Reshape_1/shape
	Reshape_1Reshapedense_8/BiasAdd:output:0Reshape_1/shape:output:0*
T0*"
_output_shapes
:2
2
	Reshape_1h
IdentityIdentityReshape_1:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity
NoOpNoOp^dense_8/BiasAdd/ReadVariableOp^dense_8/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:2
2: : 2@
dense_8/BiasAdd/ReadVariableOpdense_8/BiasAdd/ReadVariableOp2>
dense_8/MatMul/ReadVariableOpdense_8/MatMul/ReadVariableOp:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
Þ
î
'__inference_lstm_17_layer_call_fn_52708

inputs
unknown:	È
	unknown_0:22
	unknown_1:	2È
	unknown_2:	È
	unknown_3:22
identity¢StatefulPartitionedCall
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_17_layer_call_and_return_conditional_losses_502442
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
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
:2
: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
È
õ
,__inference_lstm_cell_16_layer_call_fn_54257

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
:22:22:22*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_494522
StatefulPartitionedCallr
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*
_output_shapes

:222

Identityv

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*
_output_shapes

:222

Identity_1v

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*
_output_shapes

:222

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
$:22:22:22: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:22
 
_user_specified_nameinputs:HD

_output_shapes

:22
"
_user_specified_name
states/0:HD

_output_shapes

:22
"
_user_specified_name
states/1
²

M__inference_time_distributed_8_layer_call_and_return_conditional_losses_53537

inputs8
&dense_8_matmul_readvariableop_resource:25
'dense_8_biasadd_readvariableop_resource:
identity¢dense_8/BiasAdd/ReadVariableOp¢dense_8/MatMul/ReadVariableOpD
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
Reshape¥
dense_8/MatMul/ReadVariableOpReadVariableOp&dense_8_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_8/MatMul/ReadVariableOp
dense_8/MatMulMatMulReshape:output:0%dense_8/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
dense_8/MatMul¤
dense_8/BiasAdd/ReadVariableOpReadVariableOp'dense_8_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_8/BiasAdd/ReadVariableOp¡
dense_8/BiasAddBiasAdddense_8/MatMul:product:0&dense_8/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ2
dense_8/BiasAddq
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
Reshape_1/shape
	Reshape_1Reshapedense_8/BiasAdd:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2

Identity
NoOpNoOp^dense_8/BiasAdd/ReadVariableOp^dense_8/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2: : 2@
dense_8/BiasAdd/ReadVariableOpdense_8/BiasAdd/ReadVariableOp2>
dense_8/MatMul/ReadVariableOpdense_8/MatMul/ReadVariableOp:\ X
4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
ã
ú
G__inference_sequential_8_layer_call_and_return_conditional_losses_51128
lstm_17_input 
lstm_17_51098:	È
lstm_17_51100:22 
lstm_17_51102:	2È
lstm_17_51104:	È
lstm_17_51106:22 
lstm_16_51109:	2È
lstm_16_51111:22 
lstm_16_51113:	2È
lstm_16_51115:	È
lstm_16_51117:22*
time_distributed_8_51120:2&
time_distributed_8_51122:
identity¢lstm_16/StatefulPartitionedCall¢lstm_17/StatefulPartitionedCall¢*time_distributed_8/StatefulPartitionedCall¿
lstm_17/StatefulPartitionedCallStatefulPartitionedCalllstm_17_inputlstm_17_51098lstm_17_51100lstm_17_51102lstm_17_51104lstm_17_51106*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_17_layer_call_and_return_conditional_losses_509292!
lstm_17/StatefulPartitionedCallÚ
lstm_16/StatefulPartitionedCallStatefulPartitionedCall(lstm_17/StatefulPartitionedCall:output:0lstm_16_51109lstm_16_51111lstm_16_51113lstm_16_51115lstm_16_51117*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_16_layer_call_and_return_conditional_losses_507212!
lstm_16/StatefulPartitionedCallà
*time_distributed_8/StatefulPartitionedCallStatefulPartitionedCall(lstm_16/StatefulPartitionedCall:output:0time_distributed_8_51120time_distributed_8_51122*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *V
fQRO
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_505192,
*time_distributed_8/StatefulPartitionedCall
 time_distributed_8/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2"
 time_distributed_8/Reshape/shapeÂ
time_distributed_8/ReshapeReshape(lstm_16/StatefulPartitionedCall:output:0)time_distributed_8/Reshape/shape:output:0*
T0*
_output_shapes
:	ô22
time_distributed_8/Reshape
IdentityIdentity3time_distributed_8/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity¿
NoOpNoOp ^lstm_16/StatefulPartitionedCall ^lstm_17/StatefulPartitionedCall+^time_distributed_8/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 2B
lstm_16/StatefulPartitionedCalllstm_16/StatefulPartitionedCall2B
lstm_17/StatefulPartitionedCalllstm_17/StatefulPartitionedCall2X
*time_distributed_8/StatefulPartitionedCall*time_distributed_8/StatefulPartitionedCall:Q M
"
_output_shapes
:2

'
_user_specified_namelstm_17_input
i
Ë

lstm_16_while_body_51412,
(lstm_16_while_lstm_16_while_loop_counter2
.lstm_16_while_lstm_16_while_maximum_iterations
lstm_16_while_placeholder
lstm_16_while_placeholder_1
lstm_16_while_placeholder_2
lstm_16_while_placeholder_3)
%lstm_16_while_lstm_16_strided_slice_0g
clstm_16_while_tensorarrayv2read_tensorlistgetitem_lstm_16_tensorarrayunstack_tensorlistfromtensor_0N
;lstm_16_while_lstm_cell_16_matmul_readvariableop_resource_0:	2ÈP
=lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource_0:	2ÈK
<lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource_0:	È
lstm_16_while_identity
lstm_16_while_identity_1
lstm_16_while_identity_2
lstm_16_while_identity_3
lstm_16_while_identity_4
lstm_16_while_identity_5'
#lstm_16_while_lstm_16_strided_slicee
alstm_16_while_tensorarrayv2read_tensorlistgetitem_lstm_16_tensorarrayunstack_tensorlistfromtensorL
9lstm_16_while_lstm_cell_16_matmul_readvariableop_resource:	2ÈN
;lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource:	2ÈI
:lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource:	È¢1lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp¢0lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp¢2lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOpÓ
?lstm_16/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2A
?lstm_16/while/TensorArrayV2Read/TensorListGetItem/element_shapeú
1lstm_16/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemclstm_16_while_tensorarrayv2read_tensorlistgetitem_lstm_16_tensorarrayunstack_tensorlistfromtensor_0lstm_16_while_placeholderHlstm_16/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype023
1lstm_16/while/TensorArrayV2Read/TensorListGetItemá
0lstm_16/while/lstm_cell_16/MatMul/ReadVariableOpReadVariableOp;lstm_16_while_lstm_cell_16_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype022
0lstm_16/while/lstm_cell_16/MatMul/ReadVariableOpî
!lstm_16/while/lstm_cell_16/MatMulMatMul8lstm_16/while/TensorArrayV2Read/TensorListGetItem:item:08lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2#
!lstm_16/while/lstm_cell_16/MatMulç
2lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp=lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype024
2lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp×
#lstm_16/while/lstm_cell_16/MatMul_1MatMullstm_16_while_placeholder_2:lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2%
#lstm_16/while/lstm_cell_16/MatMul_1Ï
lstm_16/while/lstm_cell_16/addAddV2+lstm_16/while/lstm_cell_16/MatMul:product:0-lstm_16/while/lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2 
lstm_16/while/lstm_cell_16/addà
1lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp<lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype023
1lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOpÜ
"lstm_16/while/lstm_cell_16/BiasAddBiasAdd"lstm_16/while/lstm_cell_16/add:z:09lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2$
"lstm_16/while/lstm_cell_16/BiasAdd
*lstm_16/while/lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2,
*lstm_16/while/lstm_cell_16/split/split_dim
 lstm_16/while/lstm_cell_16/splitSplit3lstm_16/while/lstm_cell_16/split/split_dim:output:0+lstm_16/while/lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2"
 lstm_16/while/lstm_cell_16/split
 lstm_16/while/lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2"
 lstm_16/while/lstm_cell_16/Const
"lstm_16/while/lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_16/while/lstm_cell_16/Const_1Æ
lstm_16/while/lstm_cell_16/MulMul)lstm_16/while/lstm_cell_16/split:output:0)lstm_16/while/lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222 
lstm_16/while/lstm_cell_16/MulÇ
 lstm_16/while/lstm_cell_16/Add_1AddV2"lstm_16/while/lstm_cell_16/Mul:z:0+lstm_16/while/lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/Add_1­
2lstm_16/while/lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?24
2lstm_16/while/lstm_cell_16/clip_by_value/Minimum/yû
0lstm_16/while/lstm_cell_16/clip_by_value/MinimumMinimum$lstm_16/while/lstm_cell_16/Add_1:z:0;lstm_16/while/lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:2222
0lstm_16/while/lstm_cell_16/clip_by_value/Minimum
*lstm_16/while/lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_16/while/lstm_cell_16/clip_by_value/yó
(lstm_16/while/lstm_cell_16/clip_by_valueMaximum4lstm_16/while/lstm_cell_16/clip_by_value/Minimum:z:03lstm_16/while/lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222*
(lstm_16/while/lstm_cell_16/clip_by_value
"lstm_16/while/lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_16/while/lstm_cell_16/Const_2
"lstm_16/while/lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_16/while/lstm_cell_16/Const_3Ì
 lstm_16/while/lstm_cell_16/Mul_1Mul)lstm_16/while/lstm_cell_16/split:output:1+lstm_16/while/lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/Mul_1É
 lstm_16/while/lstm_cell_16/Add_2AddV2$lstm_16/while/lstm_cell_16/Mul_1:z:0+lstm_16/while/lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/Add_2±
4lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum/y
2lstm_16/while/lstm_cell_16/clip_by_value_1/MinimumMinimum$lstm_16/while/lstm_cell_16/Add_2:z:0=lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:2224
2lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum¡
,lstm_16/while/lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_16/while/lstm_cell_16/clip_by_value_1/yû
*lstm_16/while/lstm_cell_16/clip_by_value_1Maximum6lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum:z:05lstm_16/while/lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222,
*lstm_16/while/lstm_cell_16/clip_by_value_1Á
 lstm_16/while/lstm_cell_16/mul_2Mul.lstm_16/while/lstm_cell_16/clip_by_value_1:z:0lstm_16_while_placeholder_3*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/mul_2
lstm_16/while/lstm_cell_16/TanhTanh)lstm_16/while/lstm_cell_16/split:output:2*
T0*
_output_shapes

:222!
lstm_16/while/lstm_cell_16/TanhÇ
 lstm_16/while/lstm_cell_16/mul_3Mul,lstm_16/while/lstm_cell_16/clip_by_value:z:0#lstm_16/while/lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/mul_3Â
 lstm_16/while/lstm_cell_16/add_3AddV2$lstm_16/while/lstm_cell_16/mul_2:z:0$lstm_16/while/lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/add_3
"lstm_16/while/lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_16/while/lstm_cell_16/Const_4
"lstm_16/while/lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_16/while/lstm_cell_16/Const_5Ì
 lstm_16/while/lstm_cell_16/Mul_4Mul)lstm_16/while/lstm_cell_16/split:output:3+lstm_16/while/lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/Mul_4É
 lstm_16/while/lstm_cell_16/Add_4AddV2$lstm_16/while/lstm_cell_16/Mul_4:z:0+lstm_16/while/lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/Add_4±
4lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum/y
2lstm_16/while/lstm_cell_16/clip_by_value_2/MinimumMinimum$lstm_16/while/lstm_cell_16/Add_4:z:0=lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:2224
2lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum¡
,lstm_16/while/lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_16/while/lstm_cell_16/clip_by_value_2/yû
*lstm_16/while/lstm_cell_16/clip_by_value_2Maximum6lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum:z:05lstm_16/while/lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222,
*lstm_16/while/lstm_cell_16/clip_by_value_2
!lstm_16/while/lstm_cell_16/Tanh_1Tanh$lstm_16/while/lstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222#
!lstm_16/while/lstm_cell_16/Tanh_1Ë
 lstm_16/while/lstm_cell_16/mul_5Mul.lstm_16/while/lstm_cell_16/clip_by_value_2:z:0%lstm_16/while/lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/mul_5
2lstm_16/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_16_while_placeholder_1lstm_16_while_placeholder$lstm_16/while/lstm_cell_16/mul_5:z:0*
_output_shapes
: *
element_dtype024
2lstm_16/while/TensorArrayV2Write/TensorListSetIteml
lstm_16/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_16/while/add/y
lstm_16/while/addAddV2lstm_16_while_placeholderlstm_16/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_16/while/addp
lstm_16/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_16/while/add_1/y
lstm_16/while/add_1AddV2(lstm_16_while_lstm_16_while_loop_counterlstm_16/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_16/while/add_1
lstm_16/while/IdentityIdentitylstm_16/while/add_1:z:0^lstm_16/while/NoOp*
T0*
_output_shapes
: 2
lstm_16/while/Identity¦
lstm_16/while/Identity_1Identity.lstm_16_while_lstm_16_while_maximum_iterations^lstm_16/while/NoOp*
T0*
_output_shapes
: 2
lstm_16/while/Identity_1
lstm_16/while/Identity_2Identitylstm_16/while/add:z:0^lstm_16/while/NoOp*
T0*
_output_shapes
: 2
lstm_16/while/Identity_2º
lstm_16/while/Identity_3IdentityBlstm_16/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_16/while/NoOp*
T0*
_output_shapes
: 2
lstm_16/while/Identity_3¤
lstm_16/while/Identity_4Identity$lstm_16/while/lstm_cell_16/mul_5:z:0^lstm_16/while/NoOp*
T0*
_output_shapes

:222
lstm_16/while/Identity_4¤
lstm_16/while/Identity_5Identity$lstm_16/while/lstm_cell_16/add_3:z:0^lstm_16/while/NoOp*
T0*
_output_shapes

:222
lstm_16/while/Identity_5
lstm_16/while/NoOpNoOp2^lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp1^lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp3^lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_16/while/NoOp"9
lstm_16_while_identitylstm_16/while/Identity:output:0"=
lstm_16_while_identity_1!lstm_16/while/Identity_1:output:0"=
lstm_16_while_identity_2!lstm_16/while/Identity_2:output:0"=
lstm_16_while_identity_3!lstm_16/while/Identity_3:output:0"=
lstm_16_while_identity_4!lstm_16/while/Identity_4:output:0"=
lstm_16_while_identity_5!lstm_16/while/Identity_5:output:0"L
#lstm_16_while_lstm_16_strided_slice%lstm_16_while_lstm_16_strided_slice_0"z
:lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource<lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource_0"|
;lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource=lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource_0"x
9lstm_16_while_lstm_cell_16_matmul_readvariableop_resource;lstm_16_while_lstm_cell_16_matmul_readvariableop_resource_0"È
alstm_16_while_tensorarrayv2read_tensorlistgetitem_lstm_16_tensorarrayunstack_tensorlistfromtensorclstm_16_while_tensorarrayv2read_tensorlistgetitem_lstm_16_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2f
1lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp1lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp2d
0lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp0lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp2h
2lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp2lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
ùn
´
B__inference_lstm_17_layer_call_and_return_conditional_losses_52663

inputs>
+lstm_cell_17_matmul_readvariableop_resource:	È?
-lstm_cell_17_matmul_1_readvariableop_resource:22B
/lstm_cell_17_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_17_biasadd_readvariableop_resource:	È<
*lstm_cell_17_mul_2_readvariableop_resource:22
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_17/BiasAdd/ReadVariableOp¢"lstm_cell_17/MatMul/ReadVariableOp¢$lstm_cell_17/MatMul_1/ReadVariableOp¢&lstm_cell_17/MatMul_1/ReadVariableOp_1¢!lstm_cell_17/mul_2/ReadVariableOp¢whileu
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
22
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2      2
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_17/MatMul/ReadVariableOpReadVariableOp+lstm_cell_17_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02$
"lstm_cell_17/MatMul/ReadVariableOp¤
lstm_cell_17/MatMulMatMulstrided_slice_1:output:0*lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/MatMulº
$lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02&
$lstm_cell_17/MatMul_1/ReadVariableOpÁ
&lstm_cell_17/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_17_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_17/MatMul_1/ReadVariableOp_1À
lstm_cell_17/MatMul_1MatMul,lstm_cell_17/MatMul_1/ReadVariableOp:value:0.lstm_cell_17/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/MatMul_1
lstm_cell_17/addAddV2lstm_cell_17/MatMul:product:0lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/add´
#lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_17/BiasAdd/ReadVariableOp¤
lstm_cell_17/BiasAddBiasAddlstm_cell_17/add:z:0+lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/BiasAdd~
lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/split/split_dimÏ
lstm_cell_17/splitSplit%lstm_cell_17/split/split_dim:output:0lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_17/splitm
lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Constq
lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_1
lstm_cell_17/MulMullstm_cell_17/split:output:0lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul
lstm_cell_17/Add_1AddV2lstm_cell_17/Mul:z:0lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_1
$lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_17/clip_by_value/Minimum/yÃ
"lstm_cell_17/clip_by_value/MinimumMinimumlstm_cell_17/Add_1:z:0-lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222$
"lstm_cell_17/clip_by_value/Minimum
lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_17/clip_by_value/y»
lstm_cell_17/clip_by_valueMaximum&lstm_cell_17/clip_by_value/Minimum:z:0%lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_valueq
lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Const_2q
lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_3
lstm_cell_17/Mul_1Mullstm_cell_17/split:output:1lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul_1
lstm_cell_17/Add_2AddV2lstm_cell_17/Mul_1:z:0lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_2
&lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_17/clip_by_value_1/Minimum/yÉ
$lstm_cell_17/clip_by_value_1/MinimumMinimumlstm_cell_17/Add_2:z:0/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_17/clip_by_value_1/Minimum
lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_17/clip_by_value_1/yÃ
lstm_cell_17/clip_by_value_1Maximum(lstm_cell_17/clip_by_value_1/Minimum:z:0'lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_value_1±
!lstm_cell_17/mul_2/ReadVariableOpReadVariableOp*lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02#
!lstm_cell_17/mul_2/ReadVariableOp¥
lstm_cell_17/mul_2Mul lstm_cell_17/clip_by_value_1:z:0)lstm_cell_17/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_2t
lstm_cell_17/TanhTanhlstm_cell_17/split:output:2*
T0*
_output_shapes

:222
lstm_cell_17/Tanh
lstm_cell_17/mul_3Mullstm_cell_17/clip_by_value:z:0lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_3
lstm_cell_17/add_3AddV2lstm_cell_17/mul_2:z:0lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_17/add_3q
lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Const_4q
lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_5
lstm_cell_17/Mul_4Mullstm_cell_17/split:output:3lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul_4
lstm_cell_17/Add_4AddV2lstm_cell_17/Mul_4:z:0lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_4
&lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_17/clip_by_value_2/Minimum/yÉ
$lstm_cell_17/clip_by_value_2/MinimumMinimumlstm_cell_17/Add_4:z:0/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_17/clip_by_value_2/Minimum
lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_17/clip_by_value_2/yÃ
lstm_cell_17/clip_by_value_2Maximum(lstm_cell_17/clip_by_value_2/Minimum:z:0'lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_value_2s
lstm_cell_17/Tanh_1Tanhlstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_17/Tanh_1
lstm_cell_17/mul_5Mul lstm_cell_17/clip_by_value_2:z:0lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_17_matmul_readvariableop_resource/lstm_cell_17_matmul_1_readvariableop_1_resource,lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_52558*
condR
while_cond_52557*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_17_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_17_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_17/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_17/BiasAdd/ReadVariableOp#^lstm_cell_17/MatMul/ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp'^lstm_cell_17/MatMul_1/ReadVariableOp_1"^lstm_cell_17/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:2
: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_17/BiasAdd/ReadVariableOp#lstm_cell_17/BiasAdd/ReadVariableOp2H
"lstm_cell_17/MatMul/ReadVariableOp"lstm_cell_17/MatMul/ReadVariableOp2L
$lstm_cell_17/MatMul_1/ReadVariableOp$lstm_cell_17/MatMul_1/ReadVariableOp2P
&lstm_cell_17/MatMul_1/ReadVariableOp_1&lstm_cell_17/MatMul_1/ReadVariableOp_12F
!lstm_cell_17/mul_2/ReadVariableOp!lstm_cell_17/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
ü.
º
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53658

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
:	2È2
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
$:2:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
ï

2__inference_time_distributed_8_layer_call_fn_53601

inputs
unknown:2
	unknown_0:
identity¢StatefulPartitionedCallø
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *V
fQRO
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_505192
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
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
:2
2: : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
ÔY
Ë
while_body_50824
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_17_matmul_readvariableop_resource_0:	ÈH
5while_lstm_cell_17_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_17_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_17_matmul_readvariableop_resource:	ÈF
3while_lstm_cell_17_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_17_biasadd_readvariableop_resource:	È¢)while/lstm_cell_17/BiasAdd/ReadVariableOp¢(while/lstm_cell_17/MatMul/ReadVariableOp¢*while/lstm_cell_17/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02*
(while/lstm_cell_17/MatMul/ReadVariableOpÎ
while/lstm_cell_17/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/MatMulÏ
*while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_17/MatMul_1/ReadVariableOp·
while/lstm_cell_17/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/MatMul_1¯
while/lstm_cell_17/addAddV2#while/lstm_cell_17/MatMul:product:0%while/lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/addÈ
)while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_17/BiasAdd/ReadVariableOp¼
while/lstm_cell_17/BiasAddBiasAddwhile/lstm_cell_17/add:z:01while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/BiasAdd
"while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_17/split/split_dimç
while/lstm_cell_17/splitSplit+while/lstm_cell_17/split/split_dim:output:0#while/lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_17/splity
while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const}
while/lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_1¦
while/lstm_cell_17/MulMul!while/lstm_cell_17/split:output:0!while/lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul§
while/lstm_cell_17/Add_1AddV2while/lstm_cell_17/Mul:z:0#while/lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_1
*while/lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_17/clip_by_value/Minimum/yÛ
(while/lstm_cell_17/clip_by_value/MinimumMinimumwhile/lstm_cell_17/Add_1:z:03while/lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(while/lstm_cell_17/clip_by_value/Minimum
"while/lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_17/clip_by_value/yÓ
 while/lstm_cell_17/clip_by_valueMaximum,while/lstm_cell_17/clip_by_value/Minimum:z:0+while/lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 while/lstm_cell_17/clip_by_value}
while/lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const_2}
while/lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_3¬
while/lstm_cell_17/Mul_1Mul!while/lstm_cell_17/split:output:1#while/lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul_1©
while/lstm_cell_17/Add_2AddV2while/lstm_cell_17/Mul_1:z:0#while/lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_2¡
,while/lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_17/clip_by_value_1/Minimum/yá
*while/lstm_cell_17/clip_by_value_1/MinimumMinimumwhile/lstm_cell_17/Add_2:z:05while/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_17/clip_by_value_1/Minimum
$while/lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_17/clip_by_value_1/yÛ
"while/lstm_cell_17/clip_by_value_1Maximum.while/lstm_cell_17/clip_by_value_1/Minimum:z:0-while/lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_17/clip_by_value_1¡
while/lstm_cell_17/mul_2Mul&while/lstm_cell_17/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_2
while/lstm_cell_17/TanhTanh!while/lstm_cell_17/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_17/Tanh§
while/lstm_cell_17/mul_3Mul$while/lstm_cell_17/clip_by_value:z:0while/lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_3¢
while/lstm_cell_17/add_3AddV2while/lstm_cell_17/mul_2:z:0while/lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_17/add_3}
while/lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const_4}
while/lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_5¬
while/lstm_cell_17/Mul_4Mul!while/lstm_cell_17/split:output:3#while/lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul_4©
while/lstm_cell_17/Add_4AddV2while/lstm_cell_17/Mul_4:z:0#while/lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_4¡
,while/lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_17/clip_by_value_2/Minimum/yá
*while/lstm_cell_17/clip_by_value_2/MinimumMinimumwhile/lstm_cell_17/Add_4:z:05while/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_17/clip_by_value_2/Minimum
$while/lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_17/clip_by_value_2/yÛ
"while/lstm_cell_17/clip_by_value_2Maximum.while/lstm_cell_17/clip_by_value_2/Minimum:z:0-while/lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_17/clip_by_value_2
while/lstm_cell_17/Tanh_1Tanhwhile/lstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Tanh_1«
while/lstm_cell_17/mul_5Mul&while/lstm_cell_17/clip_by_value_2:z:0while/lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_17/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_17/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_17/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*"
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
2while_lstm_cell_17_biasadd_readvariableop_resource4while_lstm_cell_17_biasadd_readvariableop_resource_0"l
3while_lstm_cell_17_matmul_1_readvariableop_resource5while_lstm_cell_17_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_17_matmul_readvariableop_resource3while_lstm_cell_17_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_17/BiasAdd/ReadVariableOp)while/lstm_cell_17/BiasAdd/ReadVariableOp2T
(while/lstm_cell_17/MatMul/ReadVariableOp(while/lstm_cell_17/MatMul/ReadVariableOp2X
*while/lstm_cell_17/MatMul_1/ReadVariableOp*while/lstm_cell_17/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
ü.
º
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54060

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
:	2È2
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
$:22:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:22
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
¨
¼
while_cond_50138
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_50138___redundant_placeholder03
/while_while_cond_50138___redundant_placeholder13
/while_while_cond_50138___redundant_placeholder23
/while_while_cond_50138___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
ï

2__inference_time_distributed_8_layer_call_fn_53592

inputs
unknown:2
	unknown_0:
identity¢StatefulPartitionedCallø
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *V
fQRO
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_504582
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
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
:2
2: : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
ø9

B__inference_lstm_17_layer_call_and_return_conditional_losses_48905

inputs$
lstm_cell_17_48817:22$
lstm_cell_17_48819:22%
lstm_cell_17_48821:	È%
lstm_cell_17_48823:	2È!
lstm_cell_17_48825:	È
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢$lstm_cell_17/StatefulPartitionedCall¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1
$lstm_cell_17/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_17_48817lstm_cell_17_48819lstm_cell_17_48821lstm_cell_17_48823lstm_cell_17_48825*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:22:22:22*'
_read_only_resource_inputs	
*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_487672&
$lstm_cell_17/StatefulPartitionedCall
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOplstm_cell_17_48817*
_output_shapes

:22*
dtype02
ReadVariableOpw
ReadVariableOp_1ReadVariableOplstm_cell_17_48819*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_17_48821lstm_cell_17_48823lstm_cell_17_48825*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_48836*
condR
while_cond_48835*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22*
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

:22*
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
:2ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime·
AssignVariableOpAssignVariableOplstm_cell_17_48817while:output:4^ReadVariableOp%^lstm_cell_17/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp½
AssignVariableOp_1AssignVariableOplstm_cell_17_48819while:output:5^ReadVariableOp_1%^lstm_cell_17/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ22

IdentityÉ
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1%^lstm_cell_17/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2ÿÿÿÿÿÿÿÿÿ: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12L
$lstm_cell_17/StatefulPartitionedCall$lstm_cell_17/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs
ùn
´
B__inference_lstm_16_layer_call_and_return_conditional_losses_53257

inputs>
+lstm_cell_16_matmul_readvariableop_resource:	2È?
-lstm_cell_16_matmul_1_readvariableop_resource:22B
/lstm_cell_16_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_16_biasadd_readvariableop_resource:	È<
*lstm_cell_16_mul_2_readvariableop_resource:22
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_16/BiasAdd/ReadVariableOp¢"lstm_cell_16/MatMul/ReadVariableOp¢$lstm_cell_16/MatMul_1/ReadVariableOp¢&lstm_cell_16/MatMul_1/ReadVariableOp_1¢!lstm_cell_16/mul_2/ReadVariableOp¢whileu
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
222
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2   2   2
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_16/MatMul/ReadVariableOpReadVariableOp+lstm_cell_16_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02$
"lstm_cell_16/MatMul/ReadVariableOp¤
lstm_cell_16/MatMulMatMulstrided_slice_1:output:0*lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/MatMulº
$lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02&
$lstm_cell_16/MatMul_1/ReadVariableOpÁ
&lstm_cell_16/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_16_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_16/MatMul_1/ReadVariableOp_1À
lstm_cell_16/MatMul_1MatMul,lstm_cell_16/MatMul_1/ReadVariableOp:value:0.lstm_cell_16/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/MatMul_1
lstm_cell_16/addAddV2lstm_cell_16/MatMul:product:0lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/add´
#lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_16_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_16/BiasAdd/ReadVariableOp¤
lstm_cell_16/BiasAddBiasAddlstm_cell_16/add:z:0+lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/BiasAdd~
lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_16/split/split_dimÏ
lstm_cell_16/splitSplit%lstm_cell_16/split/split_dim:output:0lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_16/splitm
lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Constq
lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_1
lstm_cell_16/MulMullstm_cell_16/split:output:0lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul
lstm_cell_16/Add_1AddV2lstm_cell_16/Mul:z:0lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_1
$lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_16/clip_by_value/Minimum/yÃ
"lstm_cell_16/clip_by_value/MinimumMinimumlstm_cell_16/Add_1:z:0-lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222$
"lstm_cell_16/clip_by_value/Minimum
lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_16/clip_by_value/y»
lstm_cell_16/clip_by_valueMaximum&lstm_cell_16/clip_by_value/Minimum:z:0%lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_valueq
lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Const_2q
lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_3
lstm_cell_16/Mul_1Mullstm_cell_16/split:output:1lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul_1
lstm_cell_16/Add_2AddV2lstm_cell_16/Mul_1:z:0lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_2
&lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_16/clip_by_value_1/Minimum/yÉ
$lstm_cell_16/clip_by_value_1/MinimumMinimumlstm_cell_16/Add_2:z:0/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_16/clip_by_value_1/Minimum
lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_16/clip_by_value_1/yÃ
lstm_cell_16/clip_by_value_1Maximum(lstm_cell_16/clip_by_value_1/Minimum:z:0'lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_value_1±
!lstm_cell_16/mul_2/ReadVariableOpReadVariableOp*lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02#
!lstm_cell_16/mul_2/ReadVariableOp¥
lstm_cell_16/mul_2Mul lstm_cell_16/clip_by_value_1:z:0)lstm_cell_16/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_2t
lstm_cell_16/TanhTanhlstm_cell_16/split:output:2*
T0*
_output_shapes

:222
lstm_cell_16/Tanh
lstm_cell_16/mul_3Mullstm_cell_16/clip_by_value:z:0lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_3
lstm_cell_16/add_3AddV2lstm_cell_16/mul_2:z:0lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_16/add_3q
lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Const_4q
lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_5
lstm_cell_16/Mul_4Mullstm_cell_16/split:output:3lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul_4
lstm_cell_16/Add_4AddV2lstm_cell_16/Mul_4:z:0lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_4
&lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_16/clip_by_value_2/Minimum/yÉ
$lstm_cell_16/clip_by_value_2/MinimumMinimumlstm_cell_16/Add_4:z:0/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_16/clip_by_value_2/Minimum
lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_16/clip_by_value_2/yÃ
lstm_cell_16/clip_by_value_2Maximum(lstm_cell_16/clip_by_value_2/Minimum:z:0'lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_value_2s
lstm_cell_16/Tanh_1Tanhlstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_16/Tanh_1
lstm_cell_16/mul_5Mul lstm_cell_16/clip_by_value_2:z:0lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp-lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_16_matmul_readvariableop_resource/lstm_cell_16_matmul_1_readvariableop_1_resource,lstm_cell_16_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_53152*
condR
while_cond_53151*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_16_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_16/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_16_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_16/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_16/BiasAdd/ReadVariableOp#^lstm_cell_16/MatMul/ReadVariableOp%^lstm_cell_16/MatMul_1/ReadVariableOp'^lstm_cell_16/MatMul_1/ReadVariableOp_1"^lstm_cell_16/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:2
2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_16/BiasAdd/ReadVariableOp#lstm_cell_16/BiasAdd/ReadVariableOp2H
"lstm_cell_16/MatMul/ReadVariableOp"lstm_cell_16/MatMul/ReadVariableOp2L
$lstm_cell_16/MatMul_1/ReadVariableOp$lstm_cell_16/MatMul_1/ReadVariableOp2P
&lstm_cell_16/MatMul_1/ReadVariableOp_1&lstm_cell_16/MatMul_1/ReadVariableOp_12F
!lstm_cell_16/mul_2/ReadVariableOp!lstm_cell_16/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
	
ð
'__inference_lstm_16_layer_call_fn_53465
inputs_0
unknown:22
	unknown_0:22
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
:2ÿÿÿÿÿÿÿÿÿ2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_16_layer_call_and_return_conditional_losses_496812
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2ÿÿÿÿÿÿÿÿÿ2: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ2
"
_user_specified_name
inputs/0
¨
¼
while_cond_53151
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_53151___redundant_placeholder03
/while_while_cond_53151___redundant_placeholder13
/while_while_cond_53151___redundant_placeholder23
/while_while_cond_53151___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:


Ü
lstm_17_while_cond_51237,
(lstm_17_while_lstm_17_while_loop_counter2
.lstm_17_while_lstm_17_while_maximum_iterations
lstm_17_while_placeholder
lstm_17_while_placeholder_1
lstm_17_while_placeholder_2
lstm_17_while_placeholder_3,
(lstm_17_while_less_lstm_17_strided_sliceC
?lstm_17_while_lstm_17_while_cond_51237___redundant_placeholder0C
?lstm_17_while_lstm_17_while_cond_51237___redundant_placeholder1C
?lstm_17_while_lstm_17_while_cond_51237___redundant_placeholder2C
?lstm_17_while_lstm_17_while_cond_51237___redundant_placeholder3
lstm_17_while_identity

lstm_17/while/LessLesslstm_17_while_placeholder(lstm_17_while_less_lstm_17_strided_slice*
T0*
_output_shapes
: 2
lstm_17/while/Lessu
lstm_17/while/IdentityIdentitylstm_17/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_17/while/Identity"9
lstm_17_while_identitylstm_17/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
êS
Ý
__inference__traced_save_54558
file_prefix(
$savev2_adam_iter_read_readvariableop	*
&savev2_adam_beta_1_read_readvariableop*
&savev2_adam_beta_2_read_readvariableop)
%savev2_adam_decay_read_readvariableop1
-savev2_adam_learning_rate_read_readvariableop:
6savev2_lstm_17_lstm_cell_17_kernel_read_readvariableopD
@savev2_lstm_17_lstm_cell_17_recurrent_kernel_read_readvariableop8
4savev2_lstm_17_lstm_cell_17_bias_read_readvariableop:
6savev2_lstm_16_lstm_cell_16_kernel_read_readvariableopD
@savev2_lstm_16_lstm_cell_16_recurrent_kernel_read_readvariableop8
4savev2_lstm_16_lstm_cell_16_bias_read_readvariableop8
4savev2_time_distributed_8_kernel_read_readvariableop6
2savev2_time_distributed_8_bias_read_readvariableop/
+savev2_lstm_17_variable_read_readvariableop1
-savev2_lstm_17_variable_1_read_readvariableop/
+savev2_lstm_16_variable_read_readvariableop1
-savev2_lstm_16_variable_1_read_readvariableop$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop&
"savev2_total_1_read_readvariableop&
"savev2_count_1_read_readvariableopA
=savev2_adam_lstm_17_lstm_cell_17_kernel_m_read_readvariableopK
Gsavev2_adam_lstm_17_lstm_cell_17_recurrent_kernel_m_read_readvariableop?
;savev2_adam_lstm_17_lstm_cell_17_bias_m_read_readvariableopA
=savev2_adam_lstm_16_lstm_cell_16_kernel_m_read_readvariableopK
Gsavev2_adam_lstm_16_lstm_cell_16_recurrent_kernel_m_read_readvariableop?
;savev2_adam_lstm_16_lstm_cell_16_bias_m_read_readvariableop?
;savev2_adam_time_distributed_8_kernel_m_read_readvariableop=
9savev2_adam_time_distributed_8_bias_m_read_readvariableopA
=savev2_adam_lstm_17_lstm_cell_17_kernel_v_read_readvariableopK
Gsavev2_adam_lstm_17_lstm_cell_17_recurrent_kernel_v_read_readvariableop?
;savev2_adam_lstm_17_lstm_cell_17_bias_v_read_readvariableopA
=savev2_adam_lstm_16_lstm_cell_16_kernel_v_read_readvariableopK
Gsavev2_adam_lstm_16_lstm_cell_16_recurrent_kernel_v_read_readvariableop?
;savev2_adam_lstm_16_lstm_cell_16_bias_v_read_readvariableop?
;savev2_adam_time_distributed_8_kernel_v_read_readvariableop=
9savev2_adam_time_distributed_8_bias_v_read_readvariableop
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
ShardedFilenameÞ
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:&*
dtype0*ð
valueæBã&B)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/0/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/1/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/2/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/3/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/4/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/5/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/6/.ATTRIBUTES/VARIABLE_VALUEB0trainable_variables/7/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/0/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/1/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/2/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/3/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/4/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/5/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/6/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/7/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/0/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/1/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/2/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/3/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/4/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/5/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/6/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBLtrainable_variables/7/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
SaveV2/tensor_namesÔ
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:&*
dtype0*_
valueVBT&B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B 2
SaveV2/shape_and_slices½
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:0$savev2_adam_iter_read_readvariableop&savev2_adam_beta_1_read_readvariableop&savev2_adam_beta_2_read_readvariableop%savev2_adam_decay_read_readvariableop-savev2_adam_learning_rate_read_readvariableop6savev2_lstm_17_lstm_cell_17_kernel_read_readvariableop@savev2_lstm_17_lstm_cell_17_recurrent_kernel_read_readvariableop4savev2_lstm_17_lstm_cell_17_bias_read_readvariableop6savev2_lstm_16_lstm_cell_16_kernel_read_readvariableop@savev2_lstm_16_lstm_cell_16_recurrent_kernel_read_readvariableop4savev2_lstm_16_lstm_cell_16_bias_read_readvariableop4savev2_time_distributed_8_kernel_read_readvariableop2savev2_time_distributed_8_bias_read_readvariableop+savev2_lstm_17_variable_read_readvariableop-savev2_lstm_17_variable_1_read_readvariableop+savev2_lstm_16_variable_read_readvariableop-savev2_lstm_16_variable_1_read_readvariableop savev2_total_read_readvariableop savev2_count_read_readvariableop"savev2_total_1_read_readvariableop"savev2_count_1_read_readvariableop=savev2_adam_lstm_17_lstm_cell_17_kernel_m_read_readvariableopGsavev2_adam_lstm_17_lstm_cell_17_recurrent_kernel_m_read_readvariableop;savev2_adam_lstm_17_lstm_cell_17_bias_m_read_readvariableop=savev2_adam_lstm_16_lstm_cell_16_kernel_m_read_readvariableopGsavev2_adam_lstm_16_lstm_cell_16_recurrent_kernel_m_read_readvariableop;savev2_adam_lstm_16_lstm_cell_16_bias_m_read_readvariableop;savev2_adam_time_distributed_8_kernel_m_read_readvariableop9savev2_adam_time_distributed_8_bias_m_read_readvariableop=savev2_adam_lstm_17_lstm_cell_17_kernel_v_read_readvariableopGsavev2_adam_lstm_17_lstm_cell_17_recurrent_kernel_v_read_readvariableop;savev2_adam_lstm_17_lstm_cell_17_bias_v_read_readvariableop=savev2_adam_lstm_16_lstm_cell_16_kernel_v_read_readvariableopGsavev2_adam_lstm_16_lstm_cell_16_recurrent_kernel_v_read_readvariableop;savev2_adam_lstm_16_lstm_cell_16_bias_v_read_readvariableop;savev2_adam_time_distributed_8_kernel_v_read_readvariableop9savev2_adam_time_distributed_8_bias_v_read_readvariableopsavev2_const"/device:CPU:0*
_output_shapes
 *4
dtypes*
(2&	2
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

identity_1Identity_1:output:0*±
_input_shapes
: : : : : : :	È:	2È:È:	2È:	2È:È:2::22:22:22:22: : : : :	È:	2È:È:	2È:	2È:È:2::	È:	2È:È:	2È:	2È:È:2:: 2(
MergeV2CheckpointsMergeV2Checkpoints:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :%!

_output_shapes
:	È:%!

_output_shapes
:	2È:!

_output_shapes	
:È:%	!

_output_shapes
:	2È:%
!

_output_shapes
:	2È:!

_output_shapes	
:È:$ 

_output_shapes

:2: 

_output_shapes
::$ 

_output_shapes

:22:$ 

_output_shapes

:22:$ 

_output_shapes

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :%!

_output_shapes
:	È:%!

_output_shapes
:	2È:!

_output_shapes	
:È:%!

_output_shapes
:	2È:%!

_output_shapes
:	2È:!

_output_shapes	
:È:$ 

_output_shapes

:2: 

_output_shapes
::%!

_output_shapes
:	È:%!

_output_shapes
:	2È:! 

_output_shapes	
:È:%!!

_output_shapes
:	2È:%"!

_output_shapes
:	2È:!#

_output_shapes	
:È:$$ 

_output_shapes

:2: %

_output_shapes
::&

_output_shapes
: 
á
ú
G__inference_sequential_8_layer_call_and_return_conditional_losses_51529

inputsF
3lstm_17_lstm_cell_17_matmul_readvariableop_resource:	ÈG
5lstm_17_lstm_cell_17_matmul_1_readvariableop_resource:22J
7lstm_17_lstm_cell_17_matmul_1_readvariableop_1_resource:	2ÈC
4lstm_17_lstm_cell_17_biasadd_readvariableop_resource:	ÈD
2lstm_17_lstm_cell_17_mul_2_readvariableop_resource:22F
3lstm_16_lstm_cell_16_matmul_readvariableop_resource:	2ÈG
5lstm_16_lstm_cell_16_matmul_1_readvariableop_resource:22J
7lstm_16_lstm_cell_16_matmul_1_readvariableop_1_resource:	2ÈC
4lstm_16_lstm_cell_16_biasadd_readvariableop_resource:	ÈD
2lstm_16_lstm_cell_16_mul_2_readvariableop_resource:22K
9time_distributed_8_dense_8_matmul_readvariableop_resource:2H
:time_distributed_8_dense_8_biasadd_readvariableop_resource:
identity¢lstm_16/AssignVariableOp¢lstm_16/AssignVariableOp_1¢lstm_16/ReadVariableOp¢lstm_16/ReadVariableOp_1¢+lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp¢*lstm_16/lstm_cell_16/MatMul/ReadVariableOp¢,lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp¢.lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1¢)lstm_16/lstm_cell_16/mul_2/ReadVariableOp¢lstm_16/while¢lstm_17/AssignVariableOp¢lstm_17/AssignVariableOp_1¢lstm_17/ReadVariableOp¢lstm_17/ReadVariableOp_1¢+lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp¢*lstm_17/lstm_cell_17/MatMul/ReadVariableOp¢,lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp¢.lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1¢)lstm_17/lstm_cell_17/mul_2/ReadVariableOp¢lstm_17/while¢1time_distributed_8/dense_8/BiasAdd/ReadVariableOp¢0time_distributed_8/dense_8/MatMul/ReadVariableOp
lstm_17/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_17/transpose/perm
lstm_17/transpose	Transposeinputslstm_17/transpose/perm:output:0*
T0*"
_output_shapes
:
22
lstm_17/transposes
lstm_17/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2      2
lstm_17/Shape
lstm_17/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_17/strided_slice/stack
lstm_17/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_17/strided_slice/stack_1
lstm_17/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_17/strided_slice/stack_2
lstm_17/strided_sliceStridedSlicelstm_17/Shape:output:0$lstm_17/strided_slice/stack:output:0&lstm_17/strided_slice/stack_1:output:0&lstm_17/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_17/strided_slice
#lstm_17/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2%
#lstm_17/TensorArrayV2/element_shapeÐ
lstm_17/TensorArrayV2TensorListReserve,lstm_17/TensorArrayV2/element_shape:output:0lstm_17/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_17/TensorArrayV2Ï
=lstm_17/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      2?
=lstm_17/TensorArrayUnstack/TensorListFromTensor/element_shape
/lstm_17/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_17/transpose:y:0Flstm_17/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type021
/lstm_17/TensorArrayUnstack/TensorListFromTensor
lstm_17/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_17/strided_slice_1/stack
lstm_17/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_17/strided_slice_1/stack_1
lstm_17/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_17/strided_slice_1/stack_2£
lstm_17/strided_slice_1StridedSlicelstm_17/transpose:y:0&lstm_17/strided_slice_1/stack:output:0(lstm_17/strided_slice_1/stack_1:output:0(lstm_17/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_17/strided_slice_1Í
*lstm_17/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3lstm_17_lstm_cell_17_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02,
*lstm_17/lstm_cell_17/MatMul/ReadVariableOpÄ
lstm_17/lstm_cell_17/MatMulMatMul lstm_17/strided_slice_1:output:02lstm_17/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_17/lstm_cell_17/MatMulÒ
,lstm_17/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5lstm_17_lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02.
,lstm_17/lstm_cell_17/MatMul_1/ReadVariableOpÙ
.lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1ReadVariableOp7lstm_17_lstm_cell_17_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype020
.lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1à
lstm_17/lstm_cell_17/MatMul_1MatMul4lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp:value:06lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_17/lstm_cell_17/MatMul_1·
lstm_17/lstm_cell_17/addAddV2%lstm_17/lstm_cell_17/MatMul:product:0'lstm_17/lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_17/lstm_cell_17/addÌ
+lstm_17/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4lstm_17_lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02-
+lstm_17/lstm_cell_17/BiasAdd/ReadVariableOpÄ
lstm_17/lstm_cell_17/BiasAddBiasAddlstm_17/lstm_cell_17/add:z:03lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_17/lstm_cell_17/BiasAdd
$lstm_17/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2&
$lstm_17/lstm_cell_17/split/split_dimï
lstm_17/lstm_cell_17/splitSplit-lstm_17/lstm_cell_17/split/split_dim:output:0%lstm_17/lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_17/lstm_cell_17/split}
lstm_17/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_17/lstm_cell_17/Const
lstm_17/lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_17/lstm_cell_17/Const_1®
lstm_17/lstm_cell_17/MulMul#lstm_17/lstm_cell_17/split:output:0#lstm_17/lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Mul¯
lstm_17/lstm_cell_17/Add_1AddV2lstm_17/lstm_cell_17/Mul:z:0%lstm_17/lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Add_1¡
,lstm_17/lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,lstm_17/lstm_cell_17/clip_by_value/Minimum/yã
*lstm_17/lstm_cell_17/clip_by_value/MinimumMinimumlstm_17/lstm_cell_17/Add_1:z:05lstm_17/lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222,
*lstm_17/lstm_cell_17/clip_by_value/Minimum
$lstm_17/lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_17/lstm_cell_17/clip_by_value/yÛ
"lstm_17/lstm_cell_17/clip_by_valueMaximum.lstm_17/lstm_cell_17/clip_by_value/Minimum:z:0-lstm_17/lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222$
"lstm_17/lstm_cell_17/clip_by_value
lstm_17/lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_17/lstm_cell_17/Const_2
lstm_17/lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_17/lstm_cell_17/Const_3´
lstm_17/lstm_cell_17/Mul_1Mul#lstm_17/lstm_cell_17/split:output:1%lstm_17/lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Mul_1±
lstm_17/lstm_cell_17/Add_2AddV2lstm_17/lstm_cell_17/Mul_1:z:0%lstm_17/lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Add_2¥
.lstm_17/lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_17/lstm_cell_17/clip_by_value_1/Minimum/yé
,lstm_17/lstm_cell_17/clip_by_value_1/MinimumMinimumlstm_17/lstm_cell_17/Add_2:z:07lstm_17/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222.
,lstm_17/lstm_cell_17/clip_by_value_1/Minimum
&lstm_17/lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_17/lstm_cell_17/clip_by_value_1/yã
$lstm_17/lstm_cell_17/clip_by_value_1Maximum0lstm_17/lstm_cell_17/clip_by_value_1/Minimum:z:0/lstm_17/lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222&
$lstm_17/lstm_cell_17/clip_by_value_1É
)lstm_17/lstm_cell_17/mul_2/ReadVariableOpReadVariableOp2lstm_17_lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02+
)lstm_17/lstm_cell_17/mul_2/ReadVariableOpÅ
lstm_17/lstm_cell_17/mul_2Mul(lstm_17/lstm_cell_17/clip_by_value_1:z:01lstm_17/lstm_cell_17/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/mul_2
lstm_17/lstm_cell_17/TanhTanh#lstm_17/lstm_cell_17/split:output:2*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Tanh¯
lstm_17/lstm_cell_17/mul_3Mul&lstm_17/lstm_cell_17/clip_by_value:z:0lstm_17/lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/mul_3ª
lstm_17/lstm_cell_17/add_3AddV2lstm_17/lstm_cell_17/mul_2:z:0lstm_17/lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/add_3
lstm_17/lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_17/lstm_cell_17/Const_4
lstm_17/lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_17/lstm_cell_17/Const_5´
lstm_17/lstm_cell_17/Mul_4Mul#lstm_17/lstm_cell_17/split:output:3%lstm_17/lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Mul_4±
lstm_17/lstm_cell_17/Add_4AddV2lstm_17/lstm_cell_17/Mul_4:z:0%lstm_17/lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Add_4¥
.lstm_17/lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_17/lstm_cell_17/clip_by_value_2/Minimum/yé
,lstm_17/lstm_cell_17/clip_by_value_2/MinimumMinimumlstm_17/lstm_cell_17/Add_4:z:07lstm_17/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222.
,lstm_17/lstm_cell_17/clip_by_value_2/Minimum
&lstm_17/lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_17/lstm_cell_17/clip_by_value_2/yã
$lstm_17/lstm_cell_17/clip_by_value_2Maximum0lstm_17/lstm_cell_17/clip_by_value_2/Minimum:z:0/lstm_17/lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222&
$lstm_17/lstm_cell_17/clip_by_value_2
lstm_17/lstm_cell_17/Tanh_1Tanhlstm_17/lstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/Tanh_1³
lstm_17/lstm_cell_17/mul_5Mul(lstm_17/lstm_cell_17/clip_by_value_2:z:0lstm_17/lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_17/lstm_cell_17/mul_5
%lstm_17/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2'
%lstm_17/TensorArrayV2_1/element_shapeÖ
lstm_17/TensorArrayV2_1TensorListReserve.lstm_17/TensorArrayV2_1/element_shape:output:0lstm_17/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_17/TensorArrayV2_1^
lstm_17/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_17/time¦
lstm_17/ReadVariableOpReadVariableOp5lstm_17_lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_17/ReadVariableOp§
lstm_17/ReadVariableOp_1ReadVariableOp2lstm_17_lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_17/ReadVariableOp_1
 lstm_17/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2"
 lstm_17/while/maximum_iterationsz
lstm_17/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_17/while/loop_counterí
lstm_17/whileWhile#lstm_17/while/loop_counter:output:0)lstm_17/while/maximum_iterations:output:0lstm_17/time:output:0 lstm_17/TensorArrayV2_1:handle:0lstm_17/ReadVariableOp:value:0 lstm_17/ReadVariableOp_1:value:0lstm_17/strided_slice:output:0?lstm_17/TensorArrayUnstack/TensorListFromTensor:output_handle:03lstm_17_lstm_cell_17_matmul_readvariableop_resource7lstm_17_lstm_cell_17_matmul_1_readvariableop_1_resource4lstm_17_lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *$
bodyR
lstm_17_while_body_51238*$
condR
lstm_17_while_cond_51237*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
lstm_17/whileÅ
8lstm_17/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2:
8lstm_17/TensorArrayV2Stack/TensorListStack/element_shapeÿ
*lstm_17/TensorArrayV2Stack/TensorListStackTensorListStacklstm_17/while:output:3Alstm_17/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
element_dtype02,
*lstm_17/TensorArrayV2Stack/TensorListStack
lstm_17/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
lstm_17/strided_slice_2/stack
lstm_17/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2!
lstm_17/strided_slice_2/stack_1
lstm_17/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_17/strided_slice_2/stack_2Á
lstm_17/strided_slice_2StridedSlice3lstm_17/TensorArrayV2Stack/TensorListStack:tensor:0&lstm_17/strided_slice_2/stack:output:0(lstm_17/strided_slice_2/stack_1:output:0(lstm_17/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2
lstm_17/strided_slice_2
lstm_17/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_17/transpose_1/perm¼
lstm_17/transpose_1	Transpose3lstm_17/TensorArrayV2Stack/TensorListStack:tensor:0!lstm_17/transpose_1/perm:output:0*
T0*"
_output_shapes
:2
22
lstm_17/transpose_1v
lstm_17/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_17/runtime
lstm_17/AssignVariableOpAssignVariableOp5lstm_17_lstm_cell_17_matmul_1_readvariableop_resourcelstm_17/while:output:4^lstm_17/ReadVariableOp-^lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_17/AssignVariableOp
lstm_17/AssignVariableOp_1AssignVariableOp2lstm_17_lstm_cell_17_mul_2_readvariableop_resourcelstm_17/while:output:5^lstm_17/ReadVariableOp_1*^lstm_17/lstm_cell_17/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_17/AssignVariableOp_1
lstm_16/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_16/transpose/perm
lstm_16/transpose	Transposelstm_17/transpose_1:y:0lstm_16/transpose/perm:output:0*
T0*"
_output_shapes
:
222
lstm_16/transposes
lstm_16/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2   2   2
lstm_16/Shape
lstm_16/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_16/strided_slice/stack
lstm_16/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_16/strided_slice/stack_1
lstm_16/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_16/strided_slice/stack_2
lstm_16/strided_sliceStridedSlicelstm_16/Shape:output:0$lstm_16/strided_slice/stack:output:0&lstm_16/strided_slice/stack_1:output:0&lstm_16/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_16/strided_slice
#lstm_16/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2%
#lstm_16/TensorArrayV2/element_shapeÐ
lstm_16/TensorArrayV2TensorListReserve,lstm_16/TensorArrayV2/element_shape:output:0lstm_16/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_16/TensorArrayV2Ï
=lstm_16/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2?
=lstm_16/TensorArrayUnstack/TensorListFromTensor/element_shape
/lstm_16/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_16/transpose:y:0Flstm_16/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type021
/lstm_16/TensorArrayUnstack/TensorListFromTensor
lstm_16/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_16/strided_slice_1/stack
lstm_16/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_16/strided_slice_1/stack_1
lstm_16/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_16/strided_slice_1/stack_2£
lstm_16/strided_slice_1StridedSlicelstm_16/transpose:y:0&lstm_16/strided_slice_1/stack:output:0(lstm_16/strided_slice_1/stack_1:output:0(lstm_16/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2
lstm_16/strided_slice_1Í
*lstm_16/lstm_cell_16/MatMul/ReadVariableOpReadVariableOp3lstm_16_lstm_cell_16_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02,
*lstm_16/lstm_cell_16/MatMul/ReadVariableOpÄ
lstm_16/lstm_cell_16/MatMulMatMul lstm_16/strided_slice_1:output:02lstm_16/lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_16/lstm_cell_16/MatMulÒ
,lstm_16/lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp5lstm_16_lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02.
,lstm_16/lstm_cell_16/MatMul_1/ReadVariableOpÙ
.lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1ReadVariableOp7lstm_16_lstm_cell_16_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype020
.lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1à
lstm_16/lstm_cell_16/MatMul_1MatMul4lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp:value:06lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_16/lstm_cell_16/MatMul_1·
lstm_16/lstm_cell_16/addAddV2%lstm_16/lstm_cell_16/MatMul:product:0'lstm_16/lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_16/lstm_cell_16/addÌ
+lstm_16/lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp4lstm_16_lstm_cell_16_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02-
+lstm_16/lstm_cell_16/BiasAdd/ReadVariableOpÄ
lstm_16/lstm_cell_16/BiasAddBiasAddlstm_16/lstm_cell_16/add:z:03lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_16/lstm_cell_16/BiasAdd
$lstm_16/lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2&
$lstm_16/lstm_cell_16/split/split_dimï
lstm_16/lstm_cell_16/splitSplit-lstm_16/lstm_cell_16/split/split_dim:output:0%lstm_16/lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_16/lstm_cell_16/split}
lstm_16/lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_16/lstm_cell_16/Const
lstm_16/lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_16/lstm_cell_16/Const_1®
lstm_16/lstm_cell_16/MulMul#lstm_16/lstm_cell_16/split:output:0#lstm_16/lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Mul¯
lstm_16/lstm_cell_16/Add_1AddV2lstm_16/lstm_cell_16/Mul:z:0%lstm_16/lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Add_1¡
,lstm_16/lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,lstm_16/lstm_cell_16/clip_by_value/Minimum/yã
*lstm_16/lstm_cell_16/clip_by_value/MinimumMinimumlstm_16/lstm_cell_16/Add_1:z:05lstm_16/lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222,
*lstm_16/lstm_cell_16/clip_by_value/Minimum
$lstm_16/lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_16/lstm_cell_16/clip_by_value/yÛ
"lstm_16/lstm_cell_16/clip_by_valueMaximum.lstm_16/lstm_cell_16/clip_by_value/Minimum:z:0-lstm_16/lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222$
"lstm_16/lstm_cell_16/clip_by_value
lstm_16/lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_16/lstm_cell_16/Const_2
lstm_16/lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_16/lstm_cell_16/Const_3´
lstm_16/lstm_cell_16/Mul_1Mul#lstm_16/lstm_cell_16/split:output:1%lstm_16/lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Mul_1±
lstm_16/lstm_cell_16/Add_2AddV2lstm_16/lstm_cell_16/Mul_1:z:0%lstm_16/lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Add_2¥
.lstm_16/lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_16/lstm_cell_16/clip_by_value_1/Minimum/yé
,lstm_16/lstm_cell_16/clip_by_value_1/MinimumMinimumlstm_16/lstm_cell_16/Add_2:z:07lstm_16/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222.
,lstm_16/lstm_cell_16/clip_by_value_1/Minimum
&lstm_16/lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_16/lstm_cell_16/clip_by_value_1/yã
$lstm_16/lstm_cell_16/clip_by_value_1Maximum0lstm_16/lstm_cell_16/clip_by_value_1/Minimum:z:0/lstm_16/lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222&
$lstm_16/lstm_cell_16/clip_by_value_1É
)lstm_16/lstm_cell_16/mul_2/ReadVariableOpReadVariableOp2lstm_16_lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02+
)lstm_16/lstm_cell_16/mul_2/ReadVariableOpÅ
lstm_16/lstm_cell_16/mul_2Mul(lstm_16/lstm_cell_16/clip_by_value_1:z:01lstm_16/lstm_cell_16/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/mul_2
lstm_16/lstm_cell_16/TanhTanh#lstm_16/lstm_cell_16/split:output:2*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Tanh¯
lstm_16/lstm_cell_16/mul_3Mul&lstm_16/lstm_cell_16/clip_by_value:z:0lstm_16/lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/mul_3ª
lstm_16/lstm_cell_16/add_3AddV2lstm_16/lstm_cell_16/mul_2:z:0lstm_16/lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/add_3
lstm_16/lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_16/lstm_cell_16/Const_4
lstm_16/lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_16/lstm_cell_16/Const_5´
lstm_16/lstm_cell_16/Mul_4Mul#lstm_16/lstm_cell_16/split:output:3%lstm_16/lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Mul_4±
lstm_16/lstm_cell_16/Add_4AddV2lstm_16/lstm_cell_16/Mul_4:z:0%lstm_16/lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Add_4¥
.lstm_16/lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?20
.lstm_16/lstm_cell_16/clip_by_value_2/Minimum/yé
,lstm_16/lstm_cell_16/clip_by_value_2/MinimumMinimumlstm_16/lstm_cell_16/Add_4:z:07lstm_16/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222.
,lstm_16/lstm_cell_16/clip_by_value_2/Minimum
&lstm_16/lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2(
&lstm_16/lstm_cell_16/clip_by_value_2/yã
$lstm_16/lstm_cell_16/clip_by_value_2Maximum0lstm_16/lstm_cell_16/clip_by_value_2/Minimum:z:0/lstm_16/lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222&
$lstm_16/lstm_cell_16/clip_by_value_2
lstm_16/lstm_cell_16/Tanh_1Tanhlstm_16/lstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/Tanh_1³
lstm_16/lstm_cell_16/mul_5Mul(lstm_16/lstm_cell_16/clip_by_value_2:z:0lstm_16/lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_16/lstm_cell_16/mul_5
%lstm_16/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2'
%lstm_16/TensorArrayV2_1/element_shapeÖ
lstm_16/TensorArrayV2_1TensorListReserve.lstm_16/TensorArrayV2_1/element_shape:output:0lstm_16/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_16/TensorArrayV2_1^
lstm_16/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_16/time¦
lstm_16/ReadVariableOpReadVariableOp5lstm_16_lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_16/ReadVariableOp§
lstm_16/ReadVariableOp_1ReadVariableOp2lstm_16_lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_16/ReadVariableOp_1
 lstm_16/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
ÿÿÿÿÿÿÿÿÿ2"
 lstm_16/while/maximum_iterationsz
lstm_16/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_16/while/loop_counterí
lstm_16/whileWhile#lstm_16/while/loop_counter:output:0)lstm_16/while/maximum_iterations:output:0lstm_16/time:output:0 lstm_16/TensorArrayV2_1:handle:0lstm_16/ReadVariableOp:value:0 lstm_16/ReadVariableOp_1:value:0lstm_16/strided_slice:output:0?lstm_16/TensorArrayUnstack/TensorListFromTensor:output_handle:03lstm_16_lstm_cell_16_matmul_readvariableop_resource7lstm_16_lstm_cell_16_matmul_1_readvariableop_1_resource4lstm_16_lstm_cell_16_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *$
bodyR
lstm_16_while_body_51412*$
condR
lstm_16_while_cond_51411*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
lstm_16/whileÅ
8lstm_16/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2:
8lstm_16/TensorArrayV2Stack/TensorListStack/element_shapeÿ
*lstm_16/TensorArrayV2Stack/TensorListStackTensorListStacklstm_16/while:output:3Alstm_16/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
element_dtype02,
*lstm_16/TensorArrayV2Stack/TensorListStack
lstm_16/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
ÿÿÿÿÿÿÿÿÿ2
lstm_16/strided_slice_2/stack
lstm_16/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2!
lstm_16/strided_slice_2/stack_1
lstm_16/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2!
lstm_16/strided_slice_2/stack_2Á
lstm_16/strided_slice_2StridedSlice3lstm_16/TensorArrayV2Stack/TensorListStack:tensor:0&lstm_16/strided_slice_2/stack:output:0(lstm_16/strided_slice_2/stack_1:output:0(lstm_16/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2
lstm_16/strided_slice_2
lstm_16/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_16/transpose_1/perm¼
lstm_16/transpose_1	Transpose3lstm_16/TensorArrayV2Stack/TensorListStack:tensor:0!lstm_16/transpose_1/perm:output:0*
T0*"
_output_shapes
:2
22
lstm_16/transpose_1v
lstm_16/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_16/runtime
lstm_16/AssignVariableOpAssignVariableOp5lstm_16_lstm_cell_16_matmul_1_readvariableop_resourcelstm_16/while:output:4^lstm_16/ReadVariableOp-^lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_16/AssignVariableOp
lstm_16/AssignVariableOp_1AssignVariableOp2lstm_16_lstm_cell_16_mul_2_readvariableop_resourcelstm_16/while:output:5^lstm_16/ReadVariableOp_1*^lstm_16/lstm_cell_16/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_16/AssignVariableOp_1
 time_distributed_8/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2"
 time_distributed_8/Reshape/shape±
time_distributed_8/ReshapeReshapelstm_16/transpose_1:y:0)time_distributed_8/Reshape/shape:output:0*
T0*
_output_shapes
:	ô22
time_distributed_8/ReshapeÞ
0time_distributed_8/dense_8/MatMul/ReadVariableOpReadVariableOp9time_distributed_8_dense_8_matmul_readvariableop_resource*
_output_shapes

:2*
dtype022
0time_distributed_8/dense_8/MatMul/ReadVariableOpÙ
!time_distributed_8/dense_8/MatMulMatMul#time_distributed_8/Reshape:output:08time_distributed_8/dense_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô2#
!time_distributed_8/dense_8/MatMulÝ
1time_distributed_8/dense_8/BiasAdd/ReadVariableOpReadVariableOp:time_distributed_8_dense_8_biasadd_readvariableop_resource*
_output_shapes
:*
dtype023
1time_distributed_8/dense_8/BiasAdd/ReadVariableOpå
"time_distributed_8/dense_8/BiasAddBiasAdd+time_distributed_8/dense_8/MatMul:product:09time_distributed_8/dense_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô2$
"time_distributed_8/dense_8/BiasAdd
"time_distributed_8/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      2$
"time_distributed_8/Reshape_1/shapeÎ
time_distributed_8/Reshape_1Reshape+time_distributed_8/dense_8/BiasAdd:output:0+time_distributed_8/Reshape_1/shape:output:0*
T0*"
_output_shapes
:2
2
time_distributed_8/Reshape_1
"time_distributed_8/Reshape_2/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2$
"time_distributed_8/Reshape_2/shape·
time_distributed_8/Reshape_2Reshapelstm_16/transpose_1:y:0+time_distributed_8/Reshape_2/shape:output:0*
T0*
_output_shapes
:	ô22
time_distributed_8/Reshape_2{
IdentityIdentity%time_distributed_8/Reshape_1:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identityû
NoOpNoOp^lstm_16/AssignVariableOp^lstm_16/AssignVariableOp_1^lstm_16/ReadVariableOp^lstm_16/ReadVariableOp_1,^lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp+^lstm_16/lstm_cell_16/MatMul/ReadVariableOp-^lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp/^lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1*^lstm_16/lstm_cell_16/mul_2/ReadVariableOp^lstm_16/while^lstm_17/AssignVariableOp^lstm_17/AssignVariableOp_1^lstm_17/ReadVariableOp^lstm_17/ReadVariableOp_1,^lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp+^lstm_17/lstm_cell_17/MatMul/ReadVariableOp-^lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp/^lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1*^lstm_17/lstm_cell_17/mul_2/ReadVariableOp^lstm_17/while2^time_distributed_8/dense_8/BiasAdd/ReadVariableOp1^time_distributed_8/dense_8/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 24
lstm_16/AssignVariableOplstm_16/AssignVariableOp28
lstm_16/AssignVariableOp_1lstm_16/AssignVariableOp_120
lstm_16/ReadVariableOplstm_16/ReadVariableOp24
lstm_16/ReadVariableOp_1lstm_16/ReadVariableOp_12Z
+lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp+lstm_16/lstm_cell_16/BiasAdd/ReadVariableOp2X
*lstm_16/lstm_cell_16/MatMul/ReadVariableOp*lstm_16/lstm_cell_16/MatMul/ReadVariableOp2\
,lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp,lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp2`
.lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_1.lstm_16/lstm_cell_16/MatMul_1/ReadVariableOp_12V
)lstm_16/lstm_cell_16/mul_2/ReadVariableOp)lstm_16/lstm_cell_16/mul_2/ReadVariableOp2
lstm_16/whilelstm_16/while24
lstm_17/AssignVariableOplstm_17/AssignVariableOp28
lstm_17/AssignVariableOp_1lstm_17/AssignVariableOp_120
lstm_17/ReadVariableOplstm_17/ReadVariableOp24
lstm_17/ReadVariableOp_1lstm_17/ReadVariableOp_12Z
+lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp+lstm_17/lstm_cell_17/BiasAdd/ReadVariableOp2X
*lstm_17/lstm_cell_17/MatMul/ReadVariableOp*lstm_17/lstm_cell_17/MatMul/ReadVariableOp2\
,lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp,lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp2`
.lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_1.lstm_17/lstm_cell_17/MatMul_1/ReadVariableOp_12V
)lstm_17/lstm_cell_17/mul_2/ReadVariableOp)lstm_17/lstm_cell_17/mul_2/ReadVariableOp2
lstm_17/whilelstm_17/while2f
1time_distributed_8/dense_8/BiasAdd/ReadVariableOp1time_distributed_8/dense_8/BiasAdd/ReadVariableOp2d
0time_distributed_8/dense_8/MatMul/ReadVariableOp0time_distributed_8/dense_8/MatMul/ReadVariableOp:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
o
¶
B__inference_lstm_16_layer_call_and_return_conditional_losses_52901
inputs_0>
+lstm_cell_16_matmul_readvariableop_resource:	2È?
-lstm_cell_16_matmul_1_readvariableop_resource:22B
/lstm_cell_16_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_16_biasadd_readvariableop_resource:	È<
*lstm_cell_16_mul_2_readvariableop_resource:22
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_16/BiasAdd/ReadVariableOp¢"lstm_cell_16/MatMul/ReadVariableOp¢$lstm_cell_16/MatMul_1/ReadVariableOp¢&lstm_cell_16/MatMul_1/ReadVariableOp_1¢!lstm_cell_16/mul_2/ReadVariableOp¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ222
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_16/MatMul/ReadVariableOpReadVariableOp+lstm_cell_16_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02$
"lstm_cell_16/MatMul/ReadVariableOp¤
lstm_cell_16/MatMulMatMulstrided_slice_1:output:0*lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/MatMulº
$lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02&
$lstm_cell_16/MatMul_1/ReadVariableOpÁ
&lstm_cell_16/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_16_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_16/MatMul_1/ReadVariableOp_1À
lstm_cell_16/MatMul_1MatMul,lstm_cell_16/MatMul_1/ReadVariableOp:value:0.lstm_cell_16/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/MatMul_1
lstm_cell_16/addAddV2lstm_cell_16/MatMul:product:0lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/add´
#lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_16_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_16/BiasAdd/ReadVariableOp¤
lstm_cell_16/BiasAddBiasAddlstm_cell_16/add:z:0+lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/BiasAdd~
lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_16/split/split_dimÏ
lstm_cell_16/splitSplit%lstm_cell_16/split/split_dim:output:0lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_16/splitm
lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Constq
lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_1
lstm_cell_16/MulMullstm_cell_16/split:output:0lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul
lstm_cell_16/Add_1AddV2lstm_cell_16/Mul:z:0lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_1
$lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_16/clip_by_value/Minimum/yÃ
"lstm_cell_16/clip_by_value/MinimumMinimumlstm_cell_16/Add_1:z:0-lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222$
"lstm_cell_16/clip_by_value/Minimum
lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_16/clip_by_value/y»
lstm_cell_16/clip_by_valueMaximum&lstm_cell_16/clip_by_value/Minimum:z:0%lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_valueq
lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Const_2q
lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_3
lstm_cell_16/Mul_1Mullstm_cell_16/split:output:1lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul_1
lstm_cell_16/Add_2AddV2lstm_cell_16/Mul_1:z:0lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_2
&lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_16/clip_by_value_1/Minimum/yÉ
$lstm_cell_16/clip_by_value_1/MinimumMinimumlstm_cell_16/Add_2:z:0/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_16/clip_by_value_1/Minimum
lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_16/clip_by_value_1/yÃ
lstm_cell_16/clip_by_value_1Maximum(lstm_cell_16/clip_by_value_1/Minimum:z:0'lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_value_1±
!lstm_cell_16/mul_2/ReadVariableOpReadVariableOp*lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02#
!lstm_cell_16/mul_2/ReadVariableOp¥
lstm_cell_16/mul_2Mul lstm_cell_16/clip_by_value_1:z:0)lstm_cell_16/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_2t
lstm_cell_16/TanhTanhlstm_cell_16/split:output:2*
T0*
_output_shapes

:222
lstm_cell_16/Tanh
lstm_cell_16/mul_3Mullstm_cell_16/clip_by_value:z:0lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_3
lstm_cell_16/add_3AddV2lstm_cell_16/mul_2:z:0lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_16/add_3q
lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Const_4q
lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_5
lstm_cell_16/Mul_4Mullstm_cell_16/split:output:3lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul_4
lstm_cell_16/Add_4AddV2lstm_cell_16/Mul_4:z:0lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_4
&lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_16/clip_by_value_2/Minimum/yÉ
$lstm_cell_16/clip_by_value_2/MinimumMinimumlstm_cell_16/Add_4:z:0/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_16/clip_by_value_2/Minimum
lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_16/clip_by_value_2/yÃ
lstm_cell_16/clip_by_value_2Maximum(lstm_cell_16/clip_by_value_2/Minimum:z:0'lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_value_2s
lstm_cell_16/Tanh_1Tanhlstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_16/Tanh_1
lstm_cell_16/mul_5Mul lstm_cell_16/clip_by_value_2:z:0lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp-lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_16_matmul_readvariableop_resource/lstm_cell_16_matmul_1_readvariableop_1_resource,lstm_cell_16_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_52796*
condR
while_cond_52795*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22*
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

:22*
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
:2ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_16_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_16/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_16_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_16/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_16/BiasAdd/ReadVariableOp#^lstm_cell_16/MatMul/ReadVariableOp%^lstm_cell_16/MatMul_1/ReadVariableOp'^lstm_cell_16/MatMul_1/ReadVariableOp_1"^lstm_cell_16/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2ÿÿÿÿÿÿÿÿÿ2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_16/BiasAdd/ReadVariableOp#lstm_cell_16/BiasAdd/ReadVariableOp2H
"lstm_cell_16/MatMul/ReadVariableOp"lstm_cell_16/MatMul/ReadVariableOp2L
$lstm_cell_16/MatMul_1/ReadVariableOp$lstm_cell_16/MatMul_1/ReadVariableOp2P
&lstm_cell_16/MatMul_1/ReadVariableOp_1&lstm_cell_16/MatMul_1/ReadVariableOp_12F
!lstm_cell_16/mul_2/ReadVariableOp!lstm_cell_16/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ2
"
_user_specified_name
inputs/0
æ
õ
,__inference_lstm_cell_17_layer_call_fn_53929

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
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_539162
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
$:2:22:22: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:2
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
ÔY
Ë
while_body_52202
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_17_matmul_readvariableop_resource_0:	ÈH
5while_lstm_cell_17_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_17_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_17_matmul_readvariableop_resource:	ÈF
3while_lstm_cell_17_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_17_biasadd_readvariableop_resource:	È¢)while/lstm_cell_17/BiasAdd/ReadVariableOp¢(while/lstm_cell_17/MatMul/ReadVariableOp¢*while/lstm_cell_17/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02*
(while/lstm_cell_17/MatMul/ReadVariableOpÎ
while/lstm_cell_17/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/MatMulÏ
*while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_17/MatMul_1/ReadVariableOp·
while/lstm_cell_17/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/MatMul_1¯
while/lstm_cell_17/addAddV2#while/lstm_cell_17/MatMul:product:0%while/lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/addÈ
)while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_17/BiasAdd/ReadVariableOp¼
while/lstm_cell_17/BiasAddBiasAddwhile/lstm_cell_17/add:z:01while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/BiasAdd
"while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_17/split/split_dimç
while/lstm_cell_17/splitSplit+while/lstm_cell_17/split/split_dim:output:0#while/lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_17/splity
while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const}
while/lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_1¦
while/lstm_cell_17/MulMul!while/lstm_cell_17/split:output:0!while/lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul§
while/lstm_cell_17/Add_1AddV2while/lstm_cell_17/Mul:z:0#while/lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_1
*while/lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_17/clip_by_value/Minimum/yÛ
(while/lstm_cell_17/clip_by_value/MinimumMinimumwhile/lstm_cell_17/Add_1:z:03while/lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(while/lstm_cell_17/clip_by_value/Minimum
"while/lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_17/clip_by_value/yÓ
 while/lstm_cell_17/clip_by_valueMaximum,while/lstm_cell_17/clip_by_value/Minimum:z:0+while/lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 while/lstm_cell_17/clip_by_value}
while/lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const_2}
while/lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_3¬
while/lstm_cell_17/Mul_1Mul!while/lstm_cell_17/split:output:1#while/lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul_1©
while/lstm_cell_17/Add_2AddV2while/lstm_cell_17/Mul_1:z:0#while/lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_2¡
,while/lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_17/clip_by_value_1/Minimum/yá
*while/lstm_cell_17/clip_by_value_1/MinimumMinimumwhile/lstm_cell_17/Add_2:z:05while/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_17/clip_by_value_1/Minimum
$while/lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_17/clip_by_value_1/yÛ
"while/lstm_cell_17/clip_by_value_1Maximum.while/lstm_cell_17/clip_by_value_1/Minimum:z:0-while/lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_17/clip_by_value_1¡
while/lstm_cell_17/mul_2Mul&while/lstm_cell_17/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_2
while/lstm_cell_17/TanhTanh!while/lstm_cell_17/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_17/Tanh§
while/lstm_cell_17/mul_3Mul$while/lstm_cell_17/clip_by_value:z:0while/lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_3¢
while/lstm_cell_17/add_3AddV2while/lstm_cell_17/mul_2:z:0while/lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_17/add_3}
while/lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const_4}
while/lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_5¬
while/lstm_cell_17/Mul_4Mul!while/lstm_cell_17/split:output:3#while/lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul_4©
while/lstm_cell_17/Add_4AddV2while/lstm_cell_17/Mul_4:z:0#while/lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_4¡
,while/lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_17/clip_by_value_2/Minimum/yá
*while/lstm_cell_17/clip_by_value_2/MinimumMinimumwhile/lstm_cell_17/Add_4:z:05while/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_17/clip_by_value_2/Minimum
$while/lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_17/clip_by_value_2/yÛ
"while/lstm_cell_17/clip_by_value_2Maximum.while/lstm_cell_17/clip_by_value_2/Minimum:z:0-while/lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_17/clip_by_value_2
while/lstm_cell_17/Tanh_1Tanhwhile/lstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Tanh_1«
while/lstm_cell_17/mul_5Mul&while/lstm_cell_17/clip_by_value_2:z:0while/lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_17/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_17/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_17/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*"
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
2while_lstm_cell_17_biasadd_readvariableop_resource4while_lstm_cell_17_biasadd_readvariableop_resource_0"l
3while_lstm_cell_17_matmul_1_readvariableop_resource5while_lstm_cell_17_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_17_matmul_readvariableop_resource3while_lstm_cell_17_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_17/BiasAdd/ReadVariableOp)while/lstm_cell_17/BiasAdd/ReadVariableOp2T
(while/lstm_cell_17/MatMul/ReadVariableOp(while/lstm_cell_17/MatMul/ReadVariableOp2X
*while/lstm_cell_17/MatMul_1/ReadVariableOp*while/lstm_cell_17/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
i
Ë

lstm_16_while_body_51776,
(lstm_16_while_lstm_16_while_loop_counter2
.lstm_16_while_lstm_16_while_maximum_iterations
lstm_16_while_placeholder
lstm_16_while_placeholder_1
lstm_16_while_placeholder_2
lstm_16_while_placeholder_3)
%lstm_16_while_lstm_16_strided_slice_0g
clstm_16_while_tensorarrayv2read_tensorlistgetitem_lstm_16_tensorarrayunstack_tensorlistfromtensor_0N
;lstm_16_while_lstm_cell_16_matmul_readvariableop_resource_0:	2ÈP
=lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource_0:	2ÈK
<lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource_0:	È
lstm_16_while_identity
lstm_16_while_identity_1
lstm_16_while_identity_2
lstm_16_while_identity_3
lstm_16_while_identity_4
lstm_16_while_identity_5'
#lstm_16_while_lstm_16_strided_slicee
alstm_16_while_tensorarrayv2read_tensorlistgetitem_lstm_16_tensorarrayunstack_tensorlistfromtensorL
9lstm_16_while_lstm_cell_16_matmul_readvariableop_resource:	2ÈN
;lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource:	2ÈI
:lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource:	È¢1lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp¢0lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp¢2lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOpÓ
?lstm_16/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2A
?lstm_16/while/TensorArrayV2Read/TensorListGetItem/element_shapeú
1lstm_16/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemclstm_16_while_tensorarrayv2read_tensorlistgetitem_lstm_16_tensorarrayunstack_tensorlistfromtensor_0lstm_16_while_placeholderHlstm_16/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype023
1lstm_16/while/TensorArrayV2Read/TensorListGetItemá
0lstm_16/while/lstm_cell_16/MatMul/ReadVariableOpReadVariableOp;lstm_16_while_lstm_cell_16_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype022
0lstm_16/while/lstm_cell_16/MatMul/ReadVariableOpî
!lstm_16/while/lstm_cell_16/MatMulMatMul8lstm_16/while/TensorArrayV2Read/TensorListGetItem:item:08lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2#
!lstm_16/while/lstm_cell_16/MatMulç
2lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp=lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype024
2lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp×
#lstm_16/while/lstm_cell_16/MatMul_1MatMullstm_16_while_placeholder_2:lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2%
#lstm_16/while/lstm_cell_16/MatMul_1Ï
lstm_16/while/lstm_cell_16/addAddV2+lstm_16/while/lstm_cell_16/MatMul:product:0-lstm_16/while/lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2 
lstm_16/while/lstm_cell_16/addà
1lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp<lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype023
1lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOpÜ
"lstm_16/while/lstm_cell_16/BiasAddBiasAdd"lstm_16/while/lstm_cell_16/add:z:09lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2$
"lstm_16/while/lstm_cell_16/BiasAdd
*lstm_16/while/lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2,
*lstm_16/while/lstm_cell_16/split/split_dim
 lstm_16/while/lstm_cell_16/splitSplit3lstm_16/while/lstm_cell_16/split/split_dim:output:0+lstm_16/while/lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2"
 lstm_16/while/lstm_cell_16/split
 lstm_16/while/lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2"
 lstm_16/while/lstm_cell_16/Const
"lstm_16/while/lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_16/while/lstm_cell_16/Const_1Æ
lstm_16/while/lstm_cell_16/MulMul)lstm_16/while/lstm_cell_16/split:output:0)lstm_16/while/lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222 
lstm_16/while/lstm_cell_16/MulÇ
 lstm_16/while/lstm_cell_16/Add_1AddV2"lstm_16/while/lstm_cell_16/Mul:z:0+lstm_16/while/lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/Add_1­
2lstm_16/while/lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?24
2lstm_16/while/lstm_cell_16/clip_by_value/Minimum/yû
0lstm_16/while/lstm_cell_16/clip_by_value/MinimumMinimum$lstm_16/while/lstm_cell_16/Add_1:z:0;lstm_16/while/lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:2222
0lstm_16/while/lstm_cell_16/clip_by_value/Minimum
*lstm_16/while/lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_16/while/lstm_cell_16/clip_by_value/yó
(lstm_16/while/lstm_cell_16/clip_by_valueMaximum4lstm_16/while/lstm_cell_16/clip_by_value/Minimum:z:03lstm_16/while/lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222*
(lstm_16/while/lstm_cell_16/clip_by_value
"lstm_16/while/lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_16/while/lstm_cell_16/Const_2
"lstm_16/while/lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_16/while/lstm_cell_16/Const_3Ì
 lstm_16/while/lstm_cell_16/Mul_1Mul)lstm_16/while/lstm_cell_16/split:output:1+lstm_16/while/lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/Mul_1É
 lstm_16/while/lstm_cell_16/Add_2AddV2$lstm_16/while/lstm_cell_16/Mul_1:z:0+lstm_16/while/lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/Add_2±
4lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum/y
2lstm_16/while/lstm_cell_16/clip_by_value_1/MinimumMinimum$lstm_16/while/lstm_cell_16/Add_2:z:0=lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:2224
2lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum¡
,lstm_16/while/lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_16/while/lstm_cell_16/clip_by_value_1/yû
*lstm_16/while/lstm_cell_16/clip_by_value_1Maximum6lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum:z:05lstm_16/while/lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222,
*lstm_16/while/lstm_cell_16/clip_by_value_1Á
 lstm_16/while/lstm_cell_16/mul_2Mul.lstm_16/while/lstm_cell_16/clip_by_value_1:z:0lstm_16_while_placeholder_3*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/mul_2
lstm_16/while/lstm_cell_16/TanhTanh)lstm_16/while/lstm_cell_16/split:output:2*
T0*
_output_shapes

:222!
lstm_16/while/lstm_cell_16/TanhÇ
 lstm_16/while/lstm_cell_16/mul_3Mul,lstm_16/while/lstm_cell_16/clip_by_value:z:0#lstm_16/while/lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/mul_3Â
 lstm_16/while/lstm_cell_16/add_3AddV2$lstm_16/while/lstm_cell_16/mul_2:z:0$lstm_16/while/lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/add_3
"lstm_16/while/lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2$
"lstm_16/while/lstm_cell_16/Const_4
"lstm_16/while/lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2$
"lstm_16/while/lstm_cell_16/Const_5Ì
 lstm_16/while/lstm_cell_16/Mul_4Mul)lstm_16/while/lstm_cell_16/split:output:3+lstm_16/while/lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/Mul_4É
 lstm_16/while/lstm_cell_16/Add_4AddV2$lstm_16/while/lstm_cell_16/Mul_4:z:0+lstm_16/while/lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/Add_4±
4lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?26
4lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum/y
2lstm_16/while/lstm_cell_16/clip_by_value_2/MinimumMinimum$lstm_16/while/lstm_cell_16/Add_4:z:0=lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:2224
2lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum¡
,lstm_16/while/lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2.
,lstm_16/while/lstm_cell_16/clip_by_value_2/yû
*lstm_16/while/lstm_cell_16/clip_by_value_2Maximum6lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum:z:05lstm_16/while/lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222,
*lstm_16/while/lstm_cell_16/clip_by_value_2
!lstm_16/while/lstm_cell_16/Tanh_1Tanh$lstm_16/while/lstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222#
!lstm_16/while/lstm_cell_16/Tanh_1Ë
 lstm_16/while/lstm_cell_16/mul_5Mul.lstm_16/while/lstm_cell_16/clip_by_value_2:z:0%lstm_16/while/lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222"
 lstm_16/while/lstm_cell_16/mul_5
2lstm_16/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_16_while_placeholder_1lstm_16_while_placeholder$lstm_16/while/lstm_cell_16/mul_5:z:0*
_output_shapes
: *
element_dtype024
2lstm_16/while/TensorArrayV2Write/TensorListSetIteml
lstm_16/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_16/while/add/y
lstm_16/while/addAddV2lstm_16_while_placeholderlstm_16/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_16/while/addp
lstm_16/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_16/while/add_1/y
lstm_16/while/add_1AddV2(lstm_16_while_lstm_16_while_loop_counterlstm_16/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_16/while/add_1
lstm_16/while/IdentityIdentitylstm_16/while/add_1:z:0^lstm_16/while/NoOp*
T0*
_output_shapes
: 2
lstm_16/while/Identity¦
lstm_16/while/Identity_1Identity.lstm_16_while_lstm_16_while_maximum_iterations^lstm_16/while/NoOp*
T0*
_output_shapes
: 2
lstm_16/while/Identity_1
lstm_16/while/Identity_2Identitylstm_16/while/add:z:0^lstm_16/while/NoOp*
T0*
_output_shapes
: 2
lstm_16/while/Identity_2º
lstm_16/while/Identity_3IdentityBlstm_16/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_16/while/NoOp*
T0*
_output_shapes
: 2
lstm_16/while/Identity_3¤
lstm_16/while/Identity_4Identity$lstm_16/while/lstm_cell_16/mul_5:z:0^lstm_16/while/NoOp*
T0*
_output_shapes

:222
lstm_16/while/Identity_4¤
lstm_16/while/Identity_5Identity$lstm_16/while/lstm_cell_16/add_3:z:0^lstm_16/while/NoOp*
T0*
_output_shapes

:222
lstm_16/while/Identity_5
lstm_16/while/NoOpNoOp2^lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp1^lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp3^lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_16/while/NoOp"9
lstm_16_while_identitylstm_16/while/Identity:output:0"=
lstm_16_while_identity_1!lstm_16/while/Identity_1:output:0"=
lstm_16_while_identity_2!lstm_16/while/Identity_2:output:0"=
lstm_16_while_identity_3!lstm_16/while/Identity_3:output:0"=
lstm_16_while_identity_4!lstm_16/while/Identity_4:output:0"=
lstm_16_while_identity_5!lstm_16/while/Identity_5:output:0"L
#lstm_16_while_lstm_16_strided_slice%lstm_16_while_lstm_16_strided_slice_0"z
:lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource<lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource_0"|
;lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource=lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource_0"x
9lstm_16_while_lstm_cell_16_matmul_readvariableop_resource;lstm_16_while_lstm_cell_16_matmul_readvariableop_resource_0"È
alstm_16_while_tensorarrayv2read_tensorlistgetitem_lstm_16_tensorarrayunstack_tensorlistfromtensorclstm_16_while_tensorarrayv2read_tensorlistgetitem_lstm_16_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2f
1lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp1lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp2d
0lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp0lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp2h
2lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp2lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
å$
Ø
while_body_49242
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
while_lstm_cell_16_49319_0:	2È-
while_lstm_cell_16_49321_0:	2È)
while_lstm_cell_16_49323_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
while_lstm_cell_16_49319:	2È+
while_lstm_cell_16_49321:	2È'
while_lstm_cell_16_49323:	È¢*while/lstm_cell_16/StatefulPartitionedCallÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÂ
*while/lstm_cell_16/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_16_49319_0while_lstm_cell_16_49321_0while_lstm_cell_16_49323_0*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:22:22:22*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_493182,
*while/lstm_cell_16/StatefulPartitionedCall÷
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder3while/lstm_cell_16/StatefulPartitionedCall:output:0*
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
while/Identity_4Identity3while/lstm_cell_16/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identity3while/lstm_cell_16/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5

while/NoOpNoOp+^while/lstm_cell_16/StatefulPartitionedCall*"
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
while_lstm_cell_16_49319while_lstm_cell_16_49319_0"6
while_lstm_cell_16_49321while_lstm_cell_16_49321_0"6
while_lstm_cell_16_49323while_lstm_cell_16_49323_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2X
*while/lstm_cell_16/StatefulPartitionedCall*while/lstm_cell_16/StatefulPartitionedCall: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 

ì
%sequential_8_lstm_17_while_body_48078F
Bsequential_8_lstm_17_while_sequential_8_lstm_17_while_loop_counterL
Hsequential_8_lstm_17_while_sequential_8_lstm_17_while_maximum_iterations*
&sequential_8_lstm_17_while_placeholder,
(sequential_8_lstm_17_while_placeholder_1,
(sequential_8_lstm_17_while_placeholder_2,
(sequential_8_lstm_17_while_placeholder_3C
?sequential_8_lstm_17_while_sequential_8_lstm_17_strided_slice_0
}sequential_8_lstm_17_while_tensorarrayv2read_tensorlistgetitem_sequential_8_lstm_17_tensorarrayunstack_tensorlistfromtensor_0[
Hsequential_8_lstm_17_while_lstm_cell_17_matmul_readvariableop_resource_0:	È]
Jsequential_8_lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource_0:	2ÈX
Isequential_8_lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource_0:	È'
#sequential_8_lstm_17_while_identity)
%sequential_8_lstm_17_while_identity_1)
%sequential_8_lstm_17_while_identity_2)
%sequential_8_lstm_17_while_identity_3)
%sequential_8_lstm_17_while_identity_4)
%sequential_8_lstm_17_while_identity_5A
=sequential_8_lstm_17_while_sequential_8_lstm_17_strided_slice
{sequential_8_lstm_17_while_tensorarrayv2read_tensorlistgetitem_sequential_8_lstm_17_tensorarrayunstack_tensorlistfromtensorY
Fsequential_8_lstm_17_while_lstm_cell_17_matmul_readvariableop_resource:	È[
Hsequential_8_lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource:	2ÈV
Gsequential_8_lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource:	È¢>sequential_8/lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp¢=sequential_8/lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp¢?sequential_8/lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOpí
Lsequential_8/lstm_17/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      2N
Lsequential_8/lstm_17/while/TensorArrayV2Read/TensorListGetItem/element_shapeÈ
>sequential_8/lstm_17/while/TensorArrayV2Read/TensorListGetItemTensorListGetItem}sequential_8_lstm_17_while_tensorarrayv2read_tensorlistgetitem_sequential_8_lstm_17_tensorarrayunstack_tensorlistfromtensor_0&sequential_8_lstm_17_while_placeholderUsequential_8/lstm_17/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02@
>sequential_8/lstm_17/while/TensorArrayV2Read/TensorListGetItem
=sequential_8/lstm_17/while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOpHsequential_8_lstm_17_while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02?
=sequential_8/lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp¢
.sequential_8/lstm_17/while/lstm_cell_17/MatMulMatMulEsequential_8/lstm_17/while/TensorArrayV2Read/TensorListGetItem:item:0Esequential_8/lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È20
.sequential_8/lstm_17/while/lstm_cell_17/MatMul
?sequential_8/lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOpJsequential_8_lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02A
?sequential_8/lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp
0sequential_8/lstm_17/while/lstm_cell_17/MatMul_1MatMul(sequential_8_lstm_17_while_placeholder_2Gsequential_8/lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È22
0sequential_8/lstm_17/while/lstm_cell_17/MatMul_1
+sequential_8/lstm_17/while/lstm_cell_17/addAddV28sequential_8/lstm_17/while/lstm_cell_17/MatMul:product:0:sequential_8/lstm_17/while/lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2-
+sequential_8/lstm_17/while/lstm_cell_17/add
>sequential_8/lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOpIsequential_8_lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02@
>sequential_8/lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp
/sequential_8/lstm_17/while/lstm_cell_17/BiasAddBiasAdd/sequential_8/lstm_17/while/lstm_cell_17/add:z:0Fsequential_8/lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È21
/sequential_8/lstm_17/while/lstm_cell_17/BiasAdd´
7sequential_8/lstm_17/while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :29
7sequential_8/lstm_17/while/lstm_cell_17/split/split_dim»
-sequential_8/lstm_17/while/lstm_cell_17/splitSplit@sequential_8/lstm_17/while/lstm_cell_17/split/split_dim:output:08sequential_8/lstm_17/while/lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2/
-sequential_8/lstm_17/while/lstm_cell_17/split£
-sequential_8/lstm_17/while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2/
-sequential_8/lstm_17/while/lstm_cell_17/Const§
/sequential_8/lstm_17/while/lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?21
/sequential_8/lstm_17/while/lstm_cell_17/Const_1ú
+sequential_8/lstm_17/while/lstm_cell_17/MulMul6sequential_8/lstm_17/while/lstm_cell_17/split:output:06sequential_8/lstm_17/while/lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222-
+sequential_8/lstm_17/while/lstm_cell_17/Mulû
-sequential_8/lstm_17/while/lstm_cell_17/Add_1AddV2/sequential_8/lstm_17/while/lstm_cell_17/Mul:z:08sequential_8/lstm_17/while/lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_17/while/lstm_cell_17/Add_1Ç
?sequential_8/lstm_17/while/lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2A
?sequential_8/lstm_17/while/lstm_cell_17/clip_by_value/Minimum/y¯
=sequential_8/lstm_17/while/lstm_cell_17/clip_by_value/MinimumMinimum1sequential_8/lstm_17/while/lstm_cell_17/Add_1:z:0Hsequential_8/lstm_17/while/lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222?
=sequential_8/lstm_17/while/lstm_cell_17/clip_by_value/Minimum·
7sequential_8/lstm_17/while/lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    29
7sequential_8/lstm_17/while/lstm_cell_17/clip_by_value/y§
5sequential_8/lstm_17/while/lstm_cell_17/clip_by_valueMaximumAsequential_8/lstm_17/while/lstm_cell_17/clip_by_value/Minimum:z:0@sequential_8/lstm_17/while/lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:2227
5sequential_8/lstm_17/while/lstm_cell_17/clip_by_value§
/sequential_8/lstm_17/while/lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>21
/sequential_8/lstm_17/while/lstm_cell_17/Const_2§
/sequential_8/lstm_17/while/lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?21
/sequential_8/lstm_17/while/lstm_cell_17/Const_3
-sequential_8/lstm_17/while/lstm_cell_17/Mul_1Mul6sequential_8/lstm_17/while/lstm_cell_17/split:output:18sequential_8/lstm_17/while/lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_17/while/lstm_cell_17/Mul_1ý
-sequential_8/lstm_17/while/lstm_cell_17/Add_2AddV21sequential_8/lstm_17/while/lstm_cell_17/Mul_1:z:08sequential_8/lstm_17/while/lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_17/while/lstm_cell_17/Add_2Ë
Asequential_8/lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2C
Asequential_8/lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum/yµ
?sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_1/MinimumMinimum1sequential_8/lstm_17/while/lstm_cell_17/Add_2:z:0Jsequential_8/lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222A
?sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum»
9sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2;
9sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_1/y¯
7sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_1MaximumCsequential_8/lstm_17/while/lstm_cell_17/clip_by_value_1/Minimum:z:0Bsequential_8/lstm_17/while/lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:2229
7sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_1õ
-sequential_8/lstm_17/while/lstm_cell_17/mul_2Mul;sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_1:z:0(sequential_8_lstm_17_while_placeholder_3*
T0*
_output_shapes

:222/
-sequential_8/lstm_17/while/lstm_cell_17/mul_2Å
,sequential_8/lstm_17/while/lstm_cell_17/TanhTanh6sequential_8/lstm_17/while/lstm_cell_17/split:output:2*
T0*
_output_shapes

:222.
,sequential_8/lstm_17/while/lstm_cell_17/Tanhû
-sequential_8/lstm_17/while/lstm_cell_17/mul_3Mul9sequential_8/lstm_17/while/lstm_cell_17/clip_by_value:z:00sequential_8/lstm_17/while/lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_17/while/lstm_cell_17/mul_3ö
-sequential_8/lstm_17/while/lstm_cell_17/add_3AddV21sequential_8/lstm_17/while/lstm_cell_17/mul_2:z:01sequential_8/lstm_17/while/lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_17/while/lstm_cell_17/add_3§
/sequential_8/lstm_17/while/lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>21
/sequential_8/lstm_17/while/lstm_cell_17/Const_4§
/sequential_8/lstm_17/while/lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?21
/sequential_8/lstm_17/while/lstm_cell_17/Const_5
-sequential_8/lstm_17/while/lstm_cell_17/Mul_4Mul6sequential_8/lstm_17/while/lstm_cell_17/split:output:38sequential_8/lstm_17/while/lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_17/while/lstm_cell_17/Mul_4ý
-sequential_8/lstm_17/while/lstm_cell_17/Add_4AddV21sequential_8/lstm_17/while/lstm_cell_17/Mul_4:z:08sequential_8/lstm_17/while/lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_17/while/lstm_cell_17/Add_4Ë
Asequential_8/lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2C
Asequential_8/lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum/yµ
?sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_2/MinimumMinimum1sequential_8/lstm_17/while/lstm_cell_17/Add_4:z:0Jsequential_8/lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222A
?sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum»
9sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2;
9sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_2/y¯
7sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_2MaximumCsequential_8/lstm_17/while/lstm_cell_17/clip_by_value_2/Minimum:z:0Bsequential_8/lstm_17/while/lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:2229
7sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_2Ä
.sequential_8/lstm_17/while/lstm_cell_17/Tanh_1Tanh1sequential_8/lstm_17/while/lstm_cell_17/add_3:z:0*
T0*
_output_shapes

:2220
.sequential_8/lstm_17/while/lstm_cell_17/Tanh_1ÿ
-sequential_8/lstm_17/while/lstm_cell_17/mul_5Mul;sequential_8/lstm_17/while/lstm_cell_17/clip_by_value_2:z:02sequential_8/lstm_17/while/lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_17/while/lstm_cell_17/mul_5É
?sequential_8/lstm_17/while/TensorArrayV2Write/TensorListSetItemTensorListSetItem(sequential_8_lstm_17_while_placeholder_1&sequential_8_lstm_17_while_placeholder1sequential_8/lstm_17/while/lstm_cell_17/mul_5:z:0*
_output_shapes
: *
element_dtype02A
?sequential_8/lstm_17/while/TensorArrayV2Write/TensorListSetItem
 sequential_8/lstm_17/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2"
 sequential_8/lstm_17/while/add/y½
sequential_8/lstm_17/while/addAddV2&sequential_8_lstm_17_while_placeholder)sequential_8/lstm_17/while/add/y:output:0*
T0*
_output_shapes
: 2 
sequential_8/lstm_17/while/add
"sequential_8/lstm_17/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2$
"sequential_8/lstm_17/while/add_1/yß
 sequential_8/lstm_17/while/add_1AddV2Bsequential_8_lstm_17_while_sequential_8_lstm_17_while_loop_counter+sequential_8/lstm_17/while/add_1/y:output:0*
T0*
_output_shapes
: 2"
 sequential_8/lstm_17/while/add_1¿
#sequential_8/lstm_17/while/IdentityIdentity$sequential_8/lstm_17/while/add_1:z:0 ^sequential_8/lstm_17/while/NoOp*
T0*
_output_shapes
: 2%
#sequential_8/lstm_17/while/Identityç
%sequential_8/lstm_17/while/Identity_1IdentityHsequential_8_lstm_17_while_sequential_8_lstm_17_while_maximum_iterations ^sequential_8/lstm_17/while/NoOp*
T0*
_output_shapes
: 2'
%sequential_8/lstm_17/while/Identity_1Á
%sequential_8/lstm_17/while/Identity_2Identity"sequential_8/lstm_17/while/add:z:0 ^sequential_8/lstm_17/while/NoOp*
T0*
_output_shapes
: 2'
%sequential_8/lstm_17/while/Identity_2î
%sequential_8/lstm_17/while/Identity_3IdentityOsequential_8/lstm_17/while/TensorArrayV2Write/TensorListSetItem:output_handle:0 ^sequential_8/lstm_17/while/NoOp*
T0*
_output_shapes
: 2'
%sequential_8/lstm_17/while/Identity_3Ø
%sequential_8/lstm_17/while/Identity_4Identity1sequential_8/lstm_17/while/lstm_cell_17/mul_5:z:0 ^sequential_8/lstm_17/while/NoOp*
T0*
_output_shapes

:222'
%sequential_8/lstm_17/while/Identity_4Ø
%sequential_8/lstm_17/while/Identity_5Identity1sequential_8/lstm_17/while/lstm_cell_17/add_3:z:0 ^sequential_8/lstm_17/while/NoOp*
T0*
_output_shapes

:222'
%sequential_8/lstm_17/while/Identity_5Ç
sequential_8/lstm_17/while/NoOpNoOp?^sequential_8/lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp>^sequential_8/lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp@^sequential_8/lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2!
sequential_8/lstm_17/while/NoOp"S
#sequential_8_lstm_17_while_identity,sequential_8/lstm_17/while/Identity:output:0"W
%sequential_8_lstm_17_while_identity_1.sequential_8/lstm_17/while/Identity_1:output:0"W
%sequential_8_lstm_17_while_identity_2.sequential_8/lstm_17/while/Identity_2:output:0"W
%sequential_8_lstm_17_while_identity_3.sequential_8/lstm_17/while/Identity_3:output:0"W
%sequential_8_lstm_17_while_identity_4.sequential_8/lstm_17/while/Identity_4:output:0"W
%sequential_8_lstm_17_while_identity_5.sequential_8/lstm_17/while/Identity_5:output:0"
Gsequential_8_lstm_17_while_lstm_cell_17_biasadd_readvariableop_resourceIsequential_8_lstm_17_while_lstm_cell_17_biasadd_readvariableop_resource_0"
Hsequential_8_lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resourceJsequential_8_lstm_17_while_lstm_cell_17_matmul_1_readvariableop_resource_0"
Fsequential_8_lstm_17_while_lstm_cell_17_matmul_readvariableop_resourceHsequential_8_lstm_17_while_lstm_cell_17_matmul_readvariableop_resource_0"
=sequential_8_lstm_17_while_sequential_8_lstm_17_strided_slice?sequential_8_lstm_17_while_sequential_8_lstm_17_strided_slice_0"ü
{sequential_8_lstm_17_while_tensorarrayv2read_tensorlistgetitem_sequential_8_lstm_17_tensorarrayunstack_tensorlistfromtensor}sequential_8_lstm_17_while_tensorarrayv2read_tensorlistgetitem_sequential_8_lstm_17_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2
>sequential_8/lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp>sequential_8/lstm_17/while/lstm_cell_17/BiasAdd/ReadVariableOp2~
=sequential_8/lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp=sequential_8/lstm_17/while/lstm_cell_17/MatMul/ReadVariableOp2
?sequential_8/lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp?sequential_8/lstm_17/while/lstm_cell_17/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
÷,

G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53711

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
:	2È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2	
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
(:22:22:22:22*
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

:222
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
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

:222
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:222
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:222
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:222
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:222
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

:222
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:222
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:222
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:222

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
$:2:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:HD

_output_shapes

:22
"
_user_specified_name
states/0:HD

_output_shapes

:22
"
_user_specified_name
states/1
¬
Á
,__inference_sequential_8_layer_call_fn_51062
lstm_17_input
unknown:	È
	unknown_0:22
	unknown_1:	2È
	unknown_2:	È
	unknown_3:22
	unknown_4:	2È
	unknown_5:22
	unknown_6:	2È
	unknown_7:	È
	unknown_8:22
	unknown_9:2

unknown_10:
identity¢StatefulPartitionedCallø
StatefulPartitionedCallStatefulPartitionedCalllstm_17_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_sequential_8_layer_call_and_return_conditional_losses_510062
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
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
&:2
: : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:Q M
"
_output_shapes
:2

'
_user_specified_namelstm_17_input
±0
Ô
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_48446

inputs
states:22
states_1:221
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
:	2È2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:22*
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
:	2È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2	
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
(:22:22:22:22*
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

:222
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
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

:222
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_1u
mul_2/ReadVariableOpReadVariableOpstates_1*
_output_shapes

:22*
dtype02
mul_2/ReadVariableOpq
mul_2Mulclip_by_value_1:z:0mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:222
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:222
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:222
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

:222
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:222
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:222
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:222

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
:2: : : : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:&"
 
_user_specified_namestates:&"
 
_user_specified_namestates
ü

¸
#__inference_signature_wrapper_51165
lstm_17_input
unknown:	È
	unknown_0:22
	unknown_1:	2È
	unknown_2:	È
	unknown_3:22
	unknown_4:	2È
	unknown_5:22
	unknown_6:	2È
	unknown_7:	È
	unknown_8:22
	unknown_9:2

unknown_10:
identity¢StatefulPartitionedCallÑ
StatefulPartitionedCallStatefulPartitionedCalllstm_17_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8 *)
f$R"
 __inference__wrapped_model_483692
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
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
&:2
: : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:Q M
"
_output_shapes
:2

'
_user_specified_namelstm_17_input
¨
¼
while_cond_52201
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_52201___redundant_placeholder03
/while_while_cond_52201___redundant_placeholder13
/while_while_cond_52201___redundant_placeholder23
/while_while_cond_52201___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
í

'__inference_dense_8_layer_call_fn_54424

inputs
unknown:2
	unknown_0:
identity¢StatefulPartitionedCallò
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
GPU 2J 8 *K
fFRD
B__inference_dense_8_layer_call_and_return_conditional_losses_499452
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
 
à
%sequential_8_lstm_16_while_cond_48251F
Bsequential_8_lstm_16_while_sequential_8_lstm_16_while_loop_counterL
Hsequential_8_lstm_16_while_sequential_8_lstm_16_while_maximum_iterations*
&sequential_8_lstm_16_while_placeholder,
(sequential_8_lstm_16_while_placeholder_1,
(sequential_8_lstm_16_while_placeholder_2,
(sequential_8_lstm_16_while_placeholder_3F
Bsequential_8_lstm_16_while_less_sequential_8_lstm_16_strided_slice]
Ysequential_8_lstm_16_while_sequential_8_lstm_16_while_cond_48251___redundant_placeholder0]
Ysequential_8_lstm_16_while_sequential_8_lstm_16_while_cond_48251___redundant_placeholder1]
Ysequential_8_lstm_16_while_sequential_8_lstm_16_while_cond_48251___redundant_placeholder2]
Ysequential_8_lstm_16_while_sequential_8_lstm_16_while_cond_48251___redundant_placeholder3'
#sequential_8_lstm_16_while_identity
×
sequential_8/lstm_16/while/LessLess&sequential_8_lstm_16_while_placeholderBsequential_8_lstm_16_while_less_sequential_8_lstm_16_strided_slice*
T0*
_output_shapes
: 2!
sequential_8/lstm_16/while/Less
#sequential_8/lstm_16/while/IdentityIdentity#sequential_8/lstm_16/while/Less:z:0*
T0
*
_output_shapes
: 2%
#sequential_8/lstm_16/while/Identity"S
#sequential_8_lstm_16_while_identity,sequential_8/lstm_16/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
Þ
î
'__inference_lstm_16_layer_call_fn_53480

inputs
unknown:	2È
	unknown_0:22
	unknown_1:	2È
	unknown_2:	È
	unknown_3:22
identity¢StatefulPartitionedCall
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_16_layer_call_and_return_conditional_losses_504332
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
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
:2
2: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs


Ü
lstm_17_while_cond_51601,
(lstm_17_while_lstm_17_while_loop_counter2
.lstm_17_while_lstm_17_while_maximum_iterations
lstm_17_while_placeholder
lstm_17_while_placeholder_1
lstm_17_while_placeholder_2
lstm_17_while_placeholder_3,
(lstm_17_while_less_lstm_17_strided_sliceC
?lstm_17_while_lstm_17_while_cond_51601___redundant_placeholder0C
?lstm_17_while_lstm_17_while_cond_51601___redundant_placeholder1C
?lstm_17_while_lstm_17_while_cond_51601___redundant_placeholder2C
?lstm_17_while_lstm_17_while_cond_51601___redundant_placeholder3
lstm_17_while_identity

lstm_17/while/LessLesslstm_17_while_placeholder(lstm_17_while_less_lstm_17_strided_slice*
T0*
_output_shapes
: 2
lstm_17/while/Lessu
lstm_17/while/IdentityIdentitylstm_17/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_17/while/Identity"9
lstm_17_while_identitylstm_17/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
ÔY
Ë
while_body_52380
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_17_matmul_readvariableop_resource_0:	ÈH
5while_lstm_cell_17_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_17_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_17_matmul_readvariableop_resource:	ÈF
3while_lstm_cell_17_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_17_biasadd_readvariableop_resource:	È¢)while/lstm_cell_17/BiasAdd/ReadVariableOp¢(while/lstm_cell_17/MatMul/ReadVariableOp¢*while/lstm_cell_17/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes
:	È*
dtype02*
(while/lstm_cell_17/MatMul/ReadVariableOpÎ
while/lstm_cell_17/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/MatMulÏ
*while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_17/MatMul_1/ReadVariableOp·
while/lstm_cell_17/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/MatMul_1¯
while/lstm_cell_17/addAddV2#while/lstm_cell_17/MatMul:product:0%while/lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/addÈ
)while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_17/BiasAdd/ReadVariableOp¼
while/lstm_cell_17/BiasAddBiasAddwhile/lstm_cell_17/add:z:01while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_17/BiasAdd
"while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_17/split/split_dimç
while/lstm_cell_17/splitSplit+while/lstm_cell_17/split/split_dim:output:0#while/lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_17/splity
while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const}
while/lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_1¦
while/lstm_cell_17/MulMul!while/lstm_cell_17/split:output:0!while/lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul§
while/lstm_cell_17/Add_1AddV2while/lstm_cell_17/Mul:z:0#while/lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_1
*while/lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_17/clip_by_value/Minimum/yÛ
(while/lstm_cell_17/clip_by_value/MinimumMinimumwhile/lstm_cell_17/Add_1:z:03while/lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(while/lstm_cell_17/clip_by_value/Minimum
"while/lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_17/clip_by_value/yÓ
 while/lstm_cell_17/clip_by_valueMaximum,while/lstm_cell_17/clip_by_value/Minimum:z:0+while/lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 while/lstm_cell_17/clip_by_value}
while/lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const_2}
while/lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_3¬
while/lstm_cell_17/Mul_1Mul!while/lstm_cell_17/split:output:1#while/lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul_1©
while/lstm_cell_17/Add_2AddV2while/lstm_cell_17/Mul_1:z:0#while/lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_2¡
,while/lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_17/clip_by_value_1/Minimum/yá
*while/lstm_cell_17/clip_by_value_1/MinimumMinimumwhile/lstm_cell_17/Add_2:z:05while/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_17/clip_by_value_1/Minimum
$while/lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_17/clip_by_value_1/yÛ
"while/lstm_cell_17/clip_by_value_1Maximum.while/lstm_cell_17/clip_by_value_1/Minimum:z:0-while/lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_17/clip_by_value_1¡
while/lstm_cell_17/mul_2Mul&while/lstm_cell_17/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_2
while/lstm_cell_17/TanhTanh!while/lstm_cell_17/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_17/Tanh§
while/lstm_cell_17/mul_3Mul$while/lstm_cell_17/clip_by_value:z:0while/lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_3¢
while/lstm_cell_17/add_3AddV2while/lstm_cell_17/mul_2:z:0while/lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_17/add_3}
while/lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_17/Const_4}
while/lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_17/Const_5¬
while/lstm_cell_17/Mul_4Mul!while/lstm_cell_17/split:output:3#while/lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Mul_4©
while/lstm_cell_17/Add_4AddV2while/lstm_cell_17/Mul_4:z:0#while/lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Add_4¡
,while/lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_17/clip_by_value_2/Minimum/yá
*while/lstm_cell_17/clip_by_value_2/MinimumMinimumwhile/lstm_cell_17/Add_4:z:05while/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_17/clip_by_value_2/Minimum
$while/lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_17/clip_by_value_2/yÛ
"while/lstm_cell_17/clip_by_value_2Maximum.while/lstm_cell_17/clip_by_value_2/Minimum:z:0-while/lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_17/clip_by_value_2
while/lstm_cell_17/Tanh_1Tanhwhile/lstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_17/Tanh_1«
while/lstm_cell_17/mul_5Mul&while/lstm_cell_17/clip_by_value_2:z:0while/lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_17/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_17/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_17/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_17/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*"
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
2while_lstm_cell_17_biasadd_readvariableop_resource4while_lstm_cell_17_biasadd_readvariableop_resource_0"l
3while_lstm_cell_17_matmul_1_readvariableop_resource5while_lstm_cell_17_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_17_matmul_readvariableop_resource3while_lstm_cell_17_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_17/BiasAdd/ReadVariableOp)while/lstm_cell_17/BiasAdd/ReadVariableOp2T
(while/lstm_cell_17/MatMul/ReadVariableOp(while/lstm_cell_17/MatMul/ReadVariableOp2X
*while/lstm_cell_17/MatMul_1/ReadVariableOp*while/lstm_cell_17/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
ø9

B__inference_lstm_17_layer_call_and_return_conditional_losses_48588

inputs$
lstm_cell_17_48447:22$
lstm_cell_17_48449:22%
lstm_cell_17_48451:	È%
lstm_cell_17_48453:	2È!
lstm_cell_17_48455:	È
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢$lstm_cell_17/StatefulPartitionedCall¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1
$lstm_cell_17/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_17_48447lstm_cell_17_48449lstm_cell_17_48451lstm_cell_17_48453lstm_cell_17_48455*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:22:22:22*'
_read_only_resource_inputs	
*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_484462&
$lstm_cell_17/StatefulPartitionedCall
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOplstm_cell_17_48447*
_output_shapes

:22*
dtype02
ReadVariableOpw
ReadVariableOp_1ReadVariableOplstm_cell_17_48449*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_17_48451lstm_cell_17_48453lstm_cell_17_48455*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_48466*
condR
while_cond_48465*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22*
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

:22*
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
:2ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime·
AssignVariableOpAssignVariableOplstm_cell_17_48447while:output:4^ReadVariableOp%^lstm_cell_17/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp½
AssignVariableOp_1AssignVariableOplstm_cell_17_48449while:output:5^ReadVariableOp_1%^lstm_cell_17/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ22

IdentityÉ
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1%^lstm_cell_17/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2ÿÿÿÿÿÿÿÿÿ: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12L
$lstm_cell_17/StatefulPartitionedCall$lstm_cell_17/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs
ø9

B__inference_lstm_16_layer_call_and_return_conditional_losses_49364

inputs$
lstm_cell_16_49223:22$
lstm_cell_16_49225:22%
lstm_cell_16_49227:	2È%
lstm_cell_16_49229:	2È!
lstm_cell_16_49231:	È
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢$lstm_cell_16/StatefulPartitionedCall¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ222
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1
$lstm_cell_16/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_16_49223lstm_cell_16_49225lstm_cell_16_49227lstm_cell_16_49229lstm_cell_16_49231*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:22:22:22*'
_read_only_resource_inputs	
*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_492222&
$lstm_cell_16/StatefulPartitionedCall
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOplstm_cell_16_49223*
_output_shapes

:22*
dtype02
ReadVariableOpw
ReadVariableOp_1ReadVariableOplstm_cell_16_49225*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_16_49227lstm_cell_16_49229lstm_cell_16_49231*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_49242*
condR
while_cond_49241*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22*
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

:22*
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
:2ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime·
AssignVariableOpAssignVariableOplstm_cell_16_49223while:output:4^ReadVariableOp%^lstm_cell_16/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp½
AssignVariableOp_1AssignVariableOplstm_cell_16_49225while:output:5^ReadVariableOp_1%^lstm_cell_16/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ22

IdentityÉ
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1%^lstm_cell_16/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2ÿÿÿÿÿÿÿÿÿ2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12L
$lstm_cell_16/StatefulPartitionedCall$lstm_cell_16/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
å$
Ø
while_body_48836
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
while_lstm_cell_17_48860_0:	È-
while_lstm_cell_17_48862_0:	2È)
while_lstm_cell_17_48864_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
while_lstm_cell_17_48860:	È+
while_lstm_cell_17_48862:	2È'
while_lstm_cell_17_48864:	È¢*while/lstm_cell_17/StatefulPartitionedCallÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÂ
*while/lstm_cell_17/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_17_48860_0while_lstm_cell_17_48862_0while_lstm_cell_17_48864_0*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:22:22:22*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_486762,
*while/lstm_cell_17/StatefulPartitionedCall÷
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder3while/lstm_cell_17/StatefulPartitionedCall:output:0*
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
while/Identity_4Identity3while/lstm_cell_17/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identity3while/lstm_cell_17/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5

while/NoOpNoOp+^while/lstm_cell_17/StatefulPartitionedCall*"
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
while_lstm_cell_17_48860while_lstm_cell_17_48860_0"6
while_lstm_cell_17_48862while_lstm_cell_17_48862_0"6
while_lstm_cell_17_48864while_lstm_cell_17_48864_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2X
*while/lstm_cell_17/StatefulPartitionedCall*while/lstm_cell_17/StatefulPartitionedCall: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
¨
¼
while_cond_49611
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_49611___redundant_placeholder03
/while_while_cond_49611___redundant_placeholder13
/while_while_cond_49611___redundant_placeholder23
/while_while_cond_49611___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
 
Ì
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_50004

inputs
dense_8_49994:2
dense_8_49996:
identity¢dense_8/StatefulPartitionedCallD
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
Reshape
dense_8/StatefulPartitionedCallStatefulPartitionedCallReshape:output:0dense_8_49994dense_8_49996*
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
GPU 2J 8 *K
fFRD
B__inference_dense_8_layer_call_and_return_conditional_losses_499452!
dense_8/StatefulPartitionedCallq
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
Reshape_1/shape¤
	Reshape_1Reshape(dense_8/StatefulPartitionedCall:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2

Identityp
NoOpNoOp ^dense_8/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2: : 2B
dense_8/StatefulPartitionedCalldense_8/StatefulPartitionedCall:\ X
4
_output_shapes"
 :ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
 
_user_specified_nameinputs
	
ð
'__inference_lstm_16_layer_call_fn_53450
inputs_0
unknown:22
	unknown_0:22
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
:2ÿÿÿÿÿÿÿÿÿ2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_16_layer_call_and_return_conditional_losses_493642
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2ÿÿÿÿÿÿÿÿÿ2: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ2
"
_user_specified_name
inputs/0

ì
%sequential_8_lstm_16_while_body_48252F
Bsequential_8_lstm_16_while_sequential_8_lstm_16_while_loop_counterL
Hsequential_8_lstm_16_while_sequential_8_lstm_16_while_maximum_iterations*
&sequential_8_lstm_16_while_placeholder,
(sequential_8_lstm_16_while_placeholder_1,
(sequential_8_lstm_16_while_placeholder_2,
(sequential_8_lstm_16_while_placeholder_3C
?sequential_8_lstm_16_while_sequential_8_lstm_16_strided_slice_0
}sequential_8_lstm_16_while_tensorarrayv2read_tensorlistgetitem_sequential_8_lstm_16_tensorarrayunstack_tensorlistfromtensor_0[
Hsequential_8_lstm_16_while_lstm_cell_16_matmul_readvariableop_resource_0:	2È]
Jsequential_8_lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource_0:	2ÈX
Isequential_8_lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource_0:	È'
#sequential_8_lstm_16_while_identity)
%sequential_8_lstm_16_while_identity_1)
%sequential_8_lstm_16_while_identity_2)
%sequential_8_lstm_16_while_identity_3)
%sequential_8_lstm_16_while_identity_4)
%sequential_8_lstm_16_while_identity_5A
=sequential_8_lstm_16_while_sequential_8_lstm_16_strided_slice
{sequential_8_lstm_16_while_tensorarrayv2read_tensorlistgetitem_sequential_8_lstm_16_tensorarrayunstack_tensorlistfromtensorY
Fsequential_8_lstm_16_while_lstm_cell_16_matmul_readvariableop_resource:	2È[
Hsequential_8_lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource:	2ÈV
Gsequential_8_lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource:	È¢>sequential_8/lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp¢=sequential_8/lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp¢?sequential_8/lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOpí
Lsequential_8/lstm_16/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2N
Lsequential_8/lstm_16/while/TensorArrayV2Read/TensorListGetItem/element_shapeÈ
>sequential_8/lstm_16/while/TensorArrayV2Read/TensorListGetItemTensorListGetItem}sequential_8_lstm_16_while_tensorarrayv2read_tensorlistgetitem_sequential_8_lstm_16_tensorarrayunstack_tensorlistfromtensor_0&sequential_8_lstm_16_while_placeholderUsequential_8/lstm_16/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02@
>sequential_8/lstm_16/while/TensorArrayV2Read/TensorListGetItem
=sequential_8/lstm_16/while/lstm_cell_16/MatMul/ReadVariableOpReadVariableOpHsequential_8_lstm_16_while_lstm_cell_16_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02?
=sequential_8/lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp¢
.sequential_8/lstm_16/while/lstm_cell_16/MatMulMatMulEsequential_8/lstm_16/while/TensorArrayV2Read/TensorListGetItem:item:0Esequential_8/lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È20
.sequential_8/lstm_16/while/lstm_cell_16/MatMul
?sequential_8/lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOpJsequential_8_lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02A
?sequential_8/lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp
0sequential_8/lstm_16/while/lstm_cell_16/MatMul_1MatMul(sequential_8_lstm_16_while_placeholder_2Gsequential_8/lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È22
0sequential_8/lstm_16/while/lstm_cell_16/MatMul_1
+sequential_8/lstm_16/while/lstm_cell_16/addAddV28sequential_8/lstm_16/while/lstm_cell_16/MatMul:product:0:sequential_8/lstm_16/while/lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2-
+sequential_8/lstm_16/while/lstm_cell_16/add
>sequential_8/lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOpIsequential_8_lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02@
>sequential_8/lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp
/sequential_8/lstm_16/while/lstm_cell_16/BiasAddBiasAdd/sequential_8/lstm_16/while/lstm_cell_16/add:z:0Fsequential_8/lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È21
/sequential_8/lstm_16/while/lstm_cell_16/BiasAdd´
7sequential_8/lstm_16/while/lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :29
7sequential_8/lstm_16/while/lstm_cell_16/split/split_dim»
-sequential_8/lstm_16/while/lstm_cell_16/splitSplit@sequential_8/lstm_16/while/lstm_cell_16/split/split_dim:output:08sequential_8/lstm_16/while/lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2/
-sequential_8/lstm_16/while/lstm_cell_16/split£
-sequential_8/lstm_16/while/lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2/
-sequential_8/lstm_16/while/lstm_cell_16/Const§
/sequential_8/lstm_16/while/lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?21
/sequential_8/lstm_16/while/lstm_cell_16/Const_1ú
+sequential_8/lstm_16/while/lstm_cell_16/MulMul6sequential_8/lstm_16/while/lstm_cell_16/split:output:06sequential_8/lstm_16/while/lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222-
+sequential_8/lstm_16/while/lstm_cell_16/Mulû
-sequential_8/lstm_16/while/lstm_cell_16/Add_1AddV2/sequential_8/lstm_16/while/lstm_cell_16/Mul:z:08sequential_8/lstm_16/while/lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_16/while/lstm_cell_16/Add_1Ç
?sequential_8/lstm_16/while/lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2A
?sequential_8/lstm_16/while/lstm_cell_16/clip_by_value/Minimum/y¯
=sequential_8/lstm_16/while/lstm_cell_16/clip_by_value/MinimumMinimum1sequential_8/lstm_16/while/lstm_cell_16/Add_1:z:0Hsequential_8/lstm_16/while/lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222?
=sequential_8/lstm_16/while/lstm_cell_16/clip_by_value/Minimum·
7sequential_8/lstm_16/while/lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    29
7sequential_8/lstm_16/while/lstm_cell_16/clip_by_value/y§
5sequential_8/lstm_16/while/lstm_cell_16/clip_by_valueMaximumAsequential_8/lstm_16/while/lstm_cell_16/clip_by_value/Minimum:z:0@sequential_8/lstm_16/while/lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:2227
5sequential_8/lstm_16/while/lstm_cell_16/clip_by_value§
/sequential_8/lstm_16/while/lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>21
/sequential_8/lstm_16/while/lstm_cell_16/Const_2§
/sequential_8/lstm_16/while/lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?21
/sequential_8/lstm_16/while/lstm_cell_16/Const_3
-sequential_8/lstm_16/while/lstm_cell_16/Mul_1Mul6sequential_8/lstm_16/while/lstm_cell_16/split:output:18sequential_8/lstm_16/while/lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_16/while/lstm_cell_16/Mul_1ý
-sequential_8/lstm_16/while/lstm_cell_16/Add_2AddV21sequential_8/lstm_16/while/lstm_cell_16/Mul_1:z:08sequential_8/lstm_16/while/lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_16/while/lstm_cell_16/Add_2Ë
Asequential_8/lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2C
Asequential_8/lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum/yµ
?sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_1/MinimumMinimum1sequential_8/lstm_16/while/lstm_cell_16/Add_2:z:0Jsequential_8/lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222A
?sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum»
9sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2;
9sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_1/y¯
7sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_1MaximumCsequential_8/lstm_16/while/lstm_cell_16/clip_by_value_1/Minimum:z:0Bsequential_8/lstm_16/while/lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:2229
7sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_1õ
-sequential_8/lstm_16/while/lstm_cell_16/mul_2Mul;sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_1:z:0(sequential_8_lstm_16_while_placeholder_3*
T0*
_output_shapes

:222/
-sequential_8/lstm_16/while/lstm_cell_16/mul_2Å
,sequential_8/lstm_16/while/lstm_cell_16/TanhTanh6sequential_8/lstm_16/while/lstm_cell_16/split:output:2*
T0*
_output_shapes

:222.
,sequential_8/lstm_16/while/lstm_cell_16/Tanhû
-sequential_8/lstm_16/while/lstm_cell_16/mul_3Mul9sequential_8/lstm_16/while/lstm_cell_16/clip_by_value:z:00sequential_8/lstm_16/while/lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_16/while/lstm_cell_16/mul_3ö
-sequential_8/lstm_16/while/lstm_cell_16/add_3AddV21sequential_8/lstm_16/while/lstm_cell_16/mul_2:z:01sequential_8/lstm_16/while/lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_16/while/lstm_cell_16/add_3§
/sequential_8/lstm_16/while/lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>21
/sequential_8/lstm_16/while/lstm_cell_16/Const_4§
/sequential_8/lstm_16/while/lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?21
/sequential_8/lstm_16/while/lstm_cell_16/Const_5
-sequential_8/lstm_16/while/lstm_cell_16/Mul_4Mul6sequential_8/lstm_16/while/lstm_cell_16/split:output:38sequential_8/lstm_16/while/lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_16/while/lstm_cell_16/Mul_4ý
-sequential_8/lstm_16/while/lstm_cell_16/Add_4AddV21sequential_8/lstm_16/while/lstm_cell_16/Mul_4:z:08sequential_8/lstm_16/while/lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_16/while/lstm_cell_16/Add_4Ë
Asequential_8/lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2C
Asequential_8/lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum/yµ
?sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_2/MinimumMinimum1sequential_8/lstm_16/while/lstm_cell_16/Add_4:z:0Jsequential_8/lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222A
?sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum»
9sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2;
9sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_2/y¯
7sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_2MaximumCsequential_8/lstm_16/while/lstm_cell_16/clip_by_value_2/Minimum:z:0Bsequential_8/lstm_16/while/lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:2229
7sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_2Ä
.sequential_8/lstm_16/while/lstm_cell_16/Tanh_1Tanh1sequential_8/lstm_16/while/lstm_cell_16/add_3:z:0*
T0*
_output_shapes

:2220
.sequential_8/lstm_16/while/lstm_cell_16/Tanh_1ÿ
-sequential_8/lstm_16/while/lstm_cell_16/mul_5Mul;sequential_8/lstm_16/while/lstm_cell_16/clip_by_value_2:z:02sequential_8/lstm_16/while/lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222/
-sequential_8/lstm_16/while/lstm_cell_16/mul_5É
?sequential_8/lstm_16/while/TensorArrayV2Write/TensorListSetItemTensorListSetItem(sequential_8_lstm_16_while_placeholder_1&sequential_8_lstm_16_while_placeholder1sequential_8/lstm_16/while/lstm_cell_16/mul_5:z:0*
_output_shapes
: *
element_dtype02A
?sequential_8/lstm_16/while/TensorArrayV2Write/TensorListSetItem
 sequential_8/lstm_16/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2"
 sequential_8/lstm_16/while/add/y½
sequential_8/lstm_16/while/addAddV2&sequential_8_lstm_16_while_placeholder)sequential_8/lstm_16/while/add/y:output:0*
T0*
_output_shapes
: 2 
sequential_8/lstm_16/while/add
"sequential_8/lstm_16/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2$
"sequential_8/lstm_16/while/add_1/yß
 sequential_8/lstm_16/while/add_1AddV2Bsequential_8_lstm_16_while_sequential_8_lstm_16_while_loop_counter+sequential_8/lstm_16/while/add_1/y:output:0*
T0*
_output_shapes
: 2"
 sequential_8/lstm_16/while/add_1¿
#sequential_8/lstm_16/while/IdentityIdentity$sequential_8/lstm_16/while/add_1:z:0 ^sequential_8/lstm_16/while/NoOp*
T0*
_output_shapes
: 2%
#sequential_8/lstm_16/while/Identityç
%sequential_8/lstm_16/while/Identity_1IdentityHsequential_8_lstm_16_while_sequential_8_lstm_16_while_maximum_iterations ^sequential_8/lstm_16/while/NoOp*
T0*
_output_shapes
: 2'
%sequential_8/lstm_16/while/Identity_1Á
%sequential_8/lstm_16/while/Identity_2Identity"sequential_8/lstm_16/while/add:z:0 ^sequential_8/lstm_16/while/NoOp*
T0*
_output_shapes
: 2'
%sequential_8/lstm_16/while/Identity_2î
%sequential_8/lstm_16/while/Identity_3IdentityOsequential_8/lstm_16/while/TensorArrayV2Write/TensorListSetItem:output_handle:0 ^sequential_8/lstm_16/while/NoOp*
T0*
_output_shapes
: 2'
%sequential_8/lstm_16/while/Identity_3Ø
%sequential_8/lstm_16/while/Identity_4Identity1sequential_8/lstm_16/while/lstm_cell_16/mul_5:z:0 ^sequential_8/lstm_16/while/NoOp*
T0*
_output_shapes

:222'
%sequential_8/lstm_16/while/Identity_4Ø
%sequential_8/lstm_16/while/Identity_5Identity1sequential_8/lstm_16/while/lstm_cell_16/add_3:z:0 ^sequential_8/lstm_16/while/NoOp*
T0*
_output_shapes

:222'
%sequential_8/lstm_16/while/Identity_5Ç
sequential_8/lstm_16/while/NoOpNoOp?^sequential_8/lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp>^sequential_8/lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp@^sequential_8/lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2!
sequential_8/lstm_16/while/NoOp"S
#sequential_8_lstm_16_while_identity,sequential_8/lstm_16/while/Identity:output:0"W
%sequential_8_lstm_16_while_identity_1.sequential_8/lstm_16/while/Identity_1:output:0"W
%sequential_8_lstm_16_while_identity_2.sequential_8/lstm_16/while/Identity_2:output:0"W
%sequential_8_lstm_16_while_identity_3.sequential_8/lstm_16/while/Identity_3:output:0"W
%sequential_8_lstm_16_while_identity_4.sequential_8/lstm_16/while/Identity_4:output:0"W
%sequential_8_lstm_16_while_identity_5.sequential_8/lstm_16/while/Identity_5:output:0"
Gsequential_8_lstm_16_while_lstm_cell_16_biasadd_readvariableop_resourceIsequential_8_lstm_16_while_lstm_cell_16_biasadd_readvariableop_resource_0"
Hsequential_8_lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resourceJsequential_8_lstm_16_while_lstm_cell_16_matmul_1_readvariableop_resource_0"
Fsequential_8_lstm_16_while_lstm_cell_16_matmul_readvariableop_resourceHsequential_8_lstm_16_while_lstm_cell_16_matmul_readvariableop_resource_0"
=sequential_8_lstm_16_while_sequential_8_lstm_16_strided_slice?sequential_8_lstm_16_while_sequential_8_lstm_16_strided_slice_0"ü
{sequential_8_lstm_16_while_tensorarrayv2read_tensorlistgetitem_sequential_8_lstm_16_tensorarrayunstack_tensorlistfromtensor}sequential_8_lstm_16_while_tensorarrayv2read_tensorlistgetitem_sequential_8_lstm_16_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2
>sequential_8/lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp>sequential_8/lstm_16/while/lstm_cell_16/BiasAdd/ReadVariableOp2~
=sequential_8/lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp=sequential_8/lstm_16/while/lstm_cell_16/MatMul/ReadVariableOp2
?sequential_8/lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp?sequential_8/lstm_16/while/lstm_cell_16/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
Î
ó
G__inference_sequential_8_layer_call_and_return_conditional_losses_50467

inputs 
lstm_17_50245:	È
lstm_17_50247:22 
lstm_17_50249:	2È
lstm_17_50251:	È
lstm_17_50253:22 
lstm_16_50434:	2È
lstm_16_50436:22 
lstm_16_50438:	2È
lstm_16_50440:	È
lstm_16_50442:22*
time_distributed_8_50459:2&
time_distributed_8_50461:
identity¢lstm_16/StatefulPartitionedCall¢lstm_17/StatefulPartitionedCall¢*time_distributed_8/StatefulPartitionedCall¸
lstm_17/StatefulPartitionedCallStatefulPartitionedCallinputslstm_17_50245lstm_17_50247lstm_17_50249lstm_17_50251lstm_17_50253*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_17_layer_call_and_return_conditional_losses_502442!
lstm_17/StatefulPartitionedCallÚ
lstm_16/StatefulPartitionedCallStatefulPartitionedCall(lstm_17/StatefulPartitionedCall:output:0lstm_16_50434lstm_16_50436lstm_16_50438lstm_16_50440lstm_16_50442*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_16_layer_call_and_return_conditional_losses_504332!
lstm_16/StatefulPartitionedCallà
*time_distributed_8/StatefulPartitionedCallStatefulPartitionedCall(lstm_16/StatefulPartitionedCall:output:0time_distributed_8_50459time_distributed_8_50461*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *V
fQRO
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_504582,
*time_distributed_8/StatefulPartitionedCall
 time_distributed_8/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2"
 time_distributed_8/Reshape/shapeÂ
time_distributed_8/ReshapeReshape(lstm_16/StatefulPartitionedCall:output:0)time_distributed_8/Reshape/shape:output:0*
T0*
_output_shapes
:	ô22
time_distributed_8/Reshape
IdentityIdentity3time_distributed_8/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity¿
NoOpNoOp ^lstm_16/StatefulPartitionedCall ^lstm_17/StatefulPartitionedCall+^time_distributed_8/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 2B
lstm_16/StatefulPartitionedCalllstm_16/StatefulPartitionedCall2B
lstm_17/StatefulPartitionedCalllstm_17/StatefulPartitionedCall2X
*time_distributed_8/StatefulPartitionedCall*time_distributed_8/StatefulPartitionedCall:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
®

M__inference_time_distributed_8_layer_call_and_return_conditional_losses_53565

inputs8
&dense_8_matmul_readvariableop_resource:25
'dense_8_biasadd_readvariableop_resource:
identity¢dense_8/BiasAdd/ReadVariableOp¢dense_8/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2
Reshape/shapeg
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes
:	ô22	
Reshape¥
dense_8/MatMul/ReadVariableOpReadVariableOp&dense_8_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_8/MatMul/ReadVariableOp
dense_8/MatMulMatMulReshape:output:0%dense_8/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô2
dense_8/MatMul¤
dense_8/BiasAdd/ReadVariableOpReadVariableOp'dense_8_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_8/BiasAdd/ReadVariableOp
dense_8/BiasAddBiasAdddense_8/MatMul:product:0&dense_8/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	ô2
dense_8/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"ÿÿÿÿ
      2
Reshape_1/shape
	Reshape_1Reshapedense_8/BiasAdd:output:0Reshape_1/shape:output:0*
T0*"
_output_shapes
:2
2
	Reshape_1h
IdentityIdentityReshape_1:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity
NoOpNoOp^dense_8/BiasAdd/ReadVariableOp^dense_8/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:2
2: : 2@
dense_8/BiasAdd/ReadVariableOpdense_8/BiasAdd/ReadVariableOp2>
dense_8/MatMul/ReadVariableOpdense_8/MatMul/ReadVariableOp:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
÷,

G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54113

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
:	2È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2	
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
(:22:22:22:22*
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

:222
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
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

:222
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:222
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:222
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:222
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:222
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

:222
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:222
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:222
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:222

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
$:22:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:22
 
_user_specified_nameinputs:HD

_output_shapes

:22
"
_user_specified_name
states/0:HD

_output_shapes

:22
"
_user_specified_name
states/1
Þ
î
'__inference_lstm_16_layer_call_fn_53495

inputs
unknown:	2È
	unknown_0:22
	unknown_1:	2È
	unknown_2:	È
	unknown_3:22
identity¢StatefulPartitionedCall
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_16_layer_call_and_return_conditional_losses_507212
StatefulPartitionedCallv
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
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
:2
2: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
æ
õ
,__inference_lstm_cell_17_layer_call_fn_54003

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
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_539902
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
$:2:22:22: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:2
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
 
à
%sequential_8_lstm_17_while_cond_48077F
Bsequential_8_lstm_17_while_sequential_8_lstm_17_while_loop_counterL
Hsequential_8_lstm_17_while_sequential_8_lstm_17_while_maximum_iterations*
&sequential_8_lstm_17_while_placeholder,
(sequential_8_lstm_17_while_placeholder_1,
(sequential_8_lstm_17_while_placeholder_2,
(sequential_8_lstm_17_while_placeholder_3F
Bsequential_8_lstm_17_while_less_sequential_8_lstm_17_strided_slice]
Ysequential_8_lstm_17_while_sequential_8_lstm_17_while_cond_48077___redundant_placeholder0]
Ysequential_8_lstm_17_while_sequential_8_lstm_17_while_cond_48077___redundant_placeholder1]
Ysequential_8_lstm_17_while_sequential_8_lstm_17_while_cond_48077___redundant_placeholder2]
Ysequential_8_lstm_17_while_sequential_8_lstm_17_while_cond_48077___redundant_placeholder3'
#sequential_8_lstm_17_while_identity
×
sequential_8/lstm_17/while/LessLess&sequential_8_lstm_17_while_placeholderBsequential_8_lstm_17_while_less_sequential_8_lstm_17_strided_slice*
T0*
_output_shapes
: 2!
sequential_8/lstm_17/while/Less
#sequential_8/lstm_17/while/IdentityIdentity#sequential_8/lstm_17/while/Less:z:0*
T0
*
_output_shapes
: 2%
#sequential_8/lstm_17/while/Identity"S
#sequential_8_lstm_17_while_identity,sequential_8/lstm_17/while/Identity:output:0*(
_construction_contextkEagerRuntime*A
_input_shapes0
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
ùn
´
B__inference_lstm_16_layer_call_and_return_conditional_losses_50721

inputs>
+lstm_cell_16_matmul_readvariableop_resource:	2È?
-lstm_cell_16_matmul_1_readvariableop_resource:22B
/lstm_cell_16_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_16_biasadd_readvariableop_resource:	È<
*lstm_cell_16_mul_2_readvariableop_resource:22
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_16/BiasAdd/ReadVariableOp¢"lstm_cell_16/MatMul/ReadVariableOp¢$lstm_cell_16/MatMul_1/ReadVariableOp¢&lstm_cell_16/MatMul_1/ReadVariableOp_1¢!lstm_cell_16/mul_2/ReadVariableOp¢whileu
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
222
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2   2   2
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_16/MatMul/ReadVariableOpReadVariableOp+lstm_cell_16_matmul_readvariableop_resource*
_output_shapes
:	2È*
dtype02$
"lstm_cell_16/MatMul/ReadVariableOp¤
lstm_cell_16/MatMulMatMulstrided_slice_1:output:0*lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/MatMulº
$lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02&
$lstm_cell_16/MatMul_1/ReadVariableOpÁ
&lstm_cell_16/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_16_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_16/MatMul_1/ReadVariableOp_1À
lstm_cell_16/MatMul_1MatMul,lstm_cell_16/MatMul_1/ReadVariableOp:value:0.lstm_cell_16/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/MatMul_1
lstm_cell_16/addAddV2lstm_cell_16/MatMul:product:0lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/add´
#lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_16_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_16/BiasAdd/ReadVariableOp¤
lstm_cell_16/BiasAddBiasAddlstm_cell_16/add:z:0+lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_16/BiasAdd~
lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_16/split/split_dimÏ
lstm_cell_16/splitSplit%lstm_cell_16/split/split_dim:output:0lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_16/splitm
lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Constq
lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_1
lstm_cell_16/MulMullstm_cell_16/split:output:0lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul
lstm_cell_16/Add_1AddV2lstm_cell_16/Mul:z:0lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_1
$lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_16/clip_by_value/Minimum/yÃ
"lstm_cell_16/clip_by_value/MinimumMinimumlstm_cell_16/Add_1:z:0-lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222$
"lstm_cell_16/clip_by_value/Minimum
lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_16/clip_by_value/y»
lstm_cell_16/clip_by_valueMaximum&lstm_cell_16/clip_by_value/Minimum:z:0%lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_valueq
lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Const_2q
lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_3
lstm_cell_16/Mul_1Mullstm_cell_16/split:output:1lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul_1
lstm_cell_16/Add_2AddV2lstm_cell_16/Mul_1:z:0lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_2
&lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_16/clip_by_value_1/Minimum/yÉ
$lstm_cell_16/clip_by_value_1/MinimumMinimumlstm_cell_16/Add_2:z:0/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_16/clip_by_value_1/Minimum
lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_16/clip_by_value_1/yÃ
lstm_cell_16/clip_by_value_1Maximum(lstm_cell_16/clip_by_value_1/Minimum:z:0'lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_value_1±
!lstm_cell_16/mul_2/ReadVariableOpReadVariableOp*lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02#
!lstm_cell_16/mul_2/ReadVariableOp¥
lstm_cell_16/mul_2Mul lstm_cell_16/clip_by_value_1:z:0)lstm_cell_16/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_2t
lstm_cell_16/TanhTanhlstm_cell_16/split:output:2*
T0*
_output_shapes

:222
lstm_cell_16/Tanh
lstm_cell_16/mul_3Mullstm_cell_16/clip_by_value:z:0lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_3
lstm_cell_16/add_3AddV2lstm_cell_16/mul_2:z:0lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_16/add_3q
lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_16/Const_4q
lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_16/Const_5
lstm_cell_16/Mul_4Mullstm_cell_16/split:output:3lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Mul_4
lstm_cell_16/Add_4AddV2lstm_cell_16/Mul_4:z:0lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_16/Add_4
&lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_16/clip_by_value_2/Minimum/yÉ
$lstm_cell_16/clip_by_value_2/MinimumMinimumlstm_cell_16/Add_4:z:0/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_16/clip_by_value_2/Minimum
lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_16/clip_by_value_2/yÃ
lstm_cell_16/clip_by_value_2Maximum(lstm_cell_16/clip_by_value_2/Minimum:z:0'lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_16/clip_by_value_2s
lstm_cell_16/Tanh_1Tanhlstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_16/Tanh_1
lstm_cell_16/mul_5Mul lstm_cell_16/clip_by_value_2:z:0lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_16/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp-lstm_cell_16_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_16_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_16_matmul_readvariableop_resource/lstm_cell_16_matmul_1_readvariableop_1_resource,lstm_cell_16_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_50616*
condR
while_cond_50615*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_16_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_16/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_16_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_16/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_16/BiasAdd/ReadVariableOp#^lstm_cell_16/MatMul/ReadVariableOp%^lstm_cell_16/MatMul_1/ReadVariableOp'^lstm_cell_16/MatMul_1/ReadVariableOp_1"^lstm_cell_16/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:2
2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_16/BiasAdd/ReadVariableOp#lstm_cell_16/BiasAdd/ReadVariableOp2H
"lstm_cell_16/MatMul/ReadVariableOp"lstm_cell_16/MatMul/ReadVariableOp2L
$lstm_cell_16/MatMul_1/ReadVariableOp$lstm_cell_16/MatMul_1/ReadVariableOp2P
&lstm_cell_16/MatMul_1/ReadVariableOp_1&lstm_cell_16/MatMul_1/ReadVariableOp_12F
!lstm_cell_16/mul_2/ReadVariableOp!lstm_cell_16/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
å$
Ø
while_body_49612
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0-
while_lstm_cell_16_49636_0:	2È-
while_lstm_cell_16_49638_0:	2È)
while_lstm_cell_16_49640_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor+
while_lstm_cell_16_49636:	2È+
while_lstm_cell_16_49638:	2È'
while_lstm_cell_16_49640:	È¢*while/lstm_cell_16/StatefulPartitionedCallÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÂ
*while/lstm_cell_16/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_16_49636_0while_lstm_cell_16_49638_0while_lstm_cell_16_49640_0*
Tin

2*
Tout
2*
_collective_manager_ids
 *2
_output_shapes 
:22:22:22*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *P
fKRI
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_494522,
*while/lstm_cell_16/StatefulPartitionedCall÷
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder3while/lstm_cell_16/StatefulPartitionedCall:output:0*
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
while/Identity_4Identity3while/lstm_cell_16/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identity3while/lstm_cell_16/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5

while/NoOpNoOp+^while/lstm_cell_16/StatefulPartitionedCall*"
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
while_lstm_cell_16_49636while_lstm_cell_16_49636_0"6
while_lstm_cell_16_49638while_lstm_cell_16_49638_0"6
while_lstm_cell_16_49640while_lstm_cell_16_49640_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2X
*while/lstm_cell_16/StatefulPartitionedCall*while/lstm_cell_16/StatefulPartitionedCall: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
æ
õ
,__inference_lstm_cell_16_layer_call_fn_54331

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
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_543182
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
$:22:22:22: : : 22
StatefulPartitionedCallStatefulPartitionedCall:F B

_output_shapes

:22
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
ü.
º
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53821

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
:	2È2
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
$:2:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp26
MatMul_1/ReadVariableOp_1MatMul_1/ReadVariableOp_12,
mul_2/ReadVariableOpmul_2/ReadVariableOp:F B

_output_shapes

:2
 
_user_specified_nameinputs:($
"
_user_specified_name
states/0:($
"
_user_specified_name
states/1
ùn
´
B__inference_lstm_17_layer_call_and_return_conditional_losses_52485

inputs>
+lstm_cell_17_matmul_readvariableop_resource:	È?
-lstm_cell_17_matmul_1_readvariableop_resource:22B
/lstm_cell_17_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_17_biasadd_readvariableop_resource:	È<
*lstm_cell_17_mul_2_readvariableop_resource:22
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_17/BiasAdd/ReadVariableOp¢"lstm_cell_17/MatMul/ReadVariableOp¢$lstm_cell_17/MatMul_1/ReadVariableOp¢&lstm_cell_17/MatMul_1/ReadVariableOp_1¢!lstm_cell_17/mul_2/ReadVariableOp¢whileu
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
22
	transposec
ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2      2
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_17/MatMul/ReadVariableOpReadVariableOp+lstm_cell_17_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02$
"lstm_cell_17/MatMul/ReadVariableOp¤
lstm_cell_17/MatMulMatMulstrided_slice_1:output:0*lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/MatMulº
$lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02&
$lstm_cell_17/MatMul_1/ReadVariableOpÁ
&lstm_cell_17/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_17_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_17/MatMul_1/ReadVariableOp_1À
lstm_cell_17/MatMul_1MatMul,lstm_cell_17/MatMul_1/ReadVariableOp:value:0.lstm_cell_17/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/MatMul_1
lstm_cell_17/addAddV2lstm_cell_17/MatMul:product:0lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/add´
#lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_17/BiasAdd/ReadVariableOp¤
lstm_cell_17/BiasAddBiasAddlstm_cell_17/add:z:0+lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/BiasAdd~
lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/split/split_dimÏ
lstm_cell_17/splitSplit%lstm_cell_17/split/split_dim:output:0lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_17/splitm
lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Constq
lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_1
lstm_cell_17/MulMullstm_cell_17/split:output:0lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul
lstm_cell_17/Add_1AddV2lstm_cell_17/Mul:z:0lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_1
$lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_17/clip_by_value/Minimum/yÃ
"lstm_cell_17/clip_by_value/MinimumMinimumlstm_cell_17/Add_1:z:0-lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222$
"lstm_cell_17/clip_by_value/Minimum
lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_17/clip_by_value/y»
lstm_cell_17/clip_by_valueMaximum&lstm_cell_17/clip_by_value/Minimum:z:0%lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_valueq
lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Const_2q
lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_3
lstm_cell_17/Mul_1Mullstm_cell_17/split:output:1lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul_1
lstm_cell_17/Add_2AddV2lstm_cell_17/Mul_1:z:0lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_2
&lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_17/clip_by_value_1/Minimum/yÉ
$lstm_cell_17/clip_by_value_1/MinimumMinimumlstm_cell_17/Add_2:z:0/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_17/clip_by_value_1/Minimum
lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_17/clip_by_value_1/yÃ
lstm_cell_17/clip_by_value_1Maximum(lstm_cell_17/clip_by_value_1/Minimum:z:0'lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_value_1±
!lstm_cell_17/mul_2/ReadVariableOpReadVariableOp*lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02#
!lstm_cell_17/mul_2/ReadVariableOp¥
lstm_cell_17/mul_2Mul lstm_cell_17/clip_by_value_1:z:0)lstm_cell_17/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_2t
lstm_cell_17/TanhTanhlstm_cell_17/split:output:2*
T0*
_output_shapes

:222
lstm_cell_17/Tanh
lstm_cell_17/mul_3Mullstm_cell_17/clip_by_value:z:0lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_3
lstm_cell_17/add_3AddV2lstm_cell_17/mul_2:z:0lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_17/add_3q
lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Const_4q
lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_5
lstm_cell_17/Mul_4Mullstm_cell_17/split:output:3lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul_4
lstm_cell_17/Add_4AddV2lstm_cell_17/Mul_4:z:0lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_4
&lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_17/clip_by_value_2/Minimum/yÉ
$lstm_cell_17/clip_by_value_2/MinimumMinimumlstm_cell_17/Add_4:z:0/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_17/clip_by_value_2/Minimum
lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_17/clip_by_value_2/yÃ
lstm_cell_17/clip_by_value_2Maximum(lstm_cell_17/clip_by_value_2/Minimum:z:0'lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_value_2s
lstm_cell_17/Tanh_1Tanhlstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_17/Tanh_1
lstm_cell_17/mul_5Mul lstm_cell_17/clip_by_value_2:z:0lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_17_matmul_readvariableop_resource/lstm_cell_17_matmul_1_readvariableop_1_resource,lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_52380*
condR
while_cond_52379*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeß
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_17_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_17_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_17/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_17/BiasAdd/ReadVariableOp#^lstm_cell_17/MatMul/ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp'^lstm_cell_17/MatMul_1/ReadVariableOp_1"^lstm_cell_17/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:2
: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_17/BiasAdd/ReadVariableOp#lstm_cell_17/BiasAdd/ReadVariableOp2H
"lstm_cell_17/MatMul/ReadVariableOp"lstm_cell_17/MatMul/ReadVariableOp2L
$lstm_cell_17/MatMul_1/ReadVariableOp$lstm_cell_17/MatMul_1/ReadVariableOp2P
&lstm_cell_17/MatMul_1/ReadVariableOp_1&lstm_cell_17/MatMul_1/ReadVariableOp_12F
!lstm_cell_17/mul_2/ReadVariableOp!lstm_cell_17/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
ï,

G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_49318

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
:	2È2
MatMul
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2È*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2È2
add
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2	
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
(:22:22:22:22*
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

:222
Mul[
Add_1AddV2Mul:z:0Const_1:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
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

:222
Mul_1]
Add_2AddV2	Mul_1:z:0Const_3:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_1]
mul_2Mulclip_by_value_1:z:0states_1*
T0*
_output_shapes

:222
mul_2M
TanhTanhsplit:output:2*
T0*
_output_shapes

:222
Tanh[
mul_3Mulclip_by_value:z:0Tanh:y:0*
T0*
_output_shapes

:222
mul_3V
add_3AddV2	mul_2:z:0	mul_3:z:0*
T0*
_output_shapes

:222
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

:222
Mul_4]
Add_4AddV2	Mul_4:z:0Const_5:output:0*
T0*
_output_shapes

:222
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

:222
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

:222
clip_by_value_2L
Tanh_1Tanh	add_3:z:0*
T0*
_output_shapes

:222
Tanh_1_
mul_5Mulclip_by_value_2:z:0
Tanh_1:y:0*
T0*
_output_shapes

:222
mul_5[
IdentityIdentity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_

Identity_1Identity	mul_5:z:0^NoOp*
T0*
_output_shapes

:222

Identity_1_

Identity_2Identity	add_3:z:0^NoOp*
T0*
_output_shapes

:222

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
$:22:22:22: : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:F B

_output_shapes

:22
 
_user_specified_nameinputs:FB

_output_shapes

:22
 
_user_specified_namestates:FB

_output_shapes

:22
 
_user_specified_namestates
ÔY
Ë
while_body_50616
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_16_matmul_readvariableop_resource_0:	2ÈH
5while_lstm_cell_16_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_16_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_16_matmul_readvariableop_resource:	2ÈF
3while_lstm_cell_16_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_16_biasadd_readvariableop_resource:	È¢)while/lstm_cell_16/BiasAdd/ReadVariableOp¢(while/lstm_cell_16/MatMul/ReadVariableOp¢*while/lstm_cell_16/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_16/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_16_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02*
(while/lstm_cell_16/MatMul/ReadVariableOpÎ
while/lstm_cell_16/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/MatMulÏ
*while/lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_16_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_16/MatMul_1/ReadVariableOp·
while/lstm_cell_16/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_16/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/MatMul_1¯
while/lstm_cell_16/addAddV2#while/lstm_cell_16/MatMul:product:0%while/lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/addÈ
)while/lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_16_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_16/BiasAdd/ReadVariableOp¼
while/lstm_cell_16/BiasAddBiasAddwhile/lstm_cell_16/add:z:01while/lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/BiasAdd
"while/lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_16/split/split_dimç
while/lstm_cell_16/splitSplit+while/lstm_cell_16/split/split_dim:output:0#while/lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_16/splity
while/lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const}
while/lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_1¦
while/lstm_cell_16/MulMul!while/lstm_cell_16/split:output:0!while/lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul§
while/lstm_cell_16/Add_1AddV2while/lstm_cell_16/Mul:z:0#while/lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_1
*while/lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_16/clip_by_value/Minimum/yÛ
(while/lstm_cell_16/clip_by_value/MinimumMinimumwhile/lstm_cell_16/Add_1:z:03while/lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(while/lstm_cell_16/clip_by_value/Minimum
"while/lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_16/clip_by_value/yÓ
 while/lstm_cell_16/clip_by_valueMaximum,while/lstm_cell_16/clip_by_value/Minimum:z:0+while/lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 while/lstm_cell_16/clip_by_value}
while/lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const_2}
while/lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_3¬
while/lstm_cell_16/Mul_1Mul!while/lstm_cell_16/split:output:1#while/lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul_1©
while/lstm_cell_16/Add_2AddV2while/lstm_cell_16/Mul_1:z:0#while/lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_2¡
,while/lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_16/clip_by_value_1/Minimum/yá
*while/lstm_cell_16/clip_by_value_1/MinimumMinimumwhile/lstm_cell_16/Add_2:z:05while/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_16/clip_by_value_1/Minimum
$while/lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_16/clip_by_value_1/yÛ
"while/lstm_cell_16/clip_by_value_1Maximum.while/lstm_cell_16/clip_by_value_1/Minimum:z:0-while/lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_16/clip_by_value_1¡
while/lstm_cell_16/mul_2Mul&while/lstm_cell_16/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_2
while/lstm_cell_16/TanhTanh!while/lstm_cell_16/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_16/Tanh§
while/lstm_cell_16/mul_3Mul$while/lstm_cell_16/clip_by_value:z:0while/lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_3¢
while/lstm_cell_16/add_3AddV2while/lstm_cell_16/mul_2:z:0while/lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_16/add_3}
while/lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const_4}
while/lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_5¬
while/lstm_cell_16/Mul_4Mul!while/lstm_cell_16/split:output:3#while/lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul_4©
while/lstm_cell_16/Add_4AddV2while/lstm_cell_16/Mul_4:z:0#while/lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_4¡
,while/lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_16/clip_by_value_2/Minimum/yá
*while/lstm_cell_16/clip_by_value_2/MinimumMinimumwhile/lstm_cell_16/Add_4:z:05while/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_16/clip_by_value_2/Minimum
$while/lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_16/clip_by_value_2/yÛ
"while/lstm_cell_16/clip_by_value_2Maximum.while/lstm_cell_16/clip_by_value_2/Minimum:z:0-while/lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_16/clip_by_value_2
while/lstm_cell_16/Tanh_1Tanhwhile/lstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Tanh_1«
while/lstm_cell_16/mul_5Mul&while/lstm_cell_16/clip_by_value_2:z:0while/lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_16/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_16/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_16/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_16/BiasAdd/ReadVariableOp)^while/lstm_cell_16/MatMul/ReadVariableOp+^while/lstm_cell_16/MatMul_1/ReadVariableOp*"
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
2while_lstm_cell_16_biasadd_readvariableop_resource4while_lstm_cell_16_biasadd_readvariableop_resource_0"l
3while_lstm_cell_16_matmul_1_readvariableop_resource5while_lstm_cell_16_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_16_matmul_readvariableop_resource3while_lstm_cell_16_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_16/BiasAdd/ReadVariableOp)while/lstm_cell_16/BiasAdd/ReadVariableOp2T
(while/lstm_cell_16/MatMul/ReadVariableOp(while/lstm_cell_16/MatMul/ReadVariableOp2X
*while/lstm_cell_16/MatMul_1/ReadVariableOp*while/lstm_cell_16/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
	
ð
'__inference_lstm_17_layer_call_fn_52693
inputs_0
unknown:22
	unknown_0:22
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
:2ÿÿÿÿÿÿÿÿÿ2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_17_layer_call_and_return_conditional_losses_489052
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2ÿÿÿÿÿÿÿÿÿ: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/0
o
¶
B__inference_lstm_17_layer_call_and_return_conditional_losses_52307
inputs_0>
+lstm_cell_17_matmul_readvariableop_resource:	È?
-lstm_cell_17_matmul_1_readvariableop_resource:22B
/lstm_cell_17_matmul_1_readvariableop_1_resource:	2È;
,lstm_cell_17_biasadd_readvariableop_resource:	È<
*lstm_cell_17_mul_2_readvariableop_resource:22
identity¢AssignVariableOp¢AssignVariableOp_1¢ReadVariableOp¢ReadVariableOp_1¢#lstm_cell_17/BiasAdd/ReadVariableOp¢"lstm_cell_17/MatMul/ReadVariableOp¢$lstm_cell_17/MatMul_1/ReadVariableOp¢&lstm_cell_17/MatMul_1/ReadVariableOp_1¢!lstm_cell_17/mul_2/ReadVariableOp¢whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1µ
"lstm_cell_17/MatMul/ReadVariableOpReadVariableOp+lstm_cell_17_matmul_readvariableop_resource*
_output_shapes
:	È*
dtype02$
"lstm_cell_17/MatMul/ReadVariableOp¤
lstm_cell_17/MatMulMatMulstrided_slice_1:output:0*lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/MatMulº
$lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02&
$lstm_cell_17/MatMul_1/ReadVariableOpÁ
&lstm_cell_17/MatMul_1/ReadVariableOp_1ReadVariableOp/lstm_cell_17_matmul_1_readvariableop_1_resource*
_output_shapes
:	2È*
dtype02(
&lstm_cell_17/MatMul_1/ReadVariableOp_1À
lstm_cell_17/MatMul_1MatMul,lstm_cell_17/MatMul_1/ReadVariableOp:value:0.lstm_cell_17/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/MatMul_1
lstm_cell_17/addAddV2lstm_cell_17/MatMul:product:0lstm_cell_17/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/add´
#lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes	
:È*
dtype02%
#lstm_cell_17/BiasAdd/ReadVariableOp¤
lstm_cell_17/BiasAddBiasAddlstm_cell_17/add:z:0+lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
lstm_cell_17/BiasAdd~
lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/split/split_dimÏ
lstm_cell_17/splitSplit%lstm_cell_17/split/split_dim:output:0lstm_cell_17/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_17/splitm
lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Constq
lstm_cell_17/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_1
lstm_cell_17/MulMullstm_cell_17/split:output:0lstm_cell_17/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul
lstm_cell_17/Add_1AddV2lstm_cell_17/Mul:z:0lstm_cell_17/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_1
$lstm_cell_17/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2&
$lstm_cell_17/clip_by_value/Minimum/yÃ
"lstm_cell_17/clip_by_value/MinimumMinimumlstm_cell_17/Add_1:z:0-lstm_cell_17/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222$
"lstm_cell_17/clip_by_value/Minimum
lstm_cell_17/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_17/clip_by_value/y»
lstm_cell_17/clip_by_valueMaximum&lstm_cell_17/clip_by_value/Minimum:z:0%lstm_cell_17/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_valueq
lstm_cell_17/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Const_2q
lstm_cell_17/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_3
lstm_cell_17/Mul_1Mullstm_cell_17/split:output:1lstm_cell_17/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul_1
lstm_cell_17/Add_2AddV2lstm_cell_17/Mul_1:z:0lstm_cell_17/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_2
&lstm_cell_17/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_17/clip_by_value_1/Minimum/yÉ
$lstm_cell_17/clip_by_value_1/MinimumMinimumlstm_cell_17/Add_2:z:0/lstm_cell_17/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_17/clip_by_value_1/Minimum
lstm_cell_17/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_17/clip_by_value_1/yÃ
lstm_cell_17/clip_by_value_1Maximum(lstm_cell_17/clip_by_value_1/Minimum:z:0'lstm_cell_17/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_value_1±
!lstm_cell_17/mul_2/ReadVariableOpReadVariableOp*lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02#
!lstm_cell_17/mul_2/ReadVariableOp¥
lstm_cell_17/mul_2Mul lstm_cell_17/clip_by_value_1:z:0)lstm_cell_17/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_2t
lstm_cell_17/TanhTanhlstm_cell_17/split:output:2*
T0*
_output_shapes

:222
lstm_cell_17/Tanh
lstm_cell_17/mul_3Mullstm_cell_17/clip_by_value:z:0lstm_cell_17/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_3
lstm_cell_17/add_3AddV2lstm_cell_17/mul_2:z:0lstm_cell_17/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_17/add_3q
lstm_cell_17/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
lstm_cell_17/Const_4q
lstm_cell_17/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_17/Const_5
lstm_cell_17/Mul_4Mullstm_cell_17/split:output:3lstm_cell_17/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Mul_4
lstm_cell_17/Add_4AddV2lstm_cell_17/Mul_4:z:0lstm_cell_17/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_17/Add_4
&lstm_cell_17/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2(
&lstm_cell_17/clip_by_value_2/Minimum/yÉ
$lstm_cell_17/clip_by_value_2/MinimumMinimumlstm_cell_17/Add_4:z:0/lstm_cell_17/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222&
$lstm_cell_17/clip_by_value_2/Minimum
lstm_cell_17/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2 
lstm_cell_17/clip_by_value_2/yÃ
lstm_cell_17/clip_by_value_2Maximum(lstm_cell_17/clip_by_value_2/Minimum:z:0'lstm_cell_17/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_17/clip_by_value_2s
lstm_cell_17/Tanh_1Tanhlstm_cell_17/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_17/Tanh_1
lstm_cell_17/mul_5Mul lstm_cell_17/clip_by_value_2:z:0lstm_cell_17/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_17/mul_5
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp
ReadVariableOp_1ReadVariableOp*lstm_cell_17_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_17_matmul_readvariableop_resource/lstm_cell_17_matmul_1_readvariableop_1_resource,lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *
bodyR
while_body_52202*
condR
while_cond_52201*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
whileµ
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shapeè
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:ÿÿÿÿÿÿÿÿÿ22*
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

:22*
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
:2ÿÿÿÿÿÿÿÿÿ22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtimeÒ
AssignVariableOpAssignVariableOp-lstm_cell_17_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOpÒ
AssignVariableOp_1AssignVariableOp*lstm_cell_17_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1"^lstm_cell_17/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ22

Identityá
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_17/BiasAdd/ReadVariableOp#^lstm_cell_17/MatMul/ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp'^lstm_cell_17/MatMul_1/ReadVariableOp_1"^lstm_cell_17/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2ÿÿÿÿÿÿÿÿÿ: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_17/BiasAdd/ReadVariableOp#lstm_cell_17/BiasAdd/ReadVariableOp2H
"lstm_cell_17/MatMul/ReadVariableOp"lstm_cell_17/MatMul/ReadVariableOp2L
$lstm_cell_17/MatMul_1/ReadVariableOp$lstm_cell_17/MatMul_1/ReadVariableOp2P
&lstm_cell_17/MatMul_1/ReadVariableOp_1&lstm_cell_17/MatMul_1/ReadVariableOp_12F
!lstm_cell_17/mul_2/ReadVariableOp!lstm_cell_17/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:2ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/0
¨
¼
while_cond_52379
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_52379___redundant_placeholder03
/while_while_cond_52379___redundant_placeholder13
/while_while_cond_52379___redundant_placeholder23
/while_while_cond_52379___redundant_placeholder3
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
.: : : : :22:22: ::::: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
:
ÔY
Ë
while_body_53152
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0F
3while_lstm_cell_16_matmul_readvariableop_resource_0:	2ÈH
5while_lstm_cell_16_matmul_1_readvariableop_resource_0:	2ÈC
4while_lstm_cell_16_biasadd_readvariableop_resource_0:	È
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorD
1while_lstm_cell_16_matmul_readvariableop_resource:	2ÈF
3while_lstm_cell_16_matmul_1_readvariableop_resource:	2ÈA
2while_lstm_cell_16_biasadd_readvariableop_resource:	È¢)while/lstm_cell_16/BiasAdd/ReadVariableOp¢(while/lstm_cell_16/MatMul/ReadVariableOp¢*while/lstm_cell_16/MatMul_1/ReadVariableOpÃ
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shapeÊ
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItemÉ
(while/lstm_cell_16/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_16_matmul_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02*
(while/lstm_cell_16/MatMul/ReadVariableOpÎ
while/lstm_cell_16/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_16/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/MatMulÏ
*while/lstm_cell_16/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_16_matmul_1_readvariableop_resource_0*
_output_shapes
:	2È*
dtype02,
*while/lstm_cell_16/MatMul_1/ReadVariableOp·
while/lstm_cell_16/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_16/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/MatMul_1¯
while/lstm_cell_16/addAddV2#while/lstm_cell_16/MatMul:product:0%while/lstm_cell_16/MatMul_1:product:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/addÈ
)while/lstm_cell_16/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_16_biasadd_readvariableop_resource_0*
_output_shapes	
:È*
dtype02+
)while/lstm_cell_16/BiasAdd/ReadVariableOp¼
while/lstm_cell_16/BiasAddBiasAddwhile/lstm_cell_16/add:z:01while/lstm_cell_16/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2È2
while/lstm_cell_16/BiasAdd
"while/lstm_cell_16/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_16/split/split_dimç
while/lstm_cell_16/splitSplit+while/lstm_cell_16/split/split_dim:output:0#while/lstm_cell_16/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_16/splity
while/lstm_cell_16/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const}
while/lstm_cell_16/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_1¦
while/lstm_cell_16/MulMul!while/lstm_cell_16/split:output:0!while/lstm_cell_16/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul§
while/lstm_cell_16/Add_1AddV2while/lstm_cell_16/Mul:z:0#while/lstm_cell_16/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_1
*while/lstm_cell_16/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2,
*while/lstm_cell_16/clip_by_value/Minimum/yÛ
(while/lstm_cell_16/clip_by_value/MinimumMinimumwhile/lstm_cell_16/Add_1:z:03while/lstm_cell_16/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(while/lstm_cell_16/clip_by_value/Minimum
"while/lstm_cell_16/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"while/lstm_cell_16/clip_by_value/yÓ
 while/lstm_cell_16/clip_by_valueMaximum,while/lstm_cell_16/clip_by_value/Minimum:z:0+while/lstm_cell_16/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 while/lstm_cell_16/clip_by_value}
while/lstm_cell_16/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const_2}
while/lstm_cell_16/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_3¬
while/lstm_cell_16/Mul_1Mul!while/lstm_cell_16/split:output:1#while/lstm_cell_16/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul_1©
while/lstm_cell_16/Add_2AddV2while/lstm_cell_16/Mul_1:z:0#while/lstm_cell_16/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_2¡
,while/lstm_cell_16/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_16/clip_by_value_1/Minimum/yá
*while/lstm_cell_16/clip_by_value_1/MinimumMinimumwhile/lstm_cell_16/Add_2:z:05while/lstm_cell_16/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_16/clip_by_value_1/Minimum
$while/lstm_cell_16/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_16/clip_by_value_1/yÛ
"while/lstm_cell_16/clip_by_value_1Maximum.while/lstm_cell_16/clip_by_value_1/Minimum:z:0-while/lstm_cell_16/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_16/clip_by_value_1¡
while/lstm_cell_16/mul_2Mul&while/lstm_cell_16/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_2
while/lstm_cell_16/TanhTanh!while/lstm_cell_16/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_16/Tanh§
while/lstm_cell_16/mul_3Mul$while/lstm_cell_16/clip_by_value:z:0while/lstm_cell_16/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_3¢
while/lstm_cell_16/add_3AddV2while/lstm_cell_16/mul_2:z:0while/lstm_cell_16/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_16/add_3}
while/lstm_cell_16/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *ÍÌL>2
while/lstm_cell_16/Const_4}
while/lstm_cell_16/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_16/Const_5¬
while/lstm_cell_16/Mul_4Mul!while/lstm_cell_16/split:output:3#while/lstm_cell_16/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Mul_4©
while/lstm_cell_16/Add_4AddV2while/lstm_cell_16/Mul_4:z:0#while/lstm_cell_16/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Add_4¡
,while/lstm_cell_16/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ?2.
,while/lstm_cell_16/clip_by_value_2/Minimum/yá
*while/lstm_cell_16/clip_by_value_2/MinimumMinimumwhile/lstm_cell_16/Add_4:z:05while/lstm_cell_16/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*while/lstm_cell_16/clip_by_value_2/Minimum
$while/lstm_cell_16/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$while/lstm_cell_16/clip_by_value_2/yÛ
"while/lstm_cell_16/clip_by_value_2Maximum.while/lstm_cell_16/clip_by_value_2/Minimum:z:0-while/lstm_cell_16/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"while/lstm_cell_16/clip_by_value_2
while/lstm_cell_16/Tanh_1Tanhwhile/lstm_cell_16/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_16/Tanh_1«
while/lstm_cell_16/mul_5Mul&while/lstm_cell_16/clip_by_value_2:z:0while/lstm_cell_16/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_16/mul_5à
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_16/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_16/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4
while/Identity_5Identitywhile/lstm_cell_16/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5Þ

while/NoOpNoOp*^while/lstm_cell_16/BiasAdd/ReadVariableOp)^while/lstm_cell_16/MatMul/ReadVariableOp+^while/lstm_cell_16/MatMul_1/ReadVariableOp*"
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
2while_lstm_cell_16_biasadd_readvariableop_resource4while_lstm_cell_16_biasadd_readvariableop_resource_0"l
3while_lstm_cell_16_matmul_1_readvariableop_resource5while_lstm_cell_16_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_16_matmul_readvariableop_resource3while_lstm_cell_16_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"¨
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_16/BiasAdd/ReadVariableOp)while/lstm_cell_16/BiasAdd/ReadVariableOp2T
(while/lstm_cell_16/MatMul/ReadVariableOp(while/lstm_cell_16/MatMul/ReadVariableOp2X
*while/lstm_cell_16/MatMul_1/ReadVariableOp*while/lstm_cell_16/MatMul_1/ReadVariableOp: 
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

:22:$ 

_output_shapes

:22:

_output_shapes
: :

_output_shapes
: 
ã
ú
G__inference_sequential_8_layer_call_and_return_conditional_losses_51095
lstm_17_input 
lstm_17_51065:	È
lstm_17_51067:22 
lstm_17_51069:	2È
lstm_17_51071:	È
lstm_17_51073:22 
lstm_16_51076:	2È
lstm_16_51078:22 
lstm_16_51080:	2È
lstm_16_51082:	È
lstm_16_51084:22*
time_distributed_8_51087:2&
time_distributed_8_51089:
identity¢lstm_16/StatefulPartitionedCall¢lstm_17/StatefulPartitionedCall¢*time_distributed_8/StatefulPartitionedCall¿
lstm_17/StatefulPartitionedCallStatefulPartitionedCalllstm_17_inputlstm_17_51065lstm_17_51067lstm_17_51069lstm_17_51071lstm_17_51073*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_17_layer_call_and_return_conditional_losses_502442!
lstm_17/StatefulPartitionedCallÚ
lstm_16/StatefulPartitionedCallStatefulPartitionedCall(lstm_17/StatefulPartitionedCall:output:0lstm_16_51076lstm_16_51078lstm_16_51080lstm_16_51082lstm_16_51084*
Tin

2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *K
fFRD
B__inference_lstm_16_layer_call_and_return_conditional_losses_504332!
lstm_16/StatefulPartitionedCallà
*time_distributed_8/StatefulPartitionedCallStatefulPartitionedCall(lstm_16/StatefulPartitionedCall:output:0time_distributed_8_51087time_distributed_8_51089*
Tin
2*
Tout
2*
_collective_manager_ids
 *"
_output_shapes
:2
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8 *V
fQRO
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_504582,
*time_distributed_8/StatefulPartitionedCall
 time_distributed_8/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"ÿÿÿÿ2   2"
 time_distributed_8/Reshape/shapeÂ
time_distributed_8/ReshapeReshape(lstm_16/StatefulPartitionedCall:output:0)time_distributed_8/Reshape/shape:output:0*
T0*
_output_shapes
:	ô22
time_distributed_8/Reshape
IdentityIdentity3time_distributed_8/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity¿
NoOpNoOp ^lstm_16/StatefulPartitionedCall ^lstm_17/StatefulPartitionedCall+^time_distributed_8/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 2B
lstm_16/StatefulPartitionedCalllstm_16/StatefulPartitionedCall2B
lstm_17/StatefulPartitionedCalllstm_17/StatefulPartitionedCall2X
*time_distributed_8/StatefulPartitionedCall*time_distributed_8/StatefulPartitionedCall:Q M
"
_output_shapes
:2

'
_user_specified_namelstm_17_input"¨L
saver_filename:0StatefulPartitionedCall_1:0StatefulPartitionedCall_28"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*·
serving_default£
B
lstm_17_input1
serving_default_lstm_17_input:02
A
time_distributed_8+
StatefulPartitionedCall:02
tensorflow/serving/predict:ûÃ
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
*z&call_and_return_all_conditional_losses
{__call__
|_default_save_signature"
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
*}&call_and_return_all_conditional_losses
~__call__"
_tf_keras_rnn_layer
Ä
cell

state_spec
trainable_variables
	variables
regularization_losses
	keras_api
*&call_and_return_all_conditional_losses
__call__"
_tf_keras_rnn_layer
²
	layer
trainable_variables
	variables
regularization_losses
	keras_api
+&call_and_return_all_conditional_losses
__call__"
_tf_keras_layer
ã
iter

beta_1

beta_2
	decay
learning_rate mj!mk"ml#mm$mn%mo&mp'mq vr!vs"vt#vu$vv%vw&vx'vy"
	optimizer
X
 0
!1
"2
#3
$4
%5
&6
'7"
trackable_list_wrapper
X
 0
!1
"2
#3
$4
%5
&6
'7"
trackable_list_wrapper
 "
trackable_list_wrapper
Ê
trainable_variables

(layers
	variables
)metrics
regularization_losses
*layer_metrics
+layer_regularization_losses
,non_trainable_variables
{__call__
|_default_save_signature
*z&call_and_return_all_conditional_losses
&z"call_and_return_conditional_losses"
_generic_user_object
-
serving_default"
signature_map
ã
-
state_size

 kernel
!recurrent_kernel
"bias
.trainable_variables
/	variables
0regularization_losses
1	keras_api
+&call_and_return_all_conditional_losses
__call__"
_tf_keras_layer
 "
trackable_list_wrapper
5
 0
!1
"2"
trackable_list_wrapper
5
 0
!1
"2"
trackable_list_wrapper
 "
trackable_list_wrapper
¹
trainable_variables

2layers
	variables
3metrics
regularization_losses
4layer_metrics
5layer_regularization_losses
6non_trainable_variables

7states
~__call__
*}&call_and_return_all_conditional_losses
&}"call_and_return_conditional_losses"
_generic_user_object
ã
8
state_size

#kernel
$recurrent_kernel
%bias
9trainable_variables
:	variables
;regularization_losses
<	keras_api
+&call_and_return_all_conditional_losses
__call__"
_tf_keras_layer
 "
trackable_list_wrapper
5
#0
$1
%2"
trackable_list_wrapper
5
#0
$1
%2"
trackable_list_wrapper
 "
trackable_list_wrapper
º
trainable_variables

=layers
	variables
>metrics
regularization_losses
?layer_metrics
@layer_regularization_losses
Anon_trainable_variables

Bstates
__call__
*&call_and_return_all_conditional_losses
&"call_and_return_conditional_losses"
_generic_user_object
½

&kernel
'bias
Ctrainable_variables
D	variables
Eregularization_losses
F	keras_api
+&call_and_return_all_conditional_losses
__call__"
_tf_keras_layer
.
&0
'1"
trackable_list_wrapper
.
&0
'1"
trackable_list_wrapper
 "
trackable_list_wrapper
°
trainable_variables

Glayers
	variables
Hmetrics
regularization_losses
Ilayer_metrics
Jlayer_regularization_losses
Knon_trainable_variables
__call__
+&call_and_return_all_conditional_losses
'"call_and_return_conditional_losses"
_generic_user_object
:	 (2	Adam/iter
: (2Adam/beta_1
: (2Adam/beta_2
: (2
Adam/decay
: (2Adam/learning_rate
.:,	È2lstm_17/lstm_cell_17/kernel
8:6	2È2%lstm_17/lstm_cell_17/recurrent_kernel
(:&È2lstm_17/lstm_cell_17/bias
.:,	2È2lstm_16/lstm_cell_16/kernel
8:6	2È2%lstm_16/lstm_cell_16/recurrent_kernel
(:&È2lstm_16/lstm_cell_16/bias
+:)22time_distributed_8/kernel
%:#2time_distributed_8/bias
5
0
1
2"
trackable_list_wrapper
.
L0
M1"
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
 0
!1
"2"
trackable_list_wrapper
5
 0
!1
"2"
trackable_list_wrapper
 "
trackable_list_wrapper
°
.trainable_variables

Nlayers
/	variables
Ometrics
0regularization_losses
Player_metrics
Qlayer_regularization_losses
Rnon_trainable_variables
__call__
+&call_and_return_all_conditional_losses
'"call_and_return_conditional_losses"
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
S0
T1"
trackable_list_wrapper
 "
trackable_list_wrapper
5
#0
$1
%2"
trackable_list_wrapper
5
#0
$1
%2"
trackable_list_wrapper
 "
trackable_list_wrapper
°
9trainable_variables

Ulayers
:	variables
Vmetrics
;regularization_losses
Wlayer_metrics
Xlayer_regularization_losses
Ynon_trainable_variables
__call__
+&call_and_return_all_conditional_losses
'"call_and_return_conditional_losses"
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
Z0
[1"
trackable_list_wrapper
.
&0
'1"
trackable_list_wrapper
.
&0
'1"
trackable_list_wrapper
 "
trackable_list_wrapper
°
Ctrainable_variables

\layers
D	variables
]metrics
Eregularization_losses
^layer_metrics
_layer_regularization_losses
`non_trainable_variables
__call__
+&call_and_return_all_conditional_losses
'"call_and_return_conditional_losses"
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
	atotal
	bcount
c	variables
d	keras_api"
_tf_keras_metric
^
	etotal
	fcount
g
_fn_kwargs
h	variables
i	keras_api"
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
": 222lstm_17/Variable
": 222lstm_17/Variable
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
": 222lstm_16/Variable
": 222lstm_16/Variable
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
a0
b1"
trackable_list_wrapper
-
c	variables"
_generic_user_object
:  (2total
:  (2count
 "
trackable_dict_wrapper
.
e0
f1"
trackable_list_wrapper
-
h	variables"
_generic_user_object
3:1	È2"Adam/lstm_17/lstm_cell_17/kernel/m
=:;	2È2,Adam/lstm_17/lstm_cell_17/recurrent_kernel/m
-:+È2 Adam/lstm_17/lstm_cell_17/bias/m
3:1	2È2"Adam/lstm_16/lstm_cell_16/kernel/m
=:;	2È2,Adam/lstm_16/lstm_cell_16/recurrent_kernel/m
-:+È2 Adam/lstm_16/lstm_cell_16/bias/m
0:.22 Adam/time_distributed_8/kernel/m
*:(2Adam/time_distributed_8/bias/m
3:1	È2"Adam/lstm_17/lstm_cell_17/kernel/v
=:;	2È2,Adam/lstm_17/lstm_cell_17/recurrent_kernel/v
-:+È2 Adam/lstm_17/lstm_cell_17/bias/v
3:1	2È2"Adam/lstm_16/lstm_cell_16/kernel/v
=:;	2È2,Adam/lstm_16/lstm_cell_16/recurrent_kernel/v
-:+È2 Adam/lstm_16/lstm_cell_16/bias/v
0:.22 Adam/time_distributed_8/kernel/v
*:(2Adam/time_distributed_8/bias/v
ê2ç
G__inference_sequential_8_layer_call_and_return_conditional_losses_51529
G__inference_sequential_8_layer_call_and_return_conditional_losses_51893
G__inference_sequential_8_layer_call_and_return_conditional_losses_51095
G__inference_sequential_8_layer_call_and_return_conditional_losses_51128À
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
þ2û
,__inference_sequential_8_layer_call_fn_50494
,__inference_sequential_8_layer_call_fn_51922
,__inference_sequential_8_layer_call_fn_51951
,__inference_sequential_8_layer_call_fn_51062À
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
 __inference__wrapped_model_48369lstm_17_input"
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
B__inference_lstm_17_layer_call_and_return_conditional_losses_52129
B__inference_lstm_17_layer_call_and_return_conditional_losses_52307
B__inference_lstm_17_layer_call_and_return_conditional_losses_52485
B__inference_lstm_17_layer_call_and_return_conditional_losses_52663Õ
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
'__inference_lstm_17_layer_call_fn_52678
'__inference_lstm_17_layer_call_fn_52693
'__inference_lstm_17_layer_call_fn_52708
'__inference_lstm_17_layer_call_fn_52723Õ
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
B__inference_lstm_16_layer_call_and_return_conditional_losses_52901
B__inference_lstm_16_layer_call_and_return_conditional_losses_53079
B__inference_lstm_16_layer_call_and_return_conditional_losses_53257
B__inference_lstm_16_layer_call_and_return_conditional_losses_53435Õ
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
'__inference_lstm_16_layer_call_fn_53450
'__inference_lstm_16_layer_call_fn_53465
'__inference_lstm_16_layer_call_fn_53480
'__inference_lstm_16_layer_call_fn_53495Õ
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
2ÿ
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_53516
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_53537
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_53551
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_53565À
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
2
2__inference_time_distributed_8_layer_call_fn_53574
2__inference_time_distributed_8_layer_call_fn_53583
2__inference_time_distributed_8_layer_call_fn_53592
2__inference_time_distributed_8_layer_call_fn_53601À
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
#__inference_signature_wrapper_51165lstm_17_input"
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
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53658
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53711
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53764
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53821¾
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
,__inference_lstm_cell_17_layer_call_fn_53838
,__inference_lstm_cell_17_layer_call_fn_53855
,__inference_lstm_cell_17_layer_call_fn_53929
,__inference_lstm_cell_17_layer_call_fn_54003¾
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
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54060
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54113
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54166
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54223¾
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
,__inference_lstm_cell_16_layer_call_fn_54240
,__inference_lstm_cell_16_layer_call_fn_54257
,__inference_lstm_cell_16_layer_call_fn_54331
,__inference_lstm_cell_16_layer_call_fn_54405¾
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
ì2é
B__inference_dense_8_layer_call_and_return_conditional_losses_54415¢
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
Ñ2Î
'__inference_dense_8_layer_call_fn_54424¢
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
 ª
 __inference__wrapped_model_48369 S!"T#Z$%[&'1¢.
'¢$
"
lstm_17_input2

ª "Bª?
=
time_distributed_8'$
time_distributed_82
¢
B__inference_dense_8_layer_call_and_return_conditional_losses_54415\&'/¢,
%¢"
 
inputsÿÿÿÿÿÿÿÿÿ2
ª "%¢"

0ÿÿÿÿÿÿÿÿÿ
 z
'__inference_dense_8_layer_call_fn_54424O&'/¢,
%¢"
 
inputsÿÿÿÿÿÿÿÿÿ2
ª "ÿÿÿÿÿÿÿÿÿÀ
B__inference_lstm_16_layer_call_and_return_conditional_losses_52901z#Z$%[F¢C
<¢9
+(
&#
inputs/02ÿÿÿÿÿÿÿÿÿ2

 
p 

 
ª ")¢&

02ÿÿÿÿÿÿÿÿÿ2
 À
B__inference_lstm_16_layer_call_and_return_conditional_losses_53079z#Z$%[F¢C
<¢9
+(
&#
inputs/02ÿÿÿÿÿÿÿÿÿ2

 
p

 
ª ")¢&

02ÿÿÿÿÿÿÿÿÿ2
 §
B__inference_lstm_16_layer_call_and_return_conditional_losses_53257a#Z$%[6¢3
,¢)

inputs2
2

 
p 

 
ª " ¢

02
2
 §
B__inference_lstm_16_layer_call_and_return_conditional_losses_53435a#Z$%[6¢3
,¢)

inputs2
2

 
p

 
ª " ¢

02
2
 
'__inference_lstm_16_layer_call_fn_53450mZ[#$%F¢C
<¢9
+(
&#
inputs/02ÿÿÿÿÿÿÿÿÿ2

 
p 

 
ª "2ÿÿÿÿÿÿÿÿÿ2
'__inference_lstm_16_layer_call_fn_53465mZ[#$%F¢C
<¢9
+(
&#
inputs/02ÿÿÿÿÿÿÿÿÿ2

 
p

 
ª "2ÿÿÿÿÿÿÿÿÿ2
'__inference_lstm_16_layer_call_fn_53480T#Z$%[6¢3
,¢)

inputs2
2

 
p 

 
ª "2
2
'__inference_lstm_16_layer_call_fn_53495T#Z$%[6¢3
,¢)

inputs2
2

 
p

 
ª "2
2À
B__inference_lstm_17_layer_call_and_return_conditional_losses_52129z S!"TF¢C
<¢9
+(
&#
inputs/02ÿÿÿÿÿÿÿÿÿ

 
p 

 
ª ")¢&

02ÿÿÿÿÿÿÿÿÿ2
 À
B__inference_lstm_17_layer_call_and_return_conditional_losses_52307z S!"TF¢C
<¢9
+(
&#
inputs/02ÿÿÿÿÿÿÿÿÿ

 
p

 
ª ")¢&

02ÿÿÿÿÿÿÿÿÿ2
 §
B__inference_lstm_17_layer_call_and_return_conditional_losses_52485a S!"T6¢3
,¢)

inputs2


 
p 

 
ª " ¢

02
2
 §
B__inference_lstm_17_layer_call_and_return_conditional_losses_52663a S!"T6¢3
,¢)

inputs2


 
p

 
ª " ¢

02
2
 
'__inference_lstm_17_layer_call_fn_52678mST !"F¢C
<¢9
+(
&#
inputs/02ÿÿÿÿÿÿÿÿÿ

 
p 

 
ª "2ÿÿÿÿÿÿÿÿÿ2
'__inference_lstm_17_layer_call_fn_52693mST !"F¢C
<¢9
+(
&#
inputs/02ÿÿÿÿÿÿÿÿÿ

 
p

 
ª "2ÿÿÿÿÿÿÿÿÿ2
'__inference_lstm_17_layer_call_fn_52708T S!"T6¢3
,¢)

inputs2


 
p 

 
ª "2
2
'__inference_lstm_17_layer_call_fn_52723T S!"T6¢3
,¢)

inputs2


 
p

 
ª "2
2¾
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54060ò#$%¢¢
¢

inputs22
s¢p
63	!¢
ú22


jstates/0VariableSpec
63	!¢
ú22
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
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54113Æ#$%e¢b
[¢X

inputs22
9¢6

states/022

states/122
p 
ª "X¢U
N¢K

0/022
30

0/1/022

0/1/122
 
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54166Æ#$%e¢b
[¢X

inputs22
9¢6

states/022

states/122
p
ª "X¢U
N¢K

0/022
30

0/1/022

0/1/122
 ¾
G__inference_lstm_cell_16_layer_call_and_return_conditional_losses_54223ò#$%¢¢
¢

inputs22
s¢p
63	!¢
ú22


jstates/0VariableSpec
63	!¢
ú22
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
,__inference_lstm_cell_16_layer_call_fn_54240¶#$%e¢b
[¢X

inputs22
9¢6

states/022

states/122
p 
ª "H¢E

022
/,

1/022

1/122ç
,__inference_lstm_cell_16_layer_call_fn_54257¶#$%e¢b
[¢X

inputs22
9¢6

states/022

states/122
p
ª "H¢E

022
/,

1/022

1/122
,__inference_lstm_cell_16_layer_call_fn_54331â#$%¢¢
¢

inputs22
s¢p
63	!¢
ú22


jstates/0VariableSpec
63	!¢
ú22
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
,__inference_lstm_cell_16_layer_call_fn_54405â#$%¢¢
¢

inputs22
s¢p
63	!¢
ú22


jstates/0VariableSpec
63	!¢
ú22
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
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53658ò !"¢¢
¢

inputs2
s¢p
63	!¢
ú22


jstates/0VariableSpec
63	!¢
ú22
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
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53711Æ !"e¢b
[¢X

inputs2
9¢6

states/022

states/122
p 
ª "X¢U
N¢K

0/022
30

0/1/022

0/1/122
 
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53764Æ !"e¢b
[¢X

inputs2
9¢6

states/022

states/122
p
ª "X¢U
N¢K

0/022
30

0/1/022

0/1/122
 ¾
G__inference_lstm_cell_17_layer_call_and_return_conditional_losses_53821ò !"¢¢
¢

inputs2
s¢p
63	!¢
ú22


jstates/0VariableSpec
63	!¢
ú22
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
,__inference_lstm_cell_17_layer_call_fn_53838¶ !"e¢b
[¢X

inputs2
9¢6

states/022

states/122
p 
ª "H¢E

022
/,

1/022

1/122ç
,__inference_lstm_cell_17_layer_call_fn_53855¶ !"e¢b
[¢X

inputs2
9¢6

states/022

states/122
p
ª "H¢E

022
/,

1/022

1/122
,__inference_lstm_cell_17_layer_call_fn_53929â !"¢¢
¢

inputs2
s¢p
63	!¢
ú22


jstates/0VariableSpec
63	!¢
ú22
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
,__inference_lstm_cell_17_layer_call_fn_54003â !"¢¢
¢

inputs2
s¢p
63	!¢
ú22


jstates/0VariableSpec
63	!¢
ú22
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
1/1¶
G__inference_sequential_8_layer_call_and_return_conditional_losses_51095k S!"T#Z$%[&'9¢6
/¢,
"
lstm_17_input2

p 

 
ª " ¢

02

 ¶
G__inference_sequential_8_layer_call_and_return_conditional_losses_51128k S!"T#Z$%[&'9¢6
/¢,
"
lstm_17_input2

p

 
ª " ¢

02

 ¯
G__inference_sequential_8_layer_call_and_return_conditional_losses_51529d S!"T#Z$%[&'2¢/
(¢%

inputs2

p 

 
ª " ¢

02

 ¯
G__inference_sequential_8_layer_call_and_return_conditional_losses_51893d S!"T#Z$%[&'2¢/
(¢%

inputs2

p

 
ª " ¢

02

 
,__inference_sequential_8_layer_call_fn_50494^ S!"T#Z$%[&'9¢6
/¢,
"
lstm_17_input2

p 

 
ª "2

,__inference_sequential_8_layer_call_fn_51062^ S!"T#Z$%[&'9¢6
/¢,
"
lstm_17_input2

p

 
ª "2

,__inference_sequential_8_layer_call_fn_51922W S!"T#Z$%[&'2¢/
(¢%

inputs2

p 

 
ª "2

,__inference_sequential_8_layer_call_fn_51951W S!"T#Z$%[&'2¢/
(¢%

inputs2

p

 
ª "2
¾
#__inference_signature_wrapper_51165 S!"T#Z$%[&'B¢?
¢ 
8ª5
3
lstm_17_input"
lstm_17_input2
"Bª?
=
time_distributed_8'$
time_distributed_82
Ï
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_53516~&'D¢A
:¢7
-*
inputsÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
p 

 
ª "2¢/
(%
0ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ
 Ï
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_53537~&'D¢A
:¢7
-*
inputsÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
p

 
ª "2¢/
(%
0ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ
 «
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_53551Z&'2¢/
(¢%

inputs2
2
p 

 
ª " ¢

02

 «
M__inference_time_distributed_8_layer_call_and_return_conditional_losses_53565Z&'2¢/
(¢%

inputs2
2
p

 
ª " ¢

02

 §
2__inference_time_distributed_8_layer_call_fn_53574q&'D¢A
:¢7
-*
inputsÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
p 

 
ª "%"ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ§
2__inference_time_distributed_8_layer_call_fn_53583q&'D¢A
:¢7
-*
inputsÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ2
p

 
ª "%"ÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿÿ
2__inference_time_distributed_8_layer_call_fn_53592M&'2¢/
(¢%

inputs2
2
p 

 
ª "2

2__inference_time_distributed_8_layer_call_fn_53601M&'2¢/
(¢%

inputs2
2
p

 
ª "2
