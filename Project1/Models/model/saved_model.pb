ױ3
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
?"serve*2.6.02v2.6.0-rc2-32-g919f693420e8??1
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
?
lstm_5/lstm_cell_7/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?**
shared_namelstm_5/lstm_cell_7/kernel
?
-lstm_5/lstm_cell_7/kernel/Read/ReadVariableOpReadVariableOplstm_5/lstm_cell_7/kernel*
_output_shapes
:	?*
dtype0
?
#lstm_5/lstm_cell_7/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2?*4
shared_name%#lstm_5/lstm_cell_7/recurrent_kernel
?
7lstm_5/lstm_cell_7/recurrent_kernel/Read/ReadVariableOpReadVariableOp#lstm_5/lstm_cell_7/recurrent_kernel*
_output_shapes
:	2?*
dtype0
?
lstm_5/lstm_cell_7/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*(
shared_namelstm_5/lstm_cell_7/bias
?
+lstm_5/lstm_cell_7/bias/Read/ReadVariableOpReadVariableOplstm_5/lstm_cell_7/bias*
_output_shapes	
:?*
dtype0
?
lstm_4/lstm_cell_6/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2?**
shared_namelstm_4/lstm_cell_6/kernel
?
-lstm_4/lstm_cell_6/kernel/Read/ReadVariableOpReadVariableOplstm_4/lstm_cell_6/kernel*
_output_shapes
:	2?*
dtype0
?
#lstm_4/lstm_cell_6/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2?*4
shared_name%#lstm_4/lstm_cell_6/recurrent_kernel
?
7lstm_4/lstm_cell_6/recurrent_kernel/Read/ReadVariableOpReadVariableOp#lstm_4/lstm_cell_6/recurrent_kernel*
_output_shapes
:	2?*
dtype0
?
lstm_4/lstm_cell_6/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*(
shared_namelstm_4/lstm_cell_6/bias
?
+lstm_4/lstm_cell_6/bias/Read/ReadVariableOpReadVariableOplstm_4/lstm_cell_6/bias*
_output_shapes	
:?*
dtype0
?
time_distributed_2/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:2**
shared_nametime_distributed_2/kernel
?
-time_distributed_2/kernel/Read/ReadVariableOpReadVariableOptime_distributed_2/kernel*
_output_shapes

:2*
dtype0
?
time_distributed_2/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*(
shared_nametime_distributed_2/bias

+time_distributed_2/bias/Read/ReadVariableOpReadVariableOptime_distributed_2/bias*
_output_shapes
:*
dtype0
z
lstm_5/VariableVarHandleOp*
_output_shapes
: *
dtype0*
shape
:22* 
shared_namelstm_5/Variable
s
#lstm_5/Variable/Read/ReadVariableOpReadVariableOplstm_5/Variable*
_output_shapes

:22*
dtype0
~
lstm_5/Variable_1VarHandleOp*
_output_shapes
: *
dtype0*
shape
:22*"
shared_namelstm_5/Variable_1
w
%lstm_5/Variable_1/Read/ReadVariableOpReadVariableOplstm_5/Variable_1*
_output_shapes

:22*
dtype0
z
lstm_4/VariableVarHandleOp*
_output_shapes
: *
dtype0*
shape
:22* 
shared_namelstm_4/Variable
s
#lstm_4/Variable/Read/ReadVariableOpReadVariableOplstm_4/Variable*
_output_shapes

:22*
dtype0
~
lstm_4/Variable_1VarHandleOp*
_output_shapes
: *
dtype0*
shape
:22*"
shared_namelstm_4/Variable_1
w
%lstm_4/Variable_1/Read/ReadVariableOpReadVariableOplstm_4/Variable_1*
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
?
 Adam/lstm_5/lstm_cell_7/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?*1
shared_name" Adam/lstm_5/lstm_cell_7/kernel/m
?
4Adam/lstm_5/lstm_cell_7/kernel/m/Read/ReadVariableOpReadVariableOp Adam/lstm_5/lstm_cell_7/kernel/m*
_output_shapes
:	?*
dtype0
?
*Adam/lstm_5/lstm_cell_7/recurrent_kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2?*;
shared_name,*Adam/lstm_5/lstm_cell_7/recurrent_kernel/m
?
>Adam/lstm_5/lstm_cell_7/recurrent_kernel/m/Read/ReadVariableOpReadVariableOp*Adam/lstm_5/lstm_cell_7/recurrent_kernel/m*
_output_shapes
:	2?*
dtype0
?
Adam/lstm_5/lstm_cell_7/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*/
shared_name Adam/lstm_5/lstm_cell_7/bias/m
?
2Adam/lstm_5/lstm_cell_7/bias/m/Read/ReadVariableOpReadVariableOpAdam/lstm_5/lstm_cell_7/bias/m*
_output_shapes	
:?*
dtype0
?
 Adam/lstm_4/lstm_cell_6/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2?*1
shared_name" Adam/lstm_4/lstm_cell_6/kernel/m
?
4Adam/lstm_4/lstm_cell_6/kernel/m/Read/ReadVariableOpReadVariableOp Adam/lstm_4/lstm_cell_6/kernel/m*
_output_shapes
:	2?*
dtype0
?
*Adam/lstm_4/lstm_cell_6/recurrent_kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2?*;
shared_name,*Adam/lstm_4/lstm_cell_6/recurrent_kernel/m
?
>Adam/lstm_4/lstm_cell_6/recurrent_kernel/m/Read/ReadVariableOpReadVariableOp*Adam/lstm_4/lstm_cell_6/recurrent_kernel/m*
_output_shapes
:	2?*
dtype0
?
Adam/lstm_4/lstm_cell_6/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*/
shared_name Adam/lstm_4/lstm_cell_6/bias/m
?
2Adam/lstm_4/lstm_cell_6/bias/m/Read/ReadVariableOpReadVariableOpAdam/lstm_4/lstm_cell_6/bias/m*
_output_shapes	
:?*
dtype0
?
 Adam/time_distributed_2/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:2*1
shared_name" Adam/time_distributed_2/kernel/m
?
4Adam/time_distributed_2/kernel/m/Read/ReadVariableOpReadVariableOp Adam/time_distributed_2/kernel/m*
_output_shapes

:2*
dtype0
?
Adam/time_distributed_2/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*/
shared_name Adam/time_distributed_2/bias/m
?
2Adam/time_distributed_2/bias/m/Read/ReadVariableOpReadVariableOpAdam/time_distributed_2/bias/m*
_output_shapes
:*
dtype0
?
 Adam/lstm_5/lstm_cell_7/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	?*1
shared_name" Adam/lstm_5/lstm_cell_7/kernel/v
?
4Adam/lstm_5/lstm_cell_7/kernel/v/Read/ReadVariableOpReadVariableOp Adam/lstm_5/lstm_cell_7/kernel/v*
_output_shapes
:	?*
dtype0
?
*Adam/lstm_5/lstm_cell_7/recurrent_kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2?*;
shared_name,*Adam/lstm_5/lstm_cell_7/recurrent_kernel/v
?
>Adam/lstm_5/lstm_cell_7/recurrent_kernel/v/Read/ReadVariableOpReadVariableOp*Adam/lstm_5/lstm_cell_7/recurrent_kernel/v*
_output_shapes
:	2?*
dtype0
?
Adam/lstm_5/lstm_cell_7/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*/
shared_name Adam/lstm_5/lstm_cell_7/bias/v
?
2Adam/lstm_5/lstm_cell_7/bias/v/Read/ReadVariableOpReadVariableOpAdam/lstm_5/lstm_cell_7/bias/v*
_output_shapes	
:?*
dtype0
?
 Adam/lstm_4/lstm_cell_6/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2?*1
shared_name" Adam/lstm_4/lstm_cell_6/kernel/v
?
4Adam/lstm_4/lstm_cell_6/kernel/v/Read/ReadVariableOpReadVariableOp Adam/lstm_4/lstm_cell_6/kernel/v*
_output_shapes
:	2?*
dtype0
?
*Adam/lstm_4/lstm_cell_6/recurrent_kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	2?*;
shared_name,*Adam/lstm_4/lstm_cell_6/recurrent_kernel/v
?
>Adam/lstm_4/lstm_cell_6/recurrent_kernel/v/Read/ReadVariableOpReadVariableOp*Adam/lstm_4/lstm_cell_6/recurrent_kernel/v*
_output_shapes
:	2?*
dtype0
?
Adam/lstm_4/lstm_cell_6/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:?*/
shared_name Adam/lstm_4/lstm_cell_6/bias/v
?
2Adam/lstm_4/lstm_cell_6/bias/v/Read/ReadVariableOpReadVariableOpAdam/lstm_4/lstm_cell_6/bias/v*
_output_shapes	
:?*
dtype0
?
 Adam/time_distributed_2/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:2*1
shared_name" Adam/time_distributed_2/kernel/v
?
4Adam/time_distributed_2/kernel/v/Read/ReadVariableOpReadVariableOp Adam/time_distributed_2/kernel/v*
_output_shapes

:2*
dtype0
?
Adam/time_distributed_2/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*/
shared_name Adam/time_distributed_2/bias/v
?
2Adam/time_distributed_2/bias/v/Read/ReadVariableOpReadVariableOpAdam/time_distributed_2/bias/v*
_output_shapes
:*
dtype0

NoOpNoOp
?7
ConstConst"/device:CPU:0*
_output_shapes
: *
dtype0*?6
value?6B?6 B?6
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
?
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
?
(layer_regularization_losses
	variables
)non_trainable_variables
trainable_variables

*layers
+layer_metrics
regularization_losses
,metrics
 
?
-
state_size

 kernel
!recurrent_kernel
"bias
.	variables
/trainable_variables
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
?
2layer_regularization_losses
	variables
3non_trainable_variables
trainable_variables

4layers
5layer_metrics

6states
regularization_losses
7metrics
?
8
state_size

#kernel
$recurrent_kernel
%bias
9	variables
:trainable_variables
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
?
=layer_regularization_losses
	variables
>non_trainable_variables
trainable_variables

?layers
@layer_metrics

Astates
regularization_losses
Bmetrics
h

&kernel
'bias
C	variables
Dtrainable_variables
Eregularization_losses
F	keras_api

&0
'1

&0
'1
 
?
Glayer_regularization_losses
Hnon_trainable_variables
	variables
trainable_variables

Ilayers
Jlayer_metrics
regularization_losses
Kmetrics
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
US
VARIABLE_VALUElstm_5/lstm_cell_7/kernel&variables/0/.ATTRIBUTES/VARIABLE_VALUE
_]
VARIABLE_VALUE#lstm_5/lstm_cell_7/recurrent_kernel&variables/1/.ATTRIBUTES/VARIABLE_VALUE
SQ
VARIABLE_VALUElstm_5/lstm_cell_7/bias&variables/2/.ATTRIBUTES/VARIABLE_VALUE
US
VARIABLE_VALUElstm_4/lstm_cell_6/kernel&variables/3/.ATTRIBUTES/VARIABLE_VALUE
_]
VARIABLE_VALUE#lstm_4/lstm_cell_6/recurrent_kernel&variables/4/.ATTRIBUTES/VARIABLE_VALUE
SQ
VARIABLE_VALUElstm_4/lstm_cell_6/bias&variables/5/.ATTRIBUTES/VARIABLE_VALUE
US
VARIABLE_VALUEtime_distributed_2/kernel&variables/6/.ATTRIBUTES/VARIABLE_VALUE
SQ
VARIABLE_VALUEtime_distributed_2/bias&variables/7/.ATTRIBUTES/VARIABLE_VALUE
 
 

0
1
2
 

L0
M1
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
?
Nlayer_regularization_losses
Onon_trainable_variables
.	variables
/trainable_variables

Players
Qlayer_metrics
0regularization_losses
Rmetrics
 
 


0
 

S0
T1
 
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
?
Ulayer_regularization_losses
Vnon_trainable_variables
9	variables
:trainable_variables

Wlayers
Xlayer_metrics
;regularization_losses
Ymetrics
 
 

0
 

Z0
[1
 

&0
'1

&0
'1
 
?
\layer_regularization_losses
]non_trainable_variables
C	variables
Dtrainable_variables

^layers
_layer_metrics
Eregularization_losses
`metrics
 
 

0
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
ge
VARIABLE_VALUElstm_5/VariableBlayer_with_weights-0/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUE
ig
VARIABLE_VALUElstm_5/Variable_1Blayer_with_weights-0/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUE
 
 
 
 
 
ge
VARIABLE_VALUElstm_4/VariableBlayer_with_weights-1/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUE
ig
VARIABLE_VALUElstm_4/Variable_1Blayer_with_weights-1/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUE
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
xv
VARIABLE_VALUE Adam/lstm_5/lstm_cell_7/kernel/mBvariables/0/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
??
VARIABLE_VALUE*Adam/lstm_5/lstm_cell_7/recurrent_kernel/mBvariables/1/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
vt
VARIABLE_VALUEAdam/lstm_5/lstm_cell_7/bias/mBvariables/2/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
xv
VARIABLE_VALUE Adam/lstm_4/lstm_cell_6/kernel/mBvariables/3/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
??
VARIABLE_VALUE*Adam/lstm_4/lstm_cell_6/recurrent_kernel/mBvariables/4/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
vt
VARIABLE_VALUEAdam/lstm_4/lstm_cell_6/bias/mBvariables/5/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
xv
VARIABLE_VALUE Adam/time_distributed_2/kernel/mBvariables/6/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
vt
VARIABLE_VALUEAdam/time_distributed_2/bias/mBvariables/7/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE
xv
VARIABLE_VALUE Adam/lstm_5/lstm_cell_7/kernel/vBvariables/0/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
??
VARIABLE_VALUE*Adam/lstm_5/lstm_cell_7/recurrent_kernel/vBvariables/1/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
vt
VARIABLE_VALUEAdam/lstm_5/lstm_cell_7/bias/vBvariables/2/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
xv
VARIABLE_VALUE Adam/lstm_4/lstm_cell_6/kernel/vBvariables/3/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
??
VARIABLE_VALUE*Adam/lstm_4/lstm_cell_6/recurrent_kernel/vBvariables/4/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
vt
VARIABLE_VALUEAdam/lstm_4/lstm_cell_6/bias/vBvariables/5/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
xv
VARIABLE_VALUE Adam/time_distributed_2/kernel/vBvariables/6/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
vt
VARIABLE_VALUEAdam/time_distributed_2/bias/vBvariables/7/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE
u
serving_default_lstm_5_inputPlaceholder*"
_output_shapes
:2
*
dtype0*
shape:2

?
StatefulPartitionedCallStatefulPartitionedCallserving_default_lstm_5_inputlstm_5/lstm_cell_7/kernellstm_5/Variable#lstm_5/lstm_cell_7/recurrent_kernellstm_5/lstm_cell_7/biaslstm_5/Variable_1lstm_4/lstm_cell_6/kernellstm_4/Variable#lstm_4/lstm_cell_6/recurrent_kernellstm_4/lstm_cell_6/biaslstm_4/Variable_1time_distributed_2/kerneltime_distributed_2/bias*
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
GPU 2J 8? *,
f'R%
#__inference_signature_wrapper_39155
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
?
StatefulPartitionedCall_1StatefulPartitionedCallsaver_filenameAdam/iter/Read/ReadVariableOpAdam/beta_1/Read/ReadVariableOpAdam/beta_2/Read/ReadVariableOpAdam/decay/Read/ReadVariableOp&Adam/learning_rate/Read/ReadVariableOp-lstm_5/lstm_cell_7/kernel/Read/ReadVariableOp7lstm_5/lstm_cell_7/recurrent_kernel/Read/ReadVariableOp+lstm_5/lstm_cell_7/bias/Read/ReadVariableOp-lstm_4/lstm_cell_6/kernel/Read/ReadVariableOp7lstm_4/lstm_cell_6/recurrent_kernel/Read/ReadVariableOp+lstm_4/lstm_cell_6/bias/Read/ReadVariableOp-time_distributed_2/kernel/Read/ReadVariableOp+time_distributed_2/bias/Read/ReadVariableOp#lstm_5/Variable/Read/ReadVariableOp%lstm_5/Variable_1/Read/ReadVariableOp#lstm_4/Variable/Read/ReadVariableOp%lstm_4/Variable_1/Read/ReadVariableOptotal/Read/ReadVariableOpcount/Read/ReadVariableOptotal_1/Read/ReadVariableOpcount_1/Read/ReadVariableOp4Adam/lstm_5/lstm_cell_7/kernel/m/Read/ReadVariableOp>Adam/lstm_5/lstm_cell_7/recurrent_kernel/m/Read/ReadVariableOp2Adam/lstm_5/lstm_cell_7/bias/m/Read/ReadVariableOp4Adam/lstm_4/lstm_cell_6/kernel/m/Read/ReadVariableOp>Adam/lstm_4/lstm_cell_6/recurrent_kernel/m/Read/ReadVariableOp2Adam/lstm_4/lstm_cell_6/bias/m/Read/ReadVariableOp4Adam/time_distributed_2/kernel/m/Read/ReadVariableOp2Adam/time_distributed_2/bias/m/Read/ReadVariableOp4Adam/lstm_5/lstm_cell_7/kernel/v/Read/ReadVariableOp>Adam/lstm_5/lstm_cell_7/recurrent_kernel/v/Read/ReadVariableOp2Adam/lstm_5/lstm_cell_7/bias/v/Read/ReadVariableOp4Adam/lstm_4/lstm_cell_6/kernel/v/Read/ReadVariableOp>Adam/lstm_4/lstm_cell_6/recurrent_kernel/v/Read/ReadVariableOp2Adam/lstm_4/lstm_cell_6/bias/v/Read/ReadVariableOp4Adam/time_distributed_2/kernel/v/Read/ReadVariableOp2Adam/time_distributed_2/bias/v/Read/ReadVariableOpConst*2
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
GPU 2J 8? *'
f"R 
__inference__traced_save_42548
?

StatefulPartitionedCall_2StatefulPartitionedCallsaver_filename	Adam/iterAdam/beta_1Adam/beta_2
Adam/decayAdam/learning_ratelstm_5/lstm_cell_7/kernel#lstm_5/lstm_cell_7/recurrent_kernellstm_5/lstm_cell_7/biaslstm_4/lstm_cell_6/kernel#lstm_4/lstm_cell_6/recurrent_kernellstm_4/lstm_cell_6/biastime_distributed_2/kerneltime_distributed_2/biaslstm_5/Variablelstm_5/Variable_1lstm_4/Variablelstm_4/Variable_1totalcounttotal_1count_1 Adam/lstm_5/lstm_cell_7/kernel/m*Adam/lstm_5/lstm_cell_7/recurrent_kernel/mAdam/lstm_5/lstm_cell_7/bias/m Adam/lstm_4/lstm_cell_6/kernel/m*Adam/lstm_4/lstm_cell_6/recurrent_kernel/mAdam/lstm_4/lstm_cell_6/bias/m Adam/time_distributed_2/kernel/mAdam/time_distributed_2/bias/m Adam/lstm_5/lstm_cell_7/kernel/v*Adam/lstm_5/lstm_cell_7/recurrent_kernel/vAdam/lstm_5/lstm_cell_7/bias/v Adam/lstm_4/lstm_cell_6/kernel/v*Adam/lstm_4/lstm_cell_6/recurrent_kernel/vAdam/lstm_4/lstm_cell_6/bias/v Adam/time_distributed_2/kernel/vAdam/time_distributed_2/bias/v*1
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
GPU 2J 8? **
f%R#
!__inference__traced_restore_42669??0
?
?
&__inference_lstm_4_layer_call_fn_41470

inputs
unknown:	2?
	unknown_0:22
	unknown_1:	2?
	unknown_2:	?
	unknown_3:22
identity??StatefulPartitionedCall?
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
GPU 2J 8? *J
fERC
A__inference_lstm_4_layer_call_and_return_conditional_losses_384232
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
?e
?

lstm_4_while_body_39402*
&lstm_4_while_lstm_4_while_loop_counter0
,lstm_4_while_lstm_4_while_maximum_iterations
lstm_4_while_placeholder
lstm_4_while_placeholder_1
lstm_4_while_placeholder_2
lstm_4_while_placeholder_3'
#lstm_4_while_lstm_4_strided_slice_0e
alstm_4_while_tensorarrayv2read_tensorlistgetitem_lstm_4_tensorarrayunstack_tensorlistfromtensor_0L
9lstm_4_while_lstm_cell_6_matmul_readvariableop_resource_0:	2?N
;lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource_0:	2?I
:lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource_0:	?
lstm_4_while_identity
lstm_4_while_identity_1
lstm_4_while_identity_2
lstm_4_while_identity_3
lstm_4_while_identity_4
lstm_4_while_identity_5%
!lstm_4_while_lstm_4_strided_slicec
_lstm_4_while_tensorarrayv2read_tensorlistgetitem_lstm_4_tensorarrayunstack_tensorlistfromtensorJ
7lstm_4_while_lstm_cell_6_matmul_readvariableop_resource:	2?L
9lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource:	2?G
8lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource:	???/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp?.lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp?0lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp?
>lstm_4/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2@
>lstm_4/while/TensorArrayV2Read/TensorListGetItem/element_shape?
0lstm_4/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemalstm_4_while_tensorarrayv2read_tensorlistgetitem_lstm_4_tensorarrayunstack_tensorlistfromtensor_0lstm_4_while_placeholderGlstm_4/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype022
0lstm_4/while/TensorArrayV2Read/TensorListGetItem?
.lstm_4/while/lstm_cell_6/MatMul/ReadVariableOpReadVariableOp9lstm_4_while_lstm_cell_6_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype020
.lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp?
lstm_4/while/lstm_cell_6/MatMulMatMul7lstm_4/while/TensorArrayV2Read/TensorListGetItem:item:06lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2!
lstm_4/while/lstm_cell_6/MatMul?
0lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp;lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype022
0lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp?
!lstm_4/while/lstm_cell_6/MatMul_1MatMullstm_4_while_placeholder_28lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2#
!lstm_4/while/lstm_cell_6/MatMul_1?
lstm_4/while/lstm_cell_6/addAddV2)lstm_4/while/lstm_cell_6/MatMul:product:0+lstm_4/while/lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_4/while/lstm_cell_6/add?
/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp:lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype021
/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp?
 lstm_4/while/lstm_cell_6/BiasAddBiasAdd lstm_4/while/lstm_cell_6/add:z:07lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2"
 lstm_4/while/lstm_cell_6/BiasAdd?
(lstm_4/while/lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2*
(lstm_4/while/lstm_cell_6/split/split_dim?
lstm_4/while/lstm_cell_6/splitSplit1lstm_4/while/lstm_cell_6/split/split_dim:output:0)lstm_4/while/lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2 
lstm_4/while/lstm_cell_6/split?
lstm_4/while/lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2 
lstm_4/while/lstm_cell_6/Const?
 lstm_4/while/lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_4/while/lstm_cell_6/Const_1?
lstm_4/while/lstm_cell_6/MulMul'lstm_4/while/lstm_cell_6/split:output:0'lstm_4/while/lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
lstm_4/while/lstm_cell_6/Mul?
lstm_4/while/lstm_cell_6/Add_1AddV2 lstm_4/while/lstm_cell_6/Mul:z:0)lstm_4/while/lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/Add_1?
0lstm_4/while/lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??22
0lstm_4/while/lstm_cell_6/clip_by_value/Minimum/y?
.lstm_4/while/lstm_cell_6/clip_by_value/MinimumMinimum"lstm_4/while/lstm_cell_6/Add_1:z:09lstm_4/while/lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:2220
.lstm_4/while/lstm_cell_6/clip_by_value/Minimum?
(lstm_4/while/lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2*
(lstm_4/while/lstm_cell_6/clip_by_value/y?
&lstm_4/while/lstm_cell_6/clip_by_valueMaximum2lstm_4/while/lstm_cell_6/clip_by_value/Minimum:z:01lstm_4/while/lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222(
&lstm_4/while/lstm_cell_6/clip_by_value?
 lstm_4/while/lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_4/while/lstm_cell_6/Const_2?
 lstm_4/while/lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_4/while/lstm_cell_6/Const_3?
lstm_4/while/lstm_cell_6/Mul_1Mul'lstm_4/while/lstm_cell_6/split:output:1)lstm_4/while/lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/Mul_1?
lstm_4/while/lstm_cell_6/Add_2AddV2"lstm_4/while/lstm_cell_6/Mul_1:z:0)lstm_4/while/lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/Add_2?
2lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum/y?
0lstm_4/while/lstm_cell_6/clip_by_value_1/MinimumMinimum"lstm_4/while/lstm_cell_6/Add_2:z:0;lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:2222
0lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum?
*lstm_4/while/lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_4/while/lstm_cell_6/clip_by_value_1/y?
(lstm_4/while/lstm_cell_6/clip_by_value_1Maximum4lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum:z:03lstm_4/while/lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222*
(lstm_4/while/lstm_cell_6/clip_by_value_1?
lstm_4/while/lstm_cell_6/mul_2Mul,lstm_4/while/lstm_cell_6/clip_by_value_1:z:0lstm_4_while_placeholder_3*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/mul_2?
lstm_4/while/lstm_cell_6/TanhTanh'lstm_4/while/lstm_cell_6/split:output:2*
T0*
_output_shapes

:222
lstm_4/while/lstm_cell_6/Tanh?
lstm_4/while/lstm_cell_6/mul_3Mul*lstm_4/while/lstm_cell_6/clip_by_value:z:0!lstm_4/while/lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/mul_3?
lstm_4/while/lstm_cell_6/add_3AddV2"lstm_4/while/lstm_cell_6/mul_2:z:0"lstm_4/while/lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/add_3?
 lstm_4/while/lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_4/while/lstm_cell_6/Const_4?
 lstm_4/while/lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_4/while/lstm_cell_6/Const_5?
lstm_4/while/lstm_cell_6/Mul_4Mul'lstm_4/while/lstm_cell_6/split:output:3)lstm_4/while/lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/Mul_4?
lstm_4/while/lstm_cell_6/Add_4AddV2"lstm_4/while/lstm_cell_6/Mul_4:z:0)lstm_4/while/lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/Add_4?
2lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum/y?
0lstm_4/while/lstm_cell_6/clip_by_value_2/MinimumMinimum"lstm_4/while/lstm_cell_6/Add_4:z:0;lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:2222
0lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum?
*lstm_4/while/lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_4/while/lstm_cell_6/clip_by_value_2/y?
(lstm_4/while/lstm_cell_6/clip_by_value_2Maximum4lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum:z:03lstm_4/while/lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222*
(lstm_4/while/lstm_cell_6/clip_by_value_2?
lstm_4/while/lstm_cell_6/Tanh_1Tanh"lstm_4/while/lstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222!
lstm_4/while/lstm_cell_6/Tanh_1?
lstm_4/while/lstm_cell_6/mul_5Mul,lstm_4/while/lstm_cell_6/clip_by_value_2:z:0#lstm_4/while/lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/mul_5?
1lstm_4/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_4_while_placeholder_1lstm_4_while_placeholder"lstm_4/while/lstm_cell_6/mul_5:z:0*
_output_shapes
: *
element_dtype023
1lstm_4/while/TensorArrayV2Write/TensorListSetItemj
lstm_4/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_4/while/add/y?
lstm_4/while/addAddV2lstm_4_while_placeholderlstm_4/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_4/while/addn
lstm_4/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_4/while/add_1/y?
lstm_4/while/add_1AddV2&lstm_4_while_lstm_4_while_loop_counterlstm_4/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_4/while/add_1?
lstm_4/while/IdentityIdentitylstm_4/while/add_1:z:0^lstm_4/while/NoOp*
T0*
_output_shapes
: 2
lstm_4/while/Identity?
lstm_4/while/Identity_1Identity,lstm_4_while_lstm_4_while_maximum_iterations^lstm_4/while/NoOp*
T0*
_output_shapes
: 2
lstm_4/while/Identity_1?
lstm_4/while/Identity_2Identitylstm_4/while/add:z:0^lstm_4/while/NoOp*
T0*
_output_shapes
: 2
lstm_4/while/Identity_2?
lstm_4/while/Identity_3IdentityAlstm_4/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_4/while/NoOp*
T0*
_output_shapes
: 2
lstm_4/while/Identity_3?
lstm_4/while/Identity_4Identity"lstm_4/while/lstm_cell_6/mul_5:z:0^lstm_4/while/NoOp*
T0*
_output_shapes

:222
lstm_4/while/Identity_4?
lstm_4/while/Identity_5Identity"lstm_4/while/lstm_cell_6/add_3:z:0^lstm_4/while/NoOp*
T0*
_output_shapes

:222
lstm_4/while/Identity_5?
lstm_4/while/NoOpNoOp0^lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp/^lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp1^lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_4/while/NoOp"7
lstm_4_while_identitylstm_4/while/Identity:output:0";
lstm_4_while_identity_1 lstm_4/while/Identity_1:output:0";
lstm_4_while_identity_2 lstm_4/while/Identity_2:output:0";
lstm_4_while_identity_3 lstm_4/while/Identity_3:output:0";
lstm_4_while_identity_4 lstm_4/while/Identity_4:output:0";
lstm_4_while_identity_5 lstm_4/while/Identity_5:output:0"H
!lstm_4_while_lstm_4_strided_slice#lstm_4_while_lstm_4_strided_slice_0"v
8lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource:lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource_0"x
9lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource;lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource_0"t
7lstm_4_while_lstm_cell_6_matmul_readvariableop_resource9lstm_4_while_lstm_cell_6_matmul_readvariableop_resource_0"?
_lstm_4_while_tensorarrayv2read_tensorlistgetitem_lstm_4_tensorarrayunstack_tensorlistfromtensoralstm_4_while_tensorarrayv2read_tensorlistgetitem_lstm_4_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2b
/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp2`
.lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp.lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp2d
0lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp0lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp: 
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
?
?
&__inference_lstm_5_layer_call_fn_40698

inputs
unknown:	?
	unknown_0:22
	unknown_1:	2?
	unknown_2:	?
	unknown_3:22
identity??StatefulPartitionedCall?
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
GPU 2J 8? *J
fERC
A__inference_lstm_5_layer_call_and_return_conditional_losses_382342
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
?
?
G__inference_sequential_2_layer_call_and_return_conditional_losses_38457

inputs
lstm_5_38235:	?
lstm_5_38237:22
lstm_5_38239:	2?
lstm_5_38241:	?
lstm_5_38243:22
lstm_4_38424:	2?
lstm_4_38426:22
lstm_4_38428:	2?
lstm_4_38430:	?
lstm_4_38432:22*
time_distributed_2_38449:2&
time_distributed_2_38451:
identity??lstm_4/StatefulPartitionedCall?lstm_5/StatefulPartitionedCall?*time_distributed_2/StatefulPartitionedCall?
lstm_5/StatefulPartitionedCallStatefulPartitionedCallinputslstm_5_38235lstm_5_38237lstm_5_38239lstm_5_38241lstm_5_38243*
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
GPU 2J 8? *J
fERC
A__inference_lstm_5_layer_call_and_return_conditional_losses_382342 
lstm_5/StatefulPartitionedCall?
lstm_4/StatefulPartitionedCallStatefulPartitionedCall'lstm_5/StatefulPartitionedCall:output:0lstm_4_38424lstm_4_38426lstm_4_38428lstm_4_38430lstm_4_38432*
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
GPU 2J 8? *J
fERC
A__inference_lstm_4_layer_call_and_return_conditional_losses_384232 
lstm_4/StatefulPartitionedCall?
*time_distributed_2/StatefulPartitionedCallStatefulPartitionedCall'lstm_4/StatefulPartitionedCall:output:0time_distributed_2_38449time_distributed_2_38451*
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
GPU 2J 8? *V
fQRO
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_384482,
*time_distributed_2/StatefulPartitionedCall?
 time_distributed_2/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2"
 time_distributed_2/Reshape/shape?
time_distributed_2/ReshapeReshape'lstm_4/StatefulPartitionedCall:output:0)time_distributed_2/Reshape/shape:output:0*
T0*
_output_shapes
:	?22
time_distributed_2/Reshape?
IdentityIdentity3time_distributed_2/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity?
NoOpNoOp^lstm_4/StatefulPartitionedCall^lstm_5/StatefulPartitionedCall+^time_distributed_2/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 2@
lstm_4/StatefulPartitionedCalllstm_4/StatefulPartitionedCall2@
lstm_5/StatefulPartitionedCalllstm_5/StatefulPartitionedCall2X
*time_distributed_2/StatefulPartitionedCall*time_distributed_2/StatefulPartitionedCall:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
?X
?
while_body_40014
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_7_matmul_readvariableop_resource_0:	?G
4while_lstm_cell_7_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_7_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_7_matmul_readvariableop_resource:	?E
2while_lstm_cell_7_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_7_biasadd_readvariableop_resource:	???(while/lstm_cell_7/BiasAdd/ReadVariableOp?'while/lstm_cell_7/MatMul/ReadVariableOp?)while/lstm_cell_7/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_7/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_7_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02)
'while/lstm_cell_7/MatMul/ReadVariableOp?
while/lstm_cell_7/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/MatMul?
)while/lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_7_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_7/MatMul_1/ReadVariableOp?
while/lstm_cell_7/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_7/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/MatMul_1?
while/lstm_cell_7/addAddV2"while/lstm_cell_7/MatMul:product:0$while/lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/add?
(while/lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_7_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_7/BiasAdd/ReadVariableOp?
while/lstm_cell_7/BiasAddBiasAddwhile/lstm_cell_7/add:z:00while/lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/BiasAdd?
!while/lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_7/split/split_dim?
while/lstm_cell_7/splitSplit*while/lstm_cell_7/split/split_dim:output:0"while/lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_7/splitw
while/lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const{
while/lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_1?
while/lstm_cell_7/MulMul while/lstm_cell_7/split:output:0 while/lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul?
while/lstm_cell_7/Add_1AddV2while/lstm_cell_7/Mul:z:0"while/lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_1?
)while/lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_7/clip_by_value/Minimum/y?
'while/lstm_cell_7/clip_by_value/MinimumMinimumwhile/lstm_cell_7/Add_1:z:02while/lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222)
'while/lstm_cell_7/clip_by_value/Minimum?
!while/lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_7/clip_by_value/y?
while/lstm_cell_7/clip_by_valueMaximum+while/lstm_cell_7/clip_by_value/Minimum:z:0*while/lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222!
while/lstm_cell_7/clip_by_value{
while/lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const_2{
while/lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_3?
while/lstm_cell_7/Mul_1Mul while/lstm_cell_7/split:output:1"while/lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul_1?
while/lstm_cell_7/Add_2AddV2while/lstm_cell_7/Mul_1:z:0"while/lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_2?
+while/lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_7/clip_by_value_1/Minimum/y?
)while/lstm_cell_7/clip_by_value_1/MinimumMinimumwhile/lstm_cell_7/Add_2:z:04while/lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_7/clip_by_value_1/Minimum?
#while/lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_7/clip_by_value_1/y?
!while/lstm_cell_7/clip_by_value_1Maximum-while/lstm_cell_7/clip_by_value_1/Minimum:z:0,while/lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_7/clip_by_value_1?
while/lstm_cell_7/mul_2Mul%while/lstm_cell_7/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_2?
while/lstm_cell_7/TanhTanh while/lstm_cell_7/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_7/Tanh?
while/lstm_cell_7/mul_3Mul#while/lstm_cell_7/clip_by_value:z:0while/lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_3?
while/lstm_cell_7/add_3AddV2while/lstm_cell_7/mul_2:z:0while/lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_7/add_3{
while/lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const_4{
while/lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_5?
while/lstm_cell_7/Mul_4Mul while/lstm_cell_7/split:output:3"while/lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul_4?
while/lstm_cell_7/Add_4AddV2while/lstm_cell_7/Mul_4:z:0"while/lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_4?
+while/lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_7/clip_by_value_2/Minimum/y?
)while/lstm_cell_7/clip_by_value_2/MinimumMinimumwhile/lstm_cell_7/Add_4:z:04while/lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_7/clip_by_value_2/Minimum?
#while/lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_7/clip_by_value_2/y?
!while/lstm_cell_7/clip_by_value_2Maximum-while/lstm_cell_7/clip_by_value_2/Minimum:z:0,while/lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_7/clip_by_value_2?
while/lstm_cell_7/Tanh_1Tanhwhile/lstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Tanh_1?
while/lstm_cell_7/mul_5Mul%while/lstm_cell_7/clip_by_value_2:z:0while/lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_7/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_7/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_7/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_7/BiasAdd/ReadVariableOp(^while/lstm_cell_7/MatMul/ReadVariableOp*^while/lstm_cell_7/MatMul_1/ReadVariableOp*"
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
1while_lstm_cell_7_biasadd_readvariableop_resource3while_lstm_cell_7_biasadd_readvariableop_resource_0"j
2while_lstm_cell_7_matmul_1_readvariableop_resource4while_lstm_cell_7_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_7_matmul_readvariableop_resource2while_lstm_cell_7_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2T
(while/lstm_cell_7/BiasAdd/ReadVariableOp(while/lstm_cell_7/BiasAdd/ReadVariableOp2R
'while/lstm_cell_7/MatMul/ReadVariableOp'while/lstm_cell_7/MatMul/ReadVariableOp2V
)while/lstm_cell_7/MatMul_1/ReadVariableOp)while/lstm_cell_7/MatMul_1/ReadVariableOp: 
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
?
?
,__inference_sequential_2_layer_call_fn_38484
lstm_5_input
unknown:	?
	unknown_0:22
	unknown_1:	2?
	unknown_2:	?
	unknown_3:22
	unknown_4:	2?
	unknown_5:22
	unknown_6:	2?
	unknown_7:	?
	unknown_8:22
	unknown_9:2

unknown_10:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCalllstm_5_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
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
GPU 2J 8? *P
fKRI
G__inference_sequential_2_layer_call_and_return_conditional_losses_384572
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
StatefulPartitionedCallStatefulPartitionedCall:P L
"
_output_shapes
:2

&
_user_specified_namelstm_5_input
?
?
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_41541

inputs8
&dense_2_matmul_readvariableop_resource:25
'dense_2_biasadd_readvariableop_resource:
identity??dense_2/BiasAdd/ReadVariableOp?dense_2/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2
Reshape/shapeg
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes
:	?22	
Reshape?
dense_2/MatMul/ReadVariableOpReadVariableOp&dense_2_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_2/MatMul/ReadVariableOp?
dense_2/MatMulMatMulReshape:output:0%dense_2/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
dense_2/MatMul?
dense_2/BiasAdd/ReadVariableOpReadVariableOp'dense_2_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_2/BiasAdd/ReadVariableOp?
dense_2/BiasAddBiasAdddense_2/MatMul:product:0&dense_2/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
dense_2/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      2
Reshape_1/shape?
	Reshape_1Reshapedense_2/BiasAdd:output:0Reshape_1/shape:output:0*
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

Identity?
NoOpNoOp^dense_2/BiasAdd/ReadVariableOp^dense_2/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:2
2: : 2@
dense_2/BiasAdd/ReadVariableOpdense_2/BiasAdd/ReadVariableOp2>
dense_2/MatMul/ReadVariableOpdense_2/MatMul/ReadVariableOp:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
?
?
,__inference_sequential_2_layer_call_fn_39052
lstm_5_input
unknown:	?
	unknown_0:22
	unknown_1:	2?
	unknown_2:	?
	unknown_3:22
	unknown_4:	2?
	unknown_5:22
	unknown_6:	2?
	unknown_7:	?
	unknown_8:22
	unknown_9:2

unknown_10:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCalllstm_5_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
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
GPU 2J 8? *P
fKRI
G__inference_sequential_2_layer_call_and_return_conditional_losses_389962
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
StatefulPartitionedCallStatefulPartitionedCall:P L
"
_output_shapes
:2

&
_user_specified_namelstm_5_input
?m
?
A__inference_lstm_5_layer_call_and_return_conditional_losses_38234

inputs=
*lstm_cell_7_matmul_readvariableop_resource:	?>
,lstm_cell_7_matmul_1_readvariableop_resource:22A
.lstm_cell_7_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_7_biasadd_readvariableop_resource:	?;
)lstm_cell_7_mul_2_readvariableop_resource:22
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_7/BiasAdd/ReadVariableOp?!lstm_cell_7/MatMul/ReadVariableOp?#lstm_cell_7/MatMul_1/ReadVariableOp?%lstm_cell_7/MatMul_1/ReadVariableOp_1? lstm_cell_7/mul_2/ReadVariableOp?whileu
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_7/MatMul/ReadVariableOpReadVariableOp*lstm_cell_7_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02#
!lstm_cell_7/MatMul/ReadVariableOp?
lstm_cell_7/MatMulMatMulstrided_slice_1:output:0)lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/MatMul?
#lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#lstm_cell_7/MatMul_1/ReadVariableOp?
%lstm_cell_7/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_7_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_7/MatMul_1/ReadVariableOp_1?
lstm_cell_7/MatMul_1MatMul+lstm_cell_7/MatMul_1/ReadVariableOp:value:0-lstm_cell_7/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/MatMul_1?
lstm_cell_7/addAddV2lstm_cell_7/MatMul:product:0lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/add?
"lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_7_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_7/BiasAdd/ReadVariableOp?
lstm_cell_7/BiasAddBiasAddlstm_cell_7/add:z:0*lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/BiasAdd|
lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_7/split/split_dim?
lstm_cell_7/splitSplit$lstm_cell_7/split/split_dim:output:0lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_7/splitk
lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Consto
lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_1?
lstm_cell_7/MulMullstm_cell_7/split:output:0lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul?
lstm_cell_7/Add_1AddV2lstm_cell_7/Mul:z:0lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_1?
#lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_7/clip_by_value/Minimum/y?
!lstm_cell_7/clip_by_value/MinimumMinimumlstm_cell_7/Add_1:z:0,lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222#
!lstm_cell_7/clip_by_value/Minimum
lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value/y?
lstm_cell_7/clip_by_valueMaximum%lstm_cell_7/clip_by_value/Minimum:z:0$lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_valueo
lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Const_2o
lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_3?
lstm_cell_7/Mul_1Mullstm_cell_7/split:output:1lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul_1?
lstm_cell_7/Add_2AddV2lstm_cell_7/Mul_1:z:0lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_2?
%lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_7/clip_by_value_1/Minimum/y?
#lstm_cell_7/clip_by_value_1/MinimumMinimumlstm_cell_7/Add_2:z:0.lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_7/clip_by_value_1/Minimum?
lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value_1/y?
lstm_cell_7/clip_by_value_1Maximum'lstm_cell_7/clip_by_value_1/Minimum:z:0&lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_value_1?
 lstm_cell_7/mul_2/ReadVariableOpReadVariableOp)lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02"
 lstm_cell_7/mul_2/ReadVariableOp?
lstm_cell_7/mul_2Mullstm_cell_7/clip_by_value_1:z:0(lstm_cell_7/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_2q
lstm_cell_7/TanhTanhlstm_cell_7/split:output:2*
T0*
_output_shapes

:222
lstm_cell_7/Tanh?
lstm_cell_7/mul_3Mullstm_cell_7/clip_by_value:z:0lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_3?
lstm_cell_7/add_3AddV2lstm_cell_7/mul_2:z:0lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_7/add_3o
lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Const_4o
lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_5?
lstm_cell_7/Mul_4Mullstm_cell_7/split:output:3lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul_4?
lstm_cell_7/Add_4AddV2lstm_cell_7/Mul_4:z:0lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_4?
%lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_7/clip_by_value_2/Minimum/y?
#lstm_cell_7/clip_by_value_2/MinimumMinimumlstm_cell_7/Add_4:z:0.lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_7/clip_by_value_2/Minimum?
lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value_2/y?
lstm_cell_7/clip_by_value_2Maximum'lstm_cell_7/clip_by_value_2/Minimum:z:0&lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_value_2p
lstm_cell_7/Tanh_1Tanhlstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_7/Tanh_1?
lstm_cell_7/mul_5Mullstm_cell_7/clip_by_value_2:z:0lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp,lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_7_matmul_readvariableop_resource.lstm_cell_7_matmul_1_readvariableop_1_resource+lstm_cell_7_biasadd_readvariableop_resource*
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
while_body_38129*
condR
while_cond_38128*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_7_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_7/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_7_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_7/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_7/BiasAdd/ReadVariableOp"^lstm_cell_7/MatMul/ReadVariableOp$^lstm_cell_7/MatMul_1/ReadVariableOp&^lstm_cell_7/MatMul_1/ReadVariableOp_1!^lstm_cell_7/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_7/BiasAdd/ReadVariableOp"lstm_cell_7/BiasAdd/ReadVariableOp2F
!lstm_cell_7/MatMul/ReadVariableOp!lstm_cell_7/MatMul/ReadVariableOp2J
#lstm_cell_7/MatMul_1/ReadVariableOp#lstm_cell_7/MatMul_1/ReadVariableOp2N
%lstm_cell_7/MatMul_1/ReadVariableOp_1%lstm_cell_7/MatMul_1/ReadVariableOp_12D
 lstm_cell_7/mul_2/ReadVariableOp lstm_cell_7/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
?m
?
A__inference_lstm_4_layer_call_and_return_conditional_losses_41247

inputs=
*lstm_cell_6_matmul_readvariableop_resource:	2?>
,lstm_cell_6_matmul_1_readvariableop_resource:22A
.lstm_cell_6_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_6_biasadd_readvariableop_resource:	?;
)lstm_cell_6_mul_2_readvariableop_resource:22
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_6/BiasAdd/ReadVariableOp?!lstm_cell_6/MatMul/ReadVariableOp?#lstm_cell_6/MatMul_1/ReadVariableOp?%lstm_cell_6/MatMul_1/ReadVariableOp_1? lstm_cell_6/mul_2/ReadVariableOp?whileu
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_6/MatMul/ReadVariableOpReadVariableOp*lstm_cell_6_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02#
!lstm_cell_6/MatMul/ReadVariableOp?
lstm_cell_6/MatMulMatMulstrided_slice_1:output:0)lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/MatMul?
#lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#lstm_cell_6/MatMul_1/ReadVariableOp?
%lstm_cell_6/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_6_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_6/MatMul_1/ReadVariableOp_1?
lstm_cell_6/MatMul_1MatMul+lstm_cell_6/MatMul_1/ReadVariableOp:value:0-lstm_cell_6/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/MatMul_1?
lstm_cell_6/addAddV2lstm_cell_6/MatMul:product:0lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/add?
"lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_6_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_6/BiasAdd/ReadVariableOp?
lstm_cell_6/BiasAddBiasAddlstm_cell_6/add:z:0*lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/BiasAdd|
lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_6/split/split_dim?
lstm_cell_6/splitSplit$lstm_cell_6/split/split_dim:output:0lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_6/splitk
lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Consto
lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_1?
lstm_cell_6/MulMullstm_cell_6/split:output:0lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul?
lstm_cell_6/Add_1AddV2lstm_cell_6/Mul:z:0lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_1?
#lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_6/clip_by_value/Minimum/y?
!lstm_cell_6/clip_by_value/MinimumMinimumlstm_cell_6/Add_1:z:0,lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222#
!lstm_cell_6/clip_by_value/Minimum
lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value/y?
lstm_cell_6/clip_by_valueMaximum%lstm_cell_6/clip_by_value/Minimum:z:0$lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_valueo
lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Const_2o
lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_3?
lstm_cell_6/Mul_1Mullstm_cell_6/split:output:1lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul_1?
lstm_cell_6/Add_2AddV2lstm_cell_6/Mul_1:z:0lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_2?
%lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_6/clip_by_value_1/Minimum/y?
#lstm_cell_6/clip_by_value_1/MinimumMinimumlstm_cell_6/Add_2:z:0.lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_6/clip_by_value_1/Minimum?
lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value_1/y?
lstm_cell_6/clip_by_value_1Maximum'lstm_cell_6/clip_by_value_1/Minimum:z:0&lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_value_1?
 lstm_cell_6/mul_2/ReadVariableOpReadVariableOp)lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02"
 lstm_cell_6/mul_2/ReadVariableOp?
lstm_cell_6/mul_2Mullstm_cell_6/clip_by_value_1:z:0(lstm_cell_6/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_2q
lstm_cell_6/TanhTanhlstm_cell_6/split:output:2*
T0*
_output_shapes

:222
lstm_cell_6/Tanh?
lstm_cell_6/mul_3Mullstm_cell_6/clip_by_value:z:0lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_3?
lstm_cell_6/add_3AddV2lstm_cell_6/mul_2:z:0lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_6/add_3o
lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Const_4o
lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_5?
lstm_cell_6/Mul_4Mullstm_cell_6/split:output:3lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul_4?
lstm_cell_6/Add_4AddV2lstm_cell_6/Mul_4:z:0lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_4?
%lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_6/clip_by_value_2/Minimum/y?
#lstm_cell_6/clip_by_value_2/MinimumMinimumlstm_cell_6/Add_4:z:0.lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_6/clip_by_value_2/Minimum?
lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value_2/y?
lstm_cell_6/clip_by_value_2Maximum'lstm_cell_6/clip_by_value_2/Minimum:z:0&lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_value_2p
lstm_cell_6/Tanh_1Tanhlstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_6/Tanh_1?
lstm_cell_6/mul_5Mullstm_cell_6/clip_by_value_2:z:0lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp,lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_6_matmul_readvariableop_resource.lstm_cell_6_matmul_1_readvariableop_1_resource+lstm_cell_6_biasadd_readvariableop_resource*
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
while_body_41142*
condR
while_cond_41141*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_6_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_6/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_6_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_6/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_6/BiasAdd/ReadVariableOp"^lstm_cell_6/MatMul/ReadVariableOp$^lstm_cell_6/MatMul_1/ReadVariableOp&^lstm_cell_6/MatMul_1/ReadVariableOp_1!^lstm_cell_6/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_6/BiasAdd/ReadVariableOp"lstm_cell_6/BiasAdd/ReadVariableOp2F
!lstm_cell_6/MatMul/ReadVariableOp!lstm_cell_6/MatMul/ReadVariableOp2J
#lstm_cell_6/MatMul_1/ReadVariableOp#lstm_cell_6/MatMul_1/ReadVariableOp2N
%lstm_cell_6/MatMul_1/ReadVariableOp_1%lstm_cell_6/MatMul_1/ReadVariableOp_12D
 lstm_cell_6/mul_2/ReadVariableOp lstm_cell_6/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
?X
?
while_body_40786
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_6_matmul_readvariableop_resource_0:	2?G
4while_lstm_cell_6_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_6_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_6_matmul_readvariableop_resource:	2?E
2while_lstm_cell_6_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_6_biasadd_readvariableop_resource:	???(while/lstm_cell_6/BiasAdd/ReadVariableOp?'while/lstm_cell_6/MatMul/ReadVariableOp?)while/lstm_cell_6/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_6/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_6_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02)
'while/lstm_cell_6/MatMul/ReadVariableOp?
while/lstm_cell_6/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/MatMul?
)while/lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_6_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_6/MatMul_1/ReadVariableOp?
while/lstm_cell_6/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_6/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/MatMul_1?
while/lstm_cell_6/addAddV2"while/lstm_cell_6/MatMul:product:0$while/lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/add?
(while/lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_6_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_6/BiasAdd/ReadVariableOp?
while/lstm_cell_6/BiasAddBiasAddwhile/lstm_cell_6/add:z:00while/lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/BiasAdd?
!while/lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_6/split/split_dim?
while/lstm_cell_6/splitSplit*while/lstm_cell_6/split/split_dim:output:0"while/lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_6/splitw
while/lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const{
while/lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_1?
while/lstm_cell_6/MulMul while/lstm_cell_6/split:output:0 while/lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul?
while/lstm_cell_6/Add_1AddV2while/lstm_cell_6/Mul:z:0"while/lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_1?
)while/lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_6/clip_by_value/Minimum/y?
'while/lstm_cell_6/clip_by_value/MinimumMinimumwhile/lstm_cell_6/Add_1:z:02while/lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222)
'while/lstm_cell_6/clip_by_value/Minimum?
!while/lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_6/clip_by_value/y?
while/lstm_cell_6/clip_by_valueMaximum+while/lstm_cell_6/clip_by_value/Minimum:z:0*while/lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222!
while/lstm_cell_6/clip_by_value{
while/lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const_2{
while/lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_3?
while/lstm_cell_6/Mul_1Mul while/lstm_cell_6/split:output:1"while/lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul_1?
while/lstm_cell_6/Add_2AddV2while/lstm_cell_6/Mul_1:z:0"while/lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_2?
+while/lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_6/clip_by_value_1/Minimum/y?
)while/lstm_cell_6/clip_by_value_1/MinimumMinimumwhile/lstm_cell_6/Add_2:z:04while/lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_6/clip_by_value_1/Minimum?
#while/lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_6/clip_by_value_1/y?
!while/lstm_cell_6/clip_by_value_1Maximum-while/lstm_cell_6/clip_by_value_1/Minimum:z:0,while/lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_6/clip_by_value_1?
while/lstm_cell_6/mul_2Mul%while/lstm_cell_6/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_2?
while/lstm_cell_6/TanhTanh while/lstm_cell_6/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_6/Tanh?
while/lstm_cell_6/mul_3Mul#while/lstm_cell_6/clip_by_value:z:0while/lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_3?
while/lstm_cell_6/add_3AddV2while/lstm_cell_6/mul_2:z:0while/lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_6/add_3{
while/lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const_4{
while/lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_5?
while/lstm_cell_6/Mul_4Mul while/lstm_cell_6/split:output:3"while/lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul_4?
while/lstm_cell_6/Add_4AddV2while/lstm_cell_6/Mul_4:z:0"while/lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_4?
+while/lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_6/clip_by_value_2/Minimum/y?
)while/lstm_cell_6/clip_by_value_2/MinimumMinimumwhile/lstm_cell_6/Add_4:z:04while/lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_6/clip_by_value_2/Minimum?
#while/lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_6/clip_by_value_2/y?
!while/lstm_cell_6/clip_by_value_2Maximum-while/lstm_cell_6/clip_by_value_2/Minimum:z:0,while/lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_6/clip_by_value_2?
while/lstm_cell_6/Tanh_1Tanhwhile/lstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Tanh_1?
while/lstm_cell_6/mul_5Mul%while/lstm_cell_6/clip_by_value_2:z:0while/lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_6/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_6/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_6/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_6/BiasAdd/ReadVariableOp(^while/lstm_cell_6/MatMul/ReadVariableOp*^while/lstm_cell_6/MatMul_1/ReadVariableOp*"
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
1while_lstm_cell_6_biasadd_readvariableop_resource3while_lstm_cell_6_biasadd_readvariableop_resource_0"j
2while_lstm_cell_6_matmul_1_readvariableop_resource4while_lstm_cell_6_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_6_matmul_readvariableop_resource2while_lstm_cell_6_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2T
(while/lstm_cell_6/BiasAdd/ReadVariableOp(while/lstm_cell_6/BiasAdd/ReadVariableOp2R
'while/lstm_cell_6/MatMul/ReadVariableOp'while/lstm_cell_6/MatMul/ReadVariableOp2V
)while/lstm_cell_6/MatMul_1/ReadVariableOp)while/lstm_cell_6/MatMul_1/ReadVariableOp: 
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
?
?
while_cond_38605
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_38605___redundant_placeholder03
/while_while_cond_38605___redundant_placeholder13
/while_while_cond_38605___redundant_placeholder23
/while_while_cond_38605___redundant_placeholder3
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
?	
?
&__inference_lstm_5_layer_call_fn_40668
inputs_0
unknown:22
	unknown_0:22
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
:2?????????2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_5_layer_call_and_return_conditional_losses_365782
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:2?????????22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2?????????: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:2?????????
"
_user_specified_name
inputs/0
?0
?
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_36436

inputs
states:22
states_1:221
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
:	2?2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:22*
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
:	2?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2	
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
(:22:22:22:22*
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
 *  ??2
clip_by_value/Minimum/y?
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
clip_by_value/y?
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
 *  ??2
clip_by_value_1/Minimum/y?
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
clip_by_value_1/y?
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
 *  ??2
clip_by_value_2/Minimum/y?
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
clip_by_value_2/y?
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
?X
?
while_body_38129
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_7_matmul_readvariableop_resource_0:	?G
4while_lstm_cell_7_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_7_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_7_matmul_readvariableop_resource:	?E
2while_lstm_cell_7_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_7_biasadd_readvariableop_resource:	???(while/lstm_cell_7/BiasAdd/ReadVariableOp?'while/lstm_cell_7/MatMul/ReadVariableOp?)while/lstm_cell_7/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_7/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_7_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02)
'while/lstm_cell_7/MatMul/ReadVariableOp?
while/lstm_cell_7/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/MatMul?
)while/lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_7_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_7/MatMul_1/ReadVariableOp?
while/lstm_cell_7/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_7/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/MatMul_1?
while/lstm_cell_7/addAddV2"while/lstm_cell_7/MatMul:product:0$while/lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/add?
(while/lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_7_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_7/BiasAdd/ReadVariableOp?
while/lstm_cell_7/BiasAddBiasAddwhile/lstm_cell_7/add:z:00while/lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/BiasAdd?
!while/lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_7/split/split_dim?
while/lstm_cell_7/splitSplit*while/lstm_cell_7/split/split_dim:output:0"while/lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_7/splitw
while/lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const{
while/lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_1?
while/lstm_cell_7/MulMul while/lstm_cell_7/split:output:0 while/lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul?
while/lstm_cell_7/Add_1AddV2while/lstm_cell_7/Mul:z:0"while/lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_1?
)while/lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_7/clip_by_value/Minimum/y?
'while/lstm_cell_7/clip_by_value/MinimumMinimumwhile/lstm_cell_7/Add_1:z:02while/lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222)
'while/lstm_cell_7/clip_by_value/Minimum?
!while/lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_7/clip_by_value/y?
while/lstm_cell_7/clip_by_valueMaximum+while/lstm_cell_7/clip_by_value/Minimum:z:0*while/lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222!
while/lstm_cell_7/clip_by_value{
while/lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const_2{
while/lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_3?
while/lstm_cell_7/Mul_1Mul while/lstm_cell_7/split:output:1"while/lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul_1?
while/lstm_cell_7/Add_2AddV2while/lstm_cell_7/Mul_1:z:0"while/lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_2?
+while/lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_7/clip_by_value_1/Minimum/y?
)while/lstm_cell_7/clip_by_value_1/MinimumMinimumwhile/lstm_cell_7/Add_2:z:04while/lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_7/clip_by_value_1/Minimum?
#while/lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_7/clip_by_value_1/y?
!while/lstm_cell_7/clip_by_value_1Maximum-while/lstm_cell_7/clip_by_value_1/Minimum:z:0,while/lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_7/clip_by_value_1?
while/lstm_cell_7/mul_2Mul%while/lstm_cell_7/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_2?
while/lstm_cell_7/TanhTanh while/lstm_cell_7/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_7/Tanh?
while/lstm_cell_7/mul_3Mul#while/lstm_cell_7/clip_by_value:z:0while/lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_3?
while/lstm_cell_7/add_3AddV2while/lstm_cell_7/mul_2:z:0while/lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_7/add_3{
while/lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const_4{
while/lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_5?
while/lstm_cell_7/Mul_4Mul while/lstm_cell_7/split:output:3"while/lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul_4?
while/lstm_cell_7/Add_4AddV2while/lstm_cell_7/Mul_4:z:0"while/lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_4?
+while/lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_7/clip_by_value_2/Minimum/y?
)while/lstm_cell_7/clip_by_value_2/MinimumMinimumwhile/lstm_cell_7/Add_4:z:04while/lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_7/clip_by_value_2/Minimum?
#while/lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_7/clip_by_value_2/y?
!while/lstm_cell_7/clip_by_value_2Maximum-while/lstm_cell_7/clip_by_value_2/Minimum:z:0,while/lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_7/clip_by_value_2?
while/lstm_cell_7/Tanh_1Tanhwhile/lstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Tanh_1?
while/lstm_cell_7/mul_5Mul%while/lstm_cell_7/clip_by_value_2:z:0while/lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_7/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_7/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_7/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_7/BiasAdd/ReadVariableOp(^while/lstm_cell_7/MatMul/ReadVariableOp*^while/lstm_cell_7/MatMul_1/ReadVariableOp*"
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
1while_lstm_cell_7_biasadd_readvariableop_resource3while_lstm_cell_7_biasadd_readvariableop_resource_0"j
2while_lstm_cell_7_matmul_1_readvariableop_resource4while_lstm_cell_7_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_7_matmul_readvariableop_resource2while_lstm_cell_7_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2T
(while/lstm_cell_7/BiasAdd/ReadVariableOp(while/lstm_cell_7/BiasAdd/ReadVariableOp2R
'while/lstm_cell_7/MatMul/ReadVariableOp'while/lstm_cell_7/MatMul/ReadVariableOp2V
)while/lstm_cell_7/MatMul_1/ReadVariableOp)while/lstm_cell_7/MatMul_1/ReadVariableOp: 
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
?
?
while_cond_36455
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_36455___redundant_placeholder03
/while_while_cond_36455___redundant_placeholder13
/while_while_cond_36455___redundant_placeholder23
/while_while_cond_36455___redundant_placeholder3
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
?m
?
A__inference_lstm_5_layer_call_and_return_conditional_losses_40119
inputs_0=
*lstm_cell_7_matmul_readvariableop_resource:	?>
,lstm_cell_7_matmul_1_readvariableop_resource:22A
.lstm_cell_7_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_7_biasadd_readvariableop_resource:	?;
)lstm_cell_7_mul_2_readvariableop_resource:22
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_7/BiasAdd/ReadVariableOp?!lstm_cell_7/MatMul/ReadVariableOp?#lstm_cell_7/MatMul_1/ReadVariableOp?%lstm_cell_7/MatMul_1/ReadVariableOp_1? lstm_cell_7/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:?????????22
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_7/MatMul/ReadVariableOpReadVariableOp*lstm_cell_7_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02#
!lstm_cell_7/MatMul/ReadVariableOp?
lstm_cell_7/MatMulMatMulstrided_slice_1:output:0)lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/MatMul?
#lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#lstm_cell_7/MatMul_1/ReadVariableOp?
%lstm_cell_7/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_7_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_7/MatMul_1/ReadVariableOp_1?
lstm_cell_7/MatMul_1MatMul+lstm_cell_7/MatMul_1/ReadVariableOp:value:0-lstm_cell_7/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/MatMul_1?
lstm_cell_7/addAddV2lstm_cell_7/MatMul:product:0lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/add?
"lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_7_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_7/BiasAdd/ReadVariableOp?
lstm_cell_7/BiasAddBiasAddlstm_cell_7/add:z:0*lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/BiasAdd|
lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_7/split/split_dim?
lstm_cell_7/splitSplit$lstm_cell_7/split/split_dim:output:0lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_7/splitk
lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Consto
lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_1?
lstm_cell_7/MulMullstm_cell_7/split:output:0lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul?
lstm_cell_7/Add_1AddV2lstm_cell_7/Mul:z:0lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_1?
#lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_7/clip_by_value/Minimum/y?
!lstm_cell_7/clip_by_value/MinimumMinimumlstm_cell_7/Add_1:z:0,lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222#
!lstm_cell_7/clip_by_value/Minimum
lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value/y?
lstm_cell_7/clip_by_valueMaximum%lstm_cell_7/clip_by_value/Minimum:z:0$lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_valueo
lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Const_2o
lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_3?
lstm_cell_7/Mul_1Mullstm_cell_7/split:output:1lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul_1?
lstm_cell_7/Add_2AddV2lstm_cell_7/Mul_1:z:0lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_2?
%lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_7/clip_by_value_1/Minimum/y?
#lstm_cell_7/clip_by_value_1/MinimumMinimumlstm_cell_7/Add_2:z:0.lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_7/clip_by_value_1/Minimum?
lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value_1/y?
lstm_cell_7/clip_by_value_1Maximum'lstm_cell_7/clip_by_value_1/Minimum:z:0&lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_value_1?
 lstm_cell_7/mul_2/ReadVariableOpReadVariableOp)lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02"
 lstm_cell_7/mul_2/ReadVariableOp?
lstm_cell_7/mul_2Mullstm_cell_7/clip_by_value_1:z:0(lstm_cell_7/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_2q
lstm_cell_7/TanhTanhlstm_cell_7/split:output:2*
T0*
_output_shapes

:222
lstm_cell_7/Tanh?
lstm_cell_7/mul_3Mullstm_cell_7/clip_by_value:z:0lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_3?
lstm_cell_7/add_3AddV2lstm_cell_7/mul_2:z:0lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_7/add_3o
lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Const_4o
lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_5?
lstm_cell_7/Mul_4Mullstm_cell_7/split:output:3lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul_4?
lstm_cell_7/Add_4AddV2lstm_cell_7/Mul_4:z:0lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_4?
%lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_7/clip_by_value_2/Minimum/y?
#lstm_cell_7/clip_by_value_2/MinimumMinimumlstm_cell_7/Add_4:z:0.lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_7/clip_by_value_2/Minimum?
lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value_2/y?
lstm_cell_7/clip_by_value_2Maximum'lstm_cell_7/clip_by_value_2/Minimum:z:0&lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_value_2p
lstm_cell_7/Tanh_1Tanhlstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_7/Tanh_1?
lstm_cell_7/mul_5Mullstm_cell_7/clip_by_value_2:z:0lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp,lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_7_matmul_readvariableop_resource.lstm_cell_7_matmul_1_readvariableop_1_resource+lstm_cell_7_biasadd_readvariableop_resource*
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
while_body_40014*
condR
while_cond_40013*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????22*
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

:22*
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
:2?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_7_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_7/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_7_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_7/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_7/BiasAdd/ReadVariableOp"^lstm_cell_7/MatMul/ReadVariableOp$^lstm_cell_7/MatMul_1/ReadVariableOp&^lstm_cell_7/MatMul_1/ReadVariableOp_1!^lstm_cell_7/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2?????????: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_7/BiasAdd/ReadVariableOp"lstm_cell_7/BiasAdd/ReadVariableOp2F
!lstm_cell_7/MatMul/ReadVariableOp!lstm_cell_7/MatMul/ReadVariableOp2J
#lstm_cell_7/MatMul_1/ReadVariableOp#lstm_cell_7/MatMul_1/ReadVariableOp2N
%lstm_cell_7/MatMul_1/ReadVariableOp_1%lstm_cell_7/MatMul_1/ReadVariableOp_12D
 lstm_cell_7/mul_2/ReadVariableOp lstm_cell_7/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:2?????????
"
_user_specified_name
inputs/0
?9
?
A__inference_lstm_5_layer_call_and_return_conditional_losses_36578

inputs#
lstm_cell_7_36437:22#
lstm_cell_7_36439:22$
lstm_cell_7_36441:	?$
lstm_cell_7_36443:	2? 
lstm_cell_7_36445:	?
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?#lstm_cell_7/StatefulPartitionedCall?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:?????????22
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1?
#lstm_cell_7/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_7_36437lstm_cell_7_36439lstm_cell_7_36441lstm_cell_7_36443lstm_cell_7_36445*
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
GPU 2J 8? *O
fJRH
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_364362%
#lstm_cell_7/StatefulPartitionedCall?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOplstm_cell_7_36437*
_output_shapes

:22*
dtype02
ReadVariableOpv
ReadVariableOp_1ReadVariableOplstm_cell_7_36439*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_7_36441lstm_cell_7_36443lstm_cell_7_36445*
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
while_body_36456*
condR
while_cond_36455*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????22*
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

:22*
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
:2?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOplstm_cell_7_36437while:output:4^ReadVariableOp$^lstm_cell_7/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOplstm_cell_7_36439while:output:5^ReadVariableOp_1$^lstm_cell_7/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_7/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2?????????: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_7/StatefulPartitionedCall#lstm_cell_7/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:2?????????
 
_user_specified_nameinputs
?X
?
while_body_38814
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_7_matmul_readvariableop_resource_0:	?G
4while_lstm_cell_7_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_7_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_7_matmul_readvariableop_resource:	?E
2while_lstm_cell_7_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_7_biasadd_readvariableop_resource:	???(while/lstm_cell_7/BiasAdd/ReadVariableOp?'while/lstm_cell_7/MatMul/ReadVariableOp?)while/lstm_cell_7/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_7/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_7_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02)
'while/lstm_cell_7/MatMul/ReadVariableOp?
while/lstm_cell_7/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/MatMul?
)while/lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_7_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_7/MatMul_1/ReadVariableOp?
while/lstm_cell_7/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_7/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/MatMul_1?
while/lstm_cell_7/addAddV2"while/lstm_cell_7/MatMul:product:0$while/lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/add?
(while/lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_7_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_7/BiasAdd/ReadVariableOp?
while/lstm_cell_7/BiasAddBiasAddwhile/lstm_cell_7/add:z:00while/lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/BiasAdd?
!while/lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_7/split/split_dim?
while/lstm_cell_7/splitSplit*while/lstm_cell_7/split/split_dim:output:0"while/lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_7/splitw
while/lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const{
while/lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_1?
while/lstm_cell_7/MulMul while/lstm_cell_7/split:output:0 while/lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul?
while/lstm_cell_7/Add_1AddV2while/lstm_cell_7/Mul:z:0"while/lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_1?
)while/lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_7/clip_by_value/Minimum/y?
'while/lstm_cell_7/clip_by_value/MinimumMinimumwhile/lstm_cell_7/Add_1:z:02while/lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222)
'while/lstm_cell_7/clip_by_value/Minimum?
!while/lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_7/clip_by_value/y?
while/lstm_cell_7/clip_by_valueMaximum+while/lstm_cell_7/clip_by_value/Minimum:z:0*while/lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222!
while/lstm_cell_7/clip_by_value{
while/lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const_2{
while/lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_3?
while/lstm_cell_7/Mul_1Mul while/lstm_cell_7/split:output:1"while/lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul_1?
while/lstm_cell_7/Add_2AddV2while/lstm_cell_7/Mul_1:z:0"while/lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_2?
+while/lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_7/clip_by_value_1/Minimum/y?
)while/lstm_cell_7/clip_by_value_1/MinimumMinimumwhile/lstm_cell_7/Add_2:z:04while/lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_7/clip_by_value_1/Minimum?
#while/lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_7/clip_by_value_1/y?
!while/lstm_cell_7/clip_by_value_1Maximum-while/lstm_cell_7/clip_by_value_1/Minimum:z:0,while/lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_7/clip_by_value_1?
while/lstm_cell_7/mul_2Mul%while/lstm_cell_7/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_2?
while/lstm_cell_7/TanhTanh while/lstm_cell_7/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_7/Tanh?
while/lstm_cell_7/mul_3Mul#while/lstm_cell_7/clip_by_value:z:0while/lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_3?
while/lstm_cell_7/add_3AddV2while/lstm_cell_7/mul_2:z:0while/lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_7/add_3{
while/lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const_4{
while/lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_5?
while/lstm_cell_7/Mul_4Mul while/lstm_cell_7/split:output:3"while/lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul_4?
while/lstm_cell_7/Add_4AddV2while/lstm_cell_7/Mul_4:z:0"while/lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_4?
+while/lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_7/clip_by_value_2/Minimum/y?
)while/lstm_cell_7/clip_by_value_2/MinimumMinimumwhile/lstm_cell_7/Add_4:z:04while/lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_7/clip_by_value_2/Minimum?
#while/lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_7/clip_by_value_2/y?
!while/lstm_cell_7/clip_by_value_2Maximum-while/lstm_cell_7/clip_by_value_2/Minimum:z:0,while/lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_7/clip_by_value_2?
while/lstm_cell_7/Tanh_1Tanhwhile/lstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Tanh_1?
while/lstm_cell_7/mul_5Mul%while/lstm_cell_7/clip_by_value_2:z:0while/lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_7/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_7/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_7/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_7/BiasAdd/ReadVariableOp(^while/lstm_cell_7/MatMul/ReadVariableOp*^while/lstm_cell_7/MatMul_1/ReadVariableOp*"
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
1while_lstm_cell_7_biasadd_readvariableop_resource3while_lstm_cell_7_biasadd_readvariableop_resource_0"j
2while_lstm_cell_7_matmul_1_readvariableop_resource4while_lstm_cell_7_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_7_matmul_readvariableop_resource2while_lstm_cell_7_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2T
(while/lstm_cell_7/BiasAdd/ReadVariableOp(while/lstm_cell_7/BiasAdd/ReadVariableOp2R
'while/lstm_cell_7/MatMul/ReadVariableOp'while/lstm_cell_7/MatMul/ReadVariableOp2V
)while/lstm_cell_7/MatMul_1/ReadVariableOp)while/lstm_cell_7/MatMul_1/ReadVariableOp: 
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
?
?
+__inference_lstm_cell_6_layer_call_fn_42321

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
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_423082
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
?
?
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_41527

inputs8
&dense_2_matmul_readvariableop_resource:25
'dense_2_biasadd_readvariableop_resource:
identity??dense_2/BiasAdd/ReadVariableOp?dense_2/MatMul/ReadVariableOpD
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
dense_2/MatMul/ReadVariableOpReadVariableOp&dense_2_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_2/MatMul/ReadVariableOp?
dense_2/MatMulMatMulReshape:output:0%dense_2/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_2/MatMul?
dense_2/BiasAdd/ReadVariableOpReadVariableOp'dense_2_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_2/BiasAdd/ReadVariableOp?
dense_2/BiasAddBiasAdddense_2/MatMul:product:0&dense_2/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_2/BiasAddq
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
	Reshape_1Reshapedense_2/BiasAdd:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :??????????????????2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :??????????????????2

Identity?
NoOpNoOp^dense_2/BiasAdd/ReadVariableOp^dense_2/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:??????????????????2: : 2@
dense_2/BiasAdd/ReadVariableOpdense_2/BiasAdd/ReadVariableOp2>
dense_2/MatMul/ReadVariableOpdense_2/MatMul/ReadVariableOp:\ X
4
_output_shapes"
 :??????????????????2
 
_user_specified_nameinputs
?9
?
A__inference_lstm_4_layer_call_and_return_conditional_losses_37671

inputs#
lstm_cell_6_37583:22#
lstm_cell_6_37585:22$
lstm_cell_6_37587:	2?$
lstm_cell_6_37589:	2? 
lstm_cell_6_37591:	?
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?#lstm_cell_6/StatefulPartitionedCall?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:?????????222
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1?
#lstm_cell_6/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_6_37583lstm_cell_6_37585lstm_cell_6_37587lstm_cell_6_37589lstm_cell_6_37591*
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
GPU 2J 8? *O
fJRH
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_375332%
#lstm_cell_6/StatefulPartitionedCall?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOplstm_cell_6_37583*
_output_shapes

:22*
dtype02
ReadVariableOpv
ReadVariableOp_1ReadVariableOplstm_cell_6_37585*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_6_37587lstm_cell_6_37589lstm_cell_6_37591*
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
while_body_37602*
condR
while_cond_37601*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????22*
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

:22*
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
:2?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOplstm_cell_6_37583while:output:4^ReadVariableOp$^lstm_cell_6/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOplstm_cell_6_37585while:output:5^ReadVariableOp_1$^lstm_cell_6/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_6/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2?????????2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_6/StatefulPartitionedCall#lstm_cell_6/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:2?????????2
 
_user_specified_nameinputs
?.
?
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42382

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
:	2?2
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
?e
?

lstm_4_while_body_39766*
&lstm_4_while_lstm_4_while_loop_counter0
,lstm_4_while_lstm_4_while_maximum_iterations
lstm_4_while_placeholder
lstm_4_while_placeholder_1
lstm_4_while_placeholder_2
lstm_4_while_placeholder_3'
#lstm_4_while_lstm_4_strided_slice_0e
alstm_4_while_tensorarrayv2read_tensorlistgetitem_lstm_4_tensorarrayunstack_tensorlistfromtensor_0L
9lstm_4_while_lstm_cell_6_matmul_readvariableop_resource_0:	2?N
;lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource_0:	2?I
:lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource_0:	?
lstm_4_while_identity
lstm_4_while_identity_1
lstm_4_while_identity_2
lstm_4_while_identity_3
lstm_4_while_identity_4
lstm_4_while_identity_5%
!lstm_4_while_lstm_4_strided_slicec
_lstm_4_while_tensorarrayv2read_tensorlistgetitem_lstm_4_tensorarrayunstack_tensorlistfromtensorJ
7lstm_4_while_lstm_cell_6_matmul_readvariableop_resource:	2?L
9lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource:	2?G
8lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource:	???/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp?.lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp?0lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp?
>lstm_4/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2@
>lstm_4/while/TensorArrayV2Read/TensorListGetItem/element_shape?
0lstm_4/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemalstm_4_while_tensorarrayv2read_tensorlistgetitem_lstm_4_tensorarrayunstack_tensorlistfromtensor_0lstm_4_while_placeholderGlstm_4/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype022
0lstm_4/while/TensorArrayV2Read/TensorListGetItem?
.lstm_4/while/lstm_cell_6/MatMul/ReadVariableOpReadVariableOp9lstm_4_while_lstm_cell_6_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype020
.lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp?
lstm_4/while/lstm_cell_6/MatMulMatMul7lstm_4/while/TensorArrayV2Read/TensorListGetItem:item:06lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2!
lstm_4/while/lstm_cell_6/MatMul?
0lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp;lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype022
0lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp?
!lstm_4/while/lstm_cell_6/MatMul_1MatMullstm_4_while_placeholder_28lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2#
!lstm_4/while/lstm_cell_6/MatMul_1?
lstm_4/while/lstm_cell_6/addAddV2)lstm_4/while/lstm_cell_6/MatMul:product:0+lstm_4/while/lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_4/while/lstm_cell_6/add?
/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp:lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype021
/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp?
 lstm_4/while/lstm_cell_6/BiasAddBiasAdd lstm_4/while/lstm_cell_6/add:z:07lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2"
 lstm_4/while/lstm_cell_6/BiasAdd?
(lstm_4/while/lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2*
(lstm_4/while/lstm_cell_6/split/split_dim?
lstm_4/while/lstm_cell_6/splitSplit1lstm_4/while/lstm_cell_6/split/split_dim:output:0)lstm_4/while/lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2 
lstm_4/while/lstm_cell_6/split?
lstm_4/while/lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2 
lstm_4/while/lstm_cell_6/Const?
 lstm_4/while/lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_4/while/lstm_cell_6/Const_1?
lstm_4/while/lstm_cell_6/MulMul'lstm_4/while/lstm_cell_6/split:output:0'lstm_4/while/lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
lstm_4/while/lstm_cell_6/Mul?
lstm_4/while/lstm_cell_6/Add_1AddV2 lstm_4/while/lstm_cell_6/Mul:z:0)lstm_4/while/lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/Add_1?
0lstm_4/while/lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??22
0lstm_4/while/lstm_cell_6/clip_by_value/Minimum/y?
.lstm_4/while/lstm_cell_6/clip_by_value/MinimumMinimum"lstm_4/while/lstm_cell_6/Add_1:z:09lstm_4/while/lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:2220
.lstm_4/while/lstm_cell_6/clip_by_value/Minimum?
(lstm_4/while/lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2*
(lstm_4/while/lstm_cell_6/clip_by_value/y?
&lstm_4/while/lstm_cell_6/clip_by_valueMaximum2lstm_4/while/lstm_cell_6/clip_by_value/Minimum:z:01lstm_4/while/lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222(
&lstm_4/while/lstm_cell_6/clip_by_value?
 lstm_4/while/lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_4/while/lstm_cell_6/Const_2?
 lstm_4/while/lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_4/while/lstm_cell_6/Const_3?
lstm_4/while/lstm_cell_6/Mul_1Mul'lstm_4/while/lstm_cell_6/split:output:1)lstm_4/while/lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/Mul_1?
lstm_4/while/lstm_cell_6/Add_2AddV2"lstm_4/while/lstm_cell_6/Mul_1:z:0)lstm_4/while/lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/Add_2?
2lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum/y?
0lstm_4/while/lstm_cell_6/clip_by_value_1/MinimumMinimum"lstm_4/while/lstm_cell_6/Add_2:z:0;lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:2222
0lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum?
*lstm_4/while/lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_4/while/lstm_cell_6/clip_by_value_1/y?
(lstm_4/while/lstm_cell_6/clip_by_value_1Maximum4lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum:z:03lstm_4/while/lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222*
(lstm_4/while/lstm_cell_6/clip_by_value_1?
lstm_4/while/lstm_cell_6/mul_2Mul,lstm_4/while/lstm_cell_6/clip_by_value_1:z:0lstm_4_while_placeholder_3*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/mul_2?
lstm_4/while/lstm_cell_6/TanhTanh'lstm_4/while/lstm_cell_6/split:output:2*
T0*
_output_shapes

:222
lstm_4/while/lstm_cell_6/Tanh?
lstm_4/while/lstm_cell_6/mul_3Mul*lstm_4/while/lstm_cell_6/clip_by_value:z:0!lstm_4/while/lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/mul_3?
lstm_4/while/lstm_cell_6/add_3AddV2"lstm_4/while/lstm_cell_6/mul_2:z:0"lstm_4/while/lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/add_3?
 lstm_4/while/lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_4/while/lstm_cell_6/Const_4?
 lstm_4/while/lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_4/while/lstm_cell_6/Const_5?
lstm_4/while/lstm_cell_6/Mul_4Mul'lstm_4/while/lstm_cell_6/split:output:3)lstm_4/while/lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/Mul_4?
lstm_4/while/lstm_cell_6/Add_4AddV2"lstm_4/while/lstm_cell_6/Mul_4:z:0)lstm_4/while/lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/Add_4?
2lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum/y?
0lstm_4/while/lstm_cell_6/clip_by_value_2/MinimumMinimum"lstm_4/while/lstm_cell_6/Add_4:z:0;lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:2222
0lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum?
*lstm_4/while/lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_4/while/lstm_cell_6/clip_by_value_2/y?
(lstm_4/while/lstm_cell_6/clip_by_value_2Maximum4lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum:z:03lstm_4/while/lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222*
(lstm_4/while/lstm_cell_6/clip_by_value_2?
lstm_4/while/lstm_cell_6/Tanh_1Tanh"lstm_4/while/lstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222!
lstm_4/while/lstm_cell_6/Tanh_1?
lstm_4/while/lstm_cell_6/mul_5Mul,lstm_4/while/lstm_cell_6/clip_by_value_2:z:0#lstm_4/while/lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222 
lstm_4/while/lstm_cell_6/mul_5?
1lstm_4/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_4_while_placeholder_1lstm_4_while_placeholder"lstm_4/while/lstm_cell_6/mul_5:z:0*
_output_shapes
: *
element_dtype023
1lstm_4/while/TensorArrayV2Write/TensorListSetItemj
lstm_4/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_4/while/add/y?
lstm_4/while/addAddV2lstm_4_while_placeholderlstm_4/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_4/while/addn
lstm_4/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_4/while/add_1/y?
lstm_4/while/add_1AddV2&lstm_4_while_lstm_4_while_loop_counterlstm_4/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_4/while/add_1?
lstm_4/while/IdentityIdentitylstm_4/while/add_1:z:0^lstm_4/while/NoOp*
T0*
_output_shapes
: 2
lstm_4/while/Identity?
lstm_4/while/Identity_1Identity,lstm_4_while_lstm_4_while_maximum_iterations^lstm_4/while/NoOp*
T0*
_output_shapes
: 2
lstm_4/while/Identity_1?
lstm_4/while/Identity_2Identitylstm_4/while/add:z:0^lstm_4/while/NoOp*
T0*
_output_shapes
: 2
lstm_4/while/Identity_2?
lstm_4/while/Identity_3IdentityAlstm_4/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_4/while/NoOp*
T0*
_output_shapes
: 2
lstm_4/while/Identity_3?
lstm_4/while/Identity_4Identity"lstm_4/while/lstm_cell_6/mul_5:z:0^lstm_4/while/NoOp*
T0*
_output_shapes

:222
lstm_4/while/Identity_4?
lstm_4/while/Identity_5Identity"lstm_4/while/lstm_cell_6/add_3:z:0^lstm_4/while/NoOp*
T0*
_output_shapes

:222
lstm_4/while/Identity_5?
lstm_4/while/NoOpNoOp0^lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp/^lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp1^lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_4/while/NoOp"7
lstm_4_while_identitylstm_4/while/Identity:output:0";
lstm_4_while_identity_1 lstm_4/while/Identity_1:output:0";
lstm_4_while_identity_2 lstm_4/while/Identity_2:output:0";
lstm_4_while_identity_3 lstm_4/while/Identity_3:output:0";
lstm_4_while_identity_4 lstm_4/while/Identity_4:output:0";
lstm_4_while_identity_5 lstm_4/while/Identity_5:output:0"H
!lstm_4_while_lstm_4_strided_slice#lstm_4_while_lstm_4_strided_slice_0"v
8lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource:lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource_0"x
9lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource;lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource_0"t
7lstm_4_while_lstm_cell_6_matmul_readvariableop_resource9lstm_4_while_lstm_cell_6_matmul_readvariableop_resource_0"?
_lstm_4_while_tensorarrayv2read_tensorlistgetitem_lstm_4_tensorarrayunstack_tensorlistfromtensoralstm_4_while_tensorarrayv2read_tensorlistgetitem_lstm_4_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2b
/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp2`
.lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp.lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp2d
0lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp0lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp: 
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
?
?
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_41555

inputs8
&dense_2_matmul_readvariableop_resource:25
'dense_2_biasadd_readvariableop_resource:
identity??dense_2/BiasAdd/ReadVariableOp?dense_2/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2
Reshape/shapeg
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes
:	?22	
Reshape?
dense_2/MatMul/ReadVariableOpReadVariableOp&dense_2_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_2/MatMul/ReadVariableOp?
dense_2/MatMulMatMulReshape:output:0%dense_2/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
dense_2/MatMul?
dense_2/BiasAdd/ReadVariableOpReadVariableOp'dense_2_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_2/BiasAdd/ReadVariableOp?
dense_2/BiasAddBiasAdddense_2/MatMul:product:0&dense_2/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
dense_2/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      2
Reshape_1/shape?
	Reshape_1Reshapedense_2/BiasAdd:output:0Reshape_1/shape:output:0*
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

Identity?
NoOpNoOp^dense_2/BiasAdd/ReadVariableOp^dense_2/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:2
2: : 2@
dense_2/BiasAdd/ReadVariableOpdense_2/BiasAdd/ReadVariableOp2>
dense_2/MatMul/ReadVariableOpdense_2/MatMul/ReadVariableOp:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
?e
?

lstm_5_while_body_39592*
&lstm_5_while_lstm_5_while_loop_counter0
,lstm_5_while_lstm_5_while_maximum_iterations
lstm_5_while_placeholder
lstm_5_while_placeholder_1
lstm_5_while_placeholder_2
lstm_5_while_placeholder_3'
#lstm_5_while_lstm_5_strided_slice_0e
alstm_5_while_tensorarrayv2read_tensorlistgetitem_lstm_5_tensorarrayunstack_tensorlistfromtensor_0L
9lstm_5_while_lstm_cell_7_matmul_readvariableop_resource_0:	?N
;lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource_0:	2?I
:lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource_0:	?
lstm_5_while_identity
lstm_5_while_identity_1
lstm_5_while_identity_2
lstm_5_while_identity_3
lstm_5_while_identity_4
lstm_5_while_identity_5%
!lstm_5_while_lstm_5_strided_slicec
_lstm_5_while_tensorarrayv2read_tensorlistgetitem_lstm_5_tensorarrayunstack_tensorlistfromtensorJ
7lstm_5_while_lstm_cell_7_matmul_readvariableop_resource:	?L
9lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource:	2?G
8lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource:	???/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp?.lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp?0lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp?
>lstm_5/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      2@
>lstm_5/while/TensorArrayV2Read/TensorListGetItem/element_shape?
0lstm_5/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemalstm_5_while_tensorarrayv2read_tensorlistgetitem_lstm_5_tensorarrayunstack_tensorlistfromtensor_0lstm_5_while_placeholderGlstm_5/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype022
0lstm_5/while/TensorArrayV2Read/TensorListGetItem?
.lstm_5/while/lstm_cell_7/MatMul/ReadVariableOpReadVariableOp9lstm_5_while_lstm_cell_7_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype020
.lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp?
lstm_5/while/lstm_cell_7/MatMulMatMul7lstm_5/while/TensorArrayV2Read/TensorListGetItem:item:06lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2!
lstm_5/while/lstm_cell_7/MatMul?
0lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp;lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype022
0lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp?
!lstm_5/while/lstm_cell_7/MatMul_1MatMullstm_5_while_placeholder_28lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2#
!lstm_5/while/lstm_cell_7/MatMul_1?
lstm_5/while/lstm_cell_7/addAddV2)lstm_5/while/lstm_cell_7/MatMul:product:0+lstm_5/while/lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_5/while/lstm_cell_7/add?
/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp:lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype021
/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp?
 lstm_5/while/lstm_cell_7/BiasAddBiasAdd lstm_5/while/lstm_cell_7/add:z:07lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2"
 lstm_5/while/lstm_cell_7/BiasAdd?
(lstm_5/while/lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2*
(lstm_5/while/lstm_cell_7/split/split_dim?
lstm_5/while/lstm_cell_7/splitSplit1lstm_5/while/lstm_cell_7/split/split_dim:output:0)lstm_5/while/lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2 
lstm_5/while/lstm_cell_7/split?
lstm_5/while/lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2 
lstm_5/while/lstm_cell_7/Const?
 lstm_5/while/lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_5/while/lstm_cell_7/Const_1?
lstm_5/while/lstm_cell_7/MulMul'lstm_5/while/lstm_cell_7/split:output:0'lstm_5/while/lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
lstm_5/while/lstm_cell_7/Mul?
lstm_5/while/lstm_cell_7/Add_1AddV2 lstm_5/while/lstm_cell_7/Mul:z:0)lstm_5/while/lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/Add_1?
0lstm_5/while/lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??22
0lstm_5/while/lstm_cell_7/clip_by_value/Minimum/y?
.lstm_5/while/lstm_cell_7/clip_by_value/MinimumMinimum"lstm_5/while/lstm_cell_7/Add_1:z:09lstm_5/while/lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:2220
.lstm_5/while/lstm_cell_7/clip_by_value/Minimum?
(lstm_5/while/lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2*
(lstm_5/while/lstm_cell_7/clip_by_value/y?
&lstm_5/while/lstm_cell_7/clip_by_valueMaximum2lstm_5/while/lstm_cell_7/clip_by_value/Minimum:z:01lstm_5/while/lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222(
&lstm_5/while/lstm_cell_7/clip_by_value?
 lstm_5/while/lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_5/while/lstm_cell_7/Const_2?
 lstm_5/while/lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_5/while/lstm_cell_7/Const_3?
lstm_5/while/lstm_cell_7/Mul_1Mul'lstm_5/while/lstm_cell_7/split:output:1)lstm_5/while/lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/Mul_1?
lstm_5/while/lstm_cell_7/Add_2AddV2"lstm_5/while/lstm_cell_7/Mul_1:z:0)lstm_5/while/lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/Add_2?
2lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum/y?
0lstm_5/while/lstm_cell_7/clip_by_value_1/MinimumMinimum"lstm_5/while/lstm_cell_7/Add_2:z:0;lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:2222
0lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum?
*lstm_5/while/lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_5/while/lstm_cell_7/clip_by_value_1/y?
(lstm_5/while/lstm_cell_7/clip_by_value_1Maximum4lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum:z:03lstm_5/while/lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222*
(lstm_5/while/lstm_cell_7/clip_by_value_1?
lstm_5/while/lstm_cell_7/mul_2Mul,lstm_5/while/lstm_cell_7/clip_by_value_1:z:0lstm_5_while_placeholder_3*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/mul_2?
lstm_5/while/lstm_cell_7/TanhTanh'lstm_5/while/lstm_cell_7/split:output:2*
T0*
_output_shapes

:222
lstm_5/while/lstm_cell_7/Tanh?
lstm_5/while/lstm_cell_7/mul_3Mul*lstm_5/while/lstm_cell_7/clip_by_value:z:0!lstm_5/while/lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/mul_3?
lstm_5/while/lstm_cell_7/add_3AddV2"lstm_5/while/lstm_cell_7/mul_2:z:0"lstm_5/while/lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/add_3?
 lstm_5/while/lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_5/while/lstm_cell_7/Const_4?
 lstm_5/while/lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_5/while/lstm_cell_7/Const_5?
lstm_5/while/lstm_cell_7/Mul_4Mul'lstm_5/while/lstm_cell_7/split:output:3)lstm_5/while/lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/Mul_4?
lstm_5/while/lstm_cell_7/Add_4AddV2"lstm_5/while/lstm_cell_7/Mul_4:z:0)lstm_5/while/lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/Add_4?
2lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum/y?
0lstm_5/while/lstm_cell_7/clip_by_value_2/MinimumMinimum"lstm_5/while/lstm_cell_7/Add_4:z:0;lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:2222
0lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum?
*lstm_5/while/lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_5/while/lstm_cell_7/clip_by_value_2/y?
(lstm_5/while/lstm_cell_7/clip_by_value_2Maximum4lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum:z:03lstm_5/while/lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222*
(lstm_5/while/lstm_cell_7/clip_by_value_2?
lstm_5/while/lstm_cell_7/Tanh_1Tanh"lstm_5/while/lstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222!
lstm_5/while/lstm_cell_7/Tanh_1?
lstm_5/while/lstm_cell_7/mul_5Mul,lstm_5/while/lstm_cell_7/clip_by_value_2:z:0#lstm_5/while/lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/mul_5?
1lstm_5/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_5_while_placeholder_1lstm_5_while_placeholder"lstm_5/while/lstm_cell_7/mul_5:z:0*
_output_shapes
: *
element_dtype023
1lstm_5/while/TensorArrayV2Write/TensorListSetItemj
lstm_5/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_5/while/add/y?
lstm_5/while/addAddV2lstm_5_while_placeholderlstm_5/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_5/while/addn
lstm_5/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_5/while/add_1/y?
lstm_5/while/add_1AddV2&lstm_5_while_lstm_5_while_loop_counterlstm_5/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_5/while/add_1?
lstm_5/while/IdentityIdentitylstm_5/while/add_1:z:0^lstm_5/while/NoOp*
T0*
_output_shapes
: 2
lstm_5/while/Identity?
lstm_5/while/Identity_1Identity,lstm_5_while_lstm_5_while_maximum_iterations^lstm_5/while/NoOp*
T0*
_output_shapes
: 2
lstm_5/while/Identity_1?
lstm_5/while/Identity_2Identitylstm_5/while/add:z:0^lstm_5/while/NoOp*
T0*
_output_shapes
: 2
lstm_5/while/Identity_2?
lstm_5/while/Identity_3IdentityAlstm_5/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_5/while/NoOp*
T0*
_output_shapes
: 2
lstm_5/while/Identity_3?
lstm_5/while/Identity_4Identity"lstm_5/while/lstm_cell_7/mul_5:z:0^lstm_5/while/NoOp*
T0*
_output_shapes

:222
lstm_5/while/Identity_4?
lstm_5/while/Identity_5Identity"lstm_5/while/lstm_cell_7/add_3:z:0^lstm_5/while/NoOp*
T0*
_output_shapes

:222
lstm_5/while/Identity_5?
lstm_5/while/NoOpNoOp0^lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp/^lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp1^lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_5/while/NoOp"7
lstm_5_while_identitylstm_5/while/Identity:output:0";
lstm_5_while_identity_1 lstm_5/while/Identity_1:output:0";
lstm_5_while_identity_2 lstm_5/while/Identity_2:output:0";
lstm_5_while_identity_3 lstm_5/while/Identity_3:output:0";
lstm_5_while_identity_4 lstm_5/while/Identity_4:output:0";
lstm_5_while_identity_5 lstm_5/while/Identity_5:output:0"H
!lstm_5_while_lstm_5_strided_slice#lstm_5_while_lstm_5_strided_slice_0"v
8lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource:lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource_0"x
9lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource;lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource_0"t
7lstm_5_while_lstm_cell_7_matmul_readvariableop_resource9lstm_5_while_lstm_cell_7_matmul_readvariableop_resource_0"?
_lstm_5_while_tensorarrayv2read_tensorlistgetitem_lstm_5_tensorarrayunstack_tensorlistfromtensoralstm_5_while_tensorarrayv2read_tensorlistgetitem_lstm_5_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2b
/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp2`
.lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp.lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp2d
0lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp0lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp: 
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
?$
?
while_body_36456
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0,
while_lstm_cell_7_36533_0:	?,
while_lstm_cell_7_36535_0:	2?(
while_lstm_cell_7_36537_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor*
while_lstm_cell_7_36533:	?*
while_lstm_cell_7_36535:	2?&
while_lstm_cell_7_36537:	???)while/lstm_cell_7/StatefulPartitionedCall?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
)while/lstm_cell_7/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_7_36533_0while_lstm_cell_7_36535_0while_lstm_cell_7_36537_0*
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
GPU 2J 8? *O
fJRH
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_365322+
)while/lstm_cell_7/StatefulPartitionedCall?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder2while/lstm_cell_7/StatefulPartitionedCall:output:0*
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
while/Identity_4Identity2while/lstm_cell_7/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identity2while/lstm_cell_7/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp*^while/lstm_cell_7/StatefulPartitionedCall*"
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
while_lstm_cell_7_36533while_lstm_cell_7_36533_0"4
while_lstm_cell_7_36535while_lstm_cell_7_36535_0"4
while_lstm_cell_7_36537while_lstm_cell_7_36537_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_7/StatefulPartitionedCall)while/lstm_cell_7/StatefulPartitionedCall: 
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
?m
?
A__inference_lstm_4_layer_call_and_return_conditional_losses_38711

inputs=
*lstm_cell_6_matmul_readvariableop_resource:	2?>
,lstm_cell_6_matmul_1_readvariableop_resource:22A
.lstm_cell_6_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_6_biasadd_readvariableop_resource:	?;
)lstm_cell_6_mul_2_readvariableop_resource:22
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_6/BiasAdd/ReadVariableOp?!lstm_cell_6/MatMul/ReadVariableOp?#lstm_cell_6/MatMul_1/ReadVariableOp?%lstm_cell_6/MatMul_1/ReadVariableOp_1? lstm_cell_6/mul_2/ReadVariableOp?whileu
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_6/MatMul/ReadVariableOpReadVariableOp*lstm_cell_6_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02#
!lstm_cell_6/MatMul/ReadVariableOp?
lstm_cell_6/MatMulMatMulstrided_slice_1:output:0)lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/MatMul?
#lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#lstm_cell_6/MatMul_1/ReadVariableOp?
%lstm_cell_6/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_6_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_6/MatMul_1/ReadVariableOp_1?
lstm_cell_6/MatMul_1MatMul+lstm_cell_6/MatMul_1/ReadVariableOp:value:0-lstm_cell_6/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/MatMul_1?
lstm_cell_6/addAddV2lstm_cell_6/MatMul:product:0lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/add?
"lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_6_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_6/BiasAdd/ReadVariableOp?
lstm_cell_6/BiasAddBiasAddlstm_cell_6/add:z:0*lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/BiasAdd|
lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_6/split/split_dim?
lstm_cell_6/splitSplit$lstm_cell_6/split/split_dim:output:0lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_6/splitk
lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Consto
lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_1?
lstm_cell_6/MulMullstm_cell_6/split:output:0lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul?
lstm_cell_6/Add_1AddV2lstm_cell_6/Mul:z:0lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_1?
#lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_6/clip_by_value/Minimum/y?
!lstm_cell_6/clip_by_value/MinimumMinimumlstm_cell_6/Add_1:z:0,lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222#
!lstm_cell_6/clip_by_value/Minimum
lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value/y?
lstm_cell_6/clip_by_valueMaximum%lstm_cell_6/clip_by_value/Minimum:z:0$lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_valueo
lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Const_2o
lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_3?
lstm_cell_6/Mul_1Mullstm_cell_6/split:output:1lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul_1?
lstm_cell_6/Add_2AddV2lstm_cell_6/Mul_1:z:0lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_2?
%lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_6/clip_by_value_1/Minimum/y?
#lstm_cell_6/clip_by_value_1/MinimumMinimumlstm_cell_6/Add_2:z:0.lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_6/clip_by_value_1/Minimum?
lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value_1/y?
lstm_cell_6/clip_by_value_1Maximum'lstm_cell_6/clip_by_value_1/Minimum:z:0&lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_value_1?
 lstm_cell_6/mul_2/ReadVariableOpReadVariableOp)lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02"
 lstm_cell_6/mul_2/ReadVariableOp?
lstm_cell_6/mul_2Mullstm_cell_6/clip_by_value_1:z:0(lstm_cell_6/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_2q
lstm_cell_6/TanhTanhlstm_cell_6/split:output:2*
T0*
_output_shapes

:222
lstm_cell_6/Tanh?
lstm_cell_6/mul_3Mullstm_cell_6/clip_by_value:z:0lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_3?
lstm_cell_6/add_3AddV2lstm_cell_6/mul_2:z:0lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_6/add_3o
lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Const_4o
lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_5?
lstm_cell_6/Mul_4Mullstm_cell_6/split:output:3lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul_4?
lstm_cell_6/Add_4AddV2lstm_cell_6/Mul_4:z:0lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_4?
%lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_6/clip_by_value_2/Minimum/y?
#lstm_cell_6/clip_by_value_2/MinimumMinimumlstm_cell_6/Add_4:z:0.lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_6/clip_by_value_2/Minimum?
lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value_2/y?
lstm_cell_6/clip_by_value_2Maximum'lstm_cell_6/clip_by_value_2/Minimum:z:0&lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_value_2p
lstm_cell_6/Tanh_1Tanhlstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_6/Tanh_1?
lstm_cell_6/mul_5Mullstm_cell_6/clip_by_value_2:z:0lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp,lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_6_matmul_readvariableop_resource.lstm_cell_6_matmul_1_readvariableop_1_resource+lstm_cell_6_biasadd_readvariableop_resource*
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
while_body_38606*
condR
while_cond_38605*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_6_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_6/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_6_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_6/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_6/BiasAdd/ReadVariableOp"^lstm_cell_6/MatMul/ReadVariableOp$^lstm_cell_6/MatMul_1/ReadVariableOp&^lstm_cell_6/MatMul_1/ReadVariableOp_1!^lstm_cell_6/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_6/BiasAdd/ReadVariableOp"lstm_cell_6/BiasAdd/ReadVariableOp2F
!lstm_cell_6/MatMul/ReadVariableOp!lstm_cell_6/MatMul/ReadVariableOp2J
#lstm_cell_6/MatMul_1/ReadVariableOp#lstm_cell_6/MatMul_1/ReadVariableOp2N
%lstm_cell_6/MatMul_1/ReadVariableOp_1%lstm_cell_6/MatMul_1/ReadVariableOp_12D
 lstm_cell_6/mul_2/ReadVariableOp lstm_cell_6/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
?
?
2__inference_time_distributed_2_layer_call_fn_41582

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
:2
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_384482
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
?0
?
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_36757

inputs
states:22
states_1:221
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
:	2?2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:22*
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
:	2?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2	
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
(:22:22:22:22*
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
 *  ??2
clip_by_value/Minimum/y?
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
clip_by_value/y?
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
 *  ??2
clip_by_value_1/Minimum/y?
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
clip_by_value_1/y?
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
 *  ??2
clip_by_value_2/Minimum/y?
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
clip_by_value_2/y?
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
?$
?
while_body_37602
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0,
while_lstm_cell_6_37626_0:	2?,
while_lstm_cell_6_37628_0:	2?(
while_lstm_cell_6_37630_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor*
while_lstm_cell_6_37626:	2?*
while_lstm_cell_6_37628:	2?&
while_lstm_cell_6_37630:	???)while/lstm_cell_6/StatefulPartitionedCall?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
)while/lstm_cell_6/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_6_37626_0while_lstm_cell_6_37628_0while_lstm_cell_6_37630_0*
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
GPU 2J 8? *O
fJRH
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_374422+
)while/lstm_cell_6/StatefulPartitionedCall?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder2while/lstm_cell_6/StatefulPartitionedCall:output:0*
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
while/Identity_4Identity2while/lstm_cell_6/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identity2while/lstm_cell_6/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp*^while/lstm_cell_6/StatefulPartitionedCall*"
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
while_lstm_cell_6_37626while_lstm_cell_6_37626_0"4
while_lstm_cell_6_37628while_lstm_cell_6_37628_0"4
while_lstm_cell_6_37630while_lstm_cell_6_37630_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_6/StatefulPartitionedCall)while/lstm_cell_6/StatefulPartitionedCall: 
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
?m
?
A__inference_lstm_5_layer_call_and_return_conditional_losses_40475

inputs=
*lstm_cell_7_matmul_readvariableop_resource:	?>
,lstm_cell_7_matmul_1_readvariableop_resource:22A
.lstm_cell_7_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_7_biasadd_readvariableop_resource:	?;
)lstm_cell_7_mul_2_readvariableop_resource:22
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_7/BiasAdd/ReadVariableOp?!lstm_cell_7/MatMul/ReadVariableOp?#lstm_cell_7/MatMul_1/ReadVariableOp?%lstm_cell_7/MatMul_1/ReadVariableOp_1? lstm_cell_7/mul_2/ReadVariableOp?whileu
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_7/MatMul/ReadVariableOpReadVariableOp*lstm_cell_7_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02#
!lstm_cell_7/MatMul/ReadVariableOp?
lstm_cell_7/MatMulMatMulstrided_slice_1:output:0)lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/MatMul?
#lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#lstm_cell_7/MatMul_1/ReadVariableOp?
%lstm_cell_7/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_7_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_7/MatMul_1/ReadVariableOp_1?
lstm_cell_7/MatMul_1MatMul+lstm_cell_7/MatMul_1/ReadVariableOp:value:0-lstm_cell_7/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/MatMul_1?
lstm_cell_7/addAddV2lstm_cell_7/MatMul:product:0lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/add?
"lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_7_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_7/BiasAdd/ReadVariableOp?
lstm_cell_7/BiasAddBiasAddlstm_cell_7/add:z:0*lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/BiasAdd|
lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_7/split/split_dim?
lstm_cell_7/splitSplit$lstm_cell_7/split/split_dim:output:0lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_7/splitk
lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Consto
lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_1?
lstm_cell_7/MulMullstm_cell_7/split:output:0lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul?
lstm_cell_7/Add_1AddV2lstm_cell_7/Mul:z:0lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_1?
#lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_7/clip_by_value/Minimum/y?
!lstm_cell_7/clip_by_value/MinimumMinimumlstm_cell_7/Add_1:z:0,lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222#
!lstm_cell_7/clip_by_value/Minimum
lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value/y?
lstm_cell_7/clip_by_valueMaximum%lstm_cell_7/clip_by_value/Minimum:z:0$lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_valueo
lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Const_2o
lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_3?
lstm_cell_7/Mul_1Mullstm_cell_7/split:output:1lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul_1?
lstm_cell_7/Add_2AddV2lstm_cell_7/Mul_1:z:0lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_2?
%lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_7/clip_by_value_1/Minimum/y?
#lstm_cell_7/clip_by_value_1/MinimumMinimumlstm_cell_7/Add_2:z:0.lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_7/clip_by_value_1/Minimum?
lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value_1/y?
lstm_cell_7/clip_by_value_1Maximum'lstm_cell_7/clip_by_value_1/Minimum:z:0&lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_value_1?
 lstm_cell_7/mul_2/ReadVariableOpReadVariableOp)lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02"
 lstm_cell_7/mul_2/ReadVariableOp?
lstm_cell_7/mul_2Mullstm_cell_7/clip_by_value_1:z:0(lstm_cell_7/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_2q
lstm_cell_7/TanhTanhlstm_cell_7/split:output:2*
T0*
_output_shapes

:222
lstm_cell_7/Tanh?
lstm_cell_7/mul_3Mullstm_cell_7/clip_by_value:z:0lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_3?
lstm_cell_7/add_3AddV2lstm_cell_7/mul_2:z:0lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_7/add_3o
lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Const_4o
lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_5?
lstm_cell_7/Mul_4Mullstm_cell_7/split:output:3lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul_4?
lstm_cell_7/Add_4AddV2lstm_cell_7/Mul_4:z:0lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_4?
%lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_7/clip_by_value_2/Minimum/y?
#lstm_cell_7/clip_by_value_2/MinimumMinimumlstm_cell_7/Add_4:z:0.lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_7/clip_by_value_2/Minimum?
lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value_2/y?
lstm_cell_7/clip_by_value_2Maximum'lstm_cell_7/clip_by_value_2/Minimum:z:0&lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_value_2p
lstm_cell_7/Tanh_1Tanhlstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_7/Tanh_1?
lstm_cell_7/mul_5Mullstm_cell_7/clip_by_value_2:z:0lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp,lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_7_matmul_readvariableop_resource.lstm_cell_7_matmul_1_readvariableop_1_resource+lstm_cell_7_biasadd_readvariableop_resource*
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
while_body_40370*
condR
while_cond_40369*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_7_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_7/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_7_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_7/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_7/BiasAdd/ReadVariableOp"^lstm_cell_7/MatMul/ReadVariableOp$^lstm_cell_7/MatMul_1/ReadVariableOp&^lstm_cell_7/MatMul_1/ReadVariableOp_1!^lstm_cell_7/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_7/BiasAdd/ReadVariableOp"lstm_cell_7/BiasAdd/ReadVariableOp2F
!lstm_cell_7/MatMul/ReadVariableOp!lstm_cell_7/MatMul/ReadVariableOp2J
#lstm_cell_7/MatMul_1/ReadVariableOp#lstm_cell_7/MatMul_1/ReadVariableOp2N
%lstm_cell_7/MatMul_1/ReadVariableOp_1%lstm_cell_7/MatMul_1/ReadVariableOp_12D
 lstm_cell_7/mul_2/ReadVariableOp lstm_cell_7/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
?
?
while_cond_38128
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_38128___redundant_placeholder03
/while_while_cond_38128___redundant_placeholder13
/while_while_cond_38128___redundant_placeholder23
/while_while_cond_38128___redundant_placeholder3
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
ȋ
?
G__inference_sequential_2_layer_call_and_return_conditional_losses_39519

inputsD
1lstm_5_lstm_cell_7_matmul_readvariableop_resource:	?E
3lstm_5_lstm_cell_7_matmul_1_readvariableop_resource:22H
5lstm_5_lstm_cell_7_matmul_1_readvariableop_1_resource:	2?A
2lstm_5_lstm_cell_7_biasadd_readvariableop_resource:	?B
0lstm_5_lstm_cell_7_mul_2_readvariableop_resource:22D
1lstm_4_lstm_cell_6_matmul_readvariableop_resource:	2?E
3lstm_4_lstm_cell_6_matmul_1_readvariableop_resource:22H
5lstm_4_lstm_cell_6_matmul_1_readvariableop_1_resource:	2?A
2lstm_4_lstm_cell_6_biasadd_readvariableop_resource:	?B
0lstm_4_lstm_cell_6_mul_2_readvariableop_resource:22K
9time_distributed_2_dense_2_matmul_readvariableop_resource:2H
:time_distributed_2_dense_2_biasadd_readvariableop_resource:
identity??lstm_4/AssignVariableOp?lstm_4/AssignVariableOp_1?lstm_4/ReadVariableOp?lstm_4/ReadVariableOp_1?)lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp?(lstm_4/lstm_cell_6/MatMul/ReadVariableOp?*lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp?,lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1?'lstm_4/lstm_cell_6/mul_2/ReadVariableOp?lstm_4/while?lstm_5/AssignVariableOp?lstm_5/AssignVariableOp_1?lstm_5/ReadVariableOp?lstm_5/ReadVariableOp_1?)lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp?(lstm_5/lstm_cell_7/MatMul/ReadVariableOp?*lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp?,lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1?'lstm_5/lstm_cell_7/mul_2/ReadVariableOp?lstm_5/while?1time_distributed_2/dense_2/BiasAdd/ReadVariableOp?0time_distributed_2/dense_2/MatMul/ReadVariableOp?
lstm_5/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_5/transpose/perm?
lstm_5/transpose	Transposeinputslstm_5/transpose/perm:output:0*
T0*"
_output_shapes
:
22
lstm_5/transposeq
lstm_5/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2      2
lstm_5/Shape?
lstm_5/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_5/strided_slice/stack?
lstm_5/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_5/strided_slice/stack_1?
lstm_5/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_5/strided_slice/stack_2?
lstm_5/strided_sliceStridedSlicelstm_5/Shape:output:0#lstm_5/strided_slice/stack:output:0%lstm_5/strided_slice/stack_1:output:0%lstm_5/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_5/strided_slice?
"lstm_5/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2$
"lstm_5/TensorArrayV2/element_shape?
lstm_5/TensorArrayV2TensorListReserve+lstm_5/TensorArrayV2/element_shape:output:0lstm_5/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_5/TensorArrayV2?
<lstm_5/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      2>
<lstm_5/TensorArrayUnstack/TensorListFromTensor/element_shape?
.lstm_5/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_5/transpose:y:0Elstm_5/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type020
.lstm_5/TensorArrayUnstack/TensorListFromTensor?
lstm_5/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_5/strided_slice_1/stack?
lstm_5/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_5/strided_slice_1/stack_1?
lstm_5/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_5/strided_slice_1/stack_2?
lstm_5/strided_slice_1StridedSlicelstm_5/transpose:y:0%lstm_5/strided_slice_1/stack:output:0'lstm_5/strided_slice_1/stack_1:output:0'lstm_5/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_5/strided_slice_1?
(lstm_5/lstm_cell_7/MatMul/ReadVariableOpReadVariableOp1lstm_5_lstm_cell_7_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02*
(lstm_5/lstm_cell_7/MatMul/ReadVariableOp?
lstm_5/lstm_cell_7/MatMulMatMullstm_5/strided_slice_1:output:00lstm_5/lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_5/lstm_cell_7/MatMul?
*lstm_5/lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp3lstm_5_lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02,
*lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp?
,lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1ReadVariableOp5lstm_5_lstm_cell_7_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02.
,lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1?
lstm_5/lstm_cell_7/MatMul_1MatMul2lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp:value:04lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_5/lstm_cell_7/MatMul_1?
lstm_5/lstm_cell_7/addAddV2#lstm_5/lstm_cell_7/MatMul:product:0%lstm_5/lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_5/lstm_cell_7/add?
)lstm_5/lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp2lstm_5_lstm_cell_7_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02+
)lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp?
lstm_5/lstm_cell_7/BiasAddBiasAddlstm_5/lstm_cell_7/add:z:01lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_5/lstm_cell_7/BiasAdd?
"lstm_5/lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"lstm_5/lstm_cell_7/split/split_dim?
lstm_5/lstm_cell_7/splitSplit+lstm_5/lstm_cell_7/split/split_dim:output:0#lstm_5/lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_5/lstm_cell_7/splity
lstm_5/lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_5/lstm_cell_7/Const}
lstm_5/lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_5/lstm_cell_7/Const_1?
lstm_5/lstm_cell_7/MulMul!lstm_5/lstm_cell_7/split:output:0!lstm_5/lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Mul?
lstm_5/lstm_cell_7/Add_1AddV2lstm_5/lstm_cell_7/Mul:z:0#lstm_5/lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Add_1?
*lstm_5/lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2,
*lstm_5/lstm_cell_7/clip_by_value/Minimum/y?
(lstm_5/lstm_cell_7/clip_by_value/MinimumMinimumlstm_5/lstm_cell_7/Add_1:z:03lstm_5/lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(lstm_5/lstm_cell_7/clip_by_value/Minimum?
"lstm_5/lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"lstm_5/lstm_cell_7/clip_by_value/y?
 lstm_5/lstm_cell_7/clip_by_valueMaximum,lstm_5/lstm_cell_7/clip_by_value/Minimum:z:0+lstm_5/lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 lstm_5/lstm_cell_7/clip_by_value}
lstm_5/lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_5/lstm_cell_7/Const_2}
lstm_5/lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_5/lstm_cell_7/Const_3?
lstm_5/lstm_cell_7/Mul_1Mul!lstm_5/lstm_cell_7/split:output:1#lstm_5/lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Mul_1?
lstm_5/lstm_cell_7/Add_2AddV2lstm_5/lstm_cell_7/Mul_1:z:0#lstm_5/lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Add_2?
,lstm_5/lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_5/lstm_cell_7/clip_by_value_1/Minimum/y?
*lstm_5/lstm_cell_7/clip_by_value_1/MinimumMinimumlstm_5/lstm_cell_7/Add_2:z:05lstm_5/lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*lstm_5/lstm_cell_7/clip_by_value_1/Minimum?
$lstm_5/lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_5/lstm_cell_7/clip_by_value_1/y?
"lstm_5/lstm_cell_7/clip_by_value_1Maximum.lstm_5/lstm_cell_7/clip_by_value_1/Minimum:z:0-lstm_5/lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"lstm_5/lstm_cell_7/clip_by_value_1?
'lstm_5/lstm_cell_7/mul_2/ReadVariableOpReadVariableOp0lstm_5_lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02)
'lstm_5/lstm_cell_7/mul_2/ReadVariableOp?
lstm_5/lstm_cell_7/mul_2Mul&lstm_5/lstm_cell_7/clip_by_value_1:z:0/lstm_5/lstm_cell_7/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/mul_2?
lstm_5/lstm_cell_7/TanhTanh!lstm_5/lstm_cell_7/split:output:2*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Tanh?
lstm_5/lstm_cell_7/mul_3Mul$lstm_5/lstm_cell_7/clip_by_value:z:0lstm_5/lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/mul_3?
lstm_5/lstm_cell_7/add_3AddV2lstm_5/lstm_cell_7/mul_2:z:0lstm_5/lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/add_3}
lstm_5/lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_5/lstm_cell_7/Const_4}
lstm_5/lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_5/lstm_cell_7/Const_5?
lstm_5/lstm_cell_7/Mul_4Mul!lstm_5/lstm_cell_7/split:output:3#lstm_5/lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Mul_4?
lstm_5/lstm_cell_7/Add_4AddV2lstm_5/lstm_cell_7/Mul_4:z:0#lstm_5/lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Add_4?
,lstm_5/lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_5/lstm_cell_7/clip_by_value_2/Minimum/y?
*lstm_5/lstm_cell_7/clip_by_value_2/MinimumMinimumlstm_5/lstm_cell_7/Add_4:z:05lstm_5/lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*lstm_5/lstm_cell_7/clip_by_value_2/Minimum?
$lstm_5/lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_5/lstm_cell_7/clip_by_value_2/y?
"lstm_5/lstm_cell_7/clip_by_value_2Maximum.lstm_5/lstm_cell_7/clip_by_value_2/Minimum:z:0-lstm_5/lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"lstm_5/lstm_cell_7/clip_by_value_2?
lstm_5/lstm_cell_7/Tanh_1Tanhlstm_5/lstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Tanh_1?
lstm_5/lstm_cell_7/mul_5Mul&lstm_5/lstm_cell_7/clip_by_value_2:z:0lstm_5/lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/mul_5?
$lstm_5/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2&
$lstm_5/TensorArrayV2_1/element_shape?
lstm_5/TensorArrayV2_1TensorListReserve-lstm_5/TensorArrayV2_1/element_shape:output:0lstm_5/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_5/TensorArrayV2_1\
lstm_5/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_5/time?
lstm_5/ReadVariableOpReadVariableOp3lstm_5_lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_5/ReadVariableOp?
lstm_5/ReadVariableOp_1ReadVariableOp0lstm_5_lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_5/ReadVariableOp_1?
lstm_5/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2!
lstm_5/while/maximum_iterationsx
lstm_5/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_5/while/loop_counter?
lstm_5/whileWhile"lstm_5/while/loop_counter:output:0(lstm_5/while/maximum_iterations:output:0lstm_5/time:output:0lstm_5/TensorArrayV2_1:handle:0lstm_5/ReadVariableOp:value:0lstm_5/ReadVariableOp_1:value:0lstm_5/strided_slice:output:0>lstm_5/TensorArrayUnstack/TensorListFromTensor:output_handle:01lstm_5_lstm_cell_7_matmul_readvariableop_resource5lstm_5_lstm_cell_7_matmul_1_readvariableop_1_resource2lstm_5_lstm_cell_7_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *#
bodyR
lstm_5_while_body_39228*#
condR
lstm_5_while_cond_39227*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
lstm_5/while?
7lstm_5/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7lstm_5/TensorArrayV2Stack/TensorListStack/element_shape?
)lstm_5/TensorArrayV2Stack/TensorListStackTensorListStacklstm_5/while:output:3@lstm_5/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
element_dtype02+
)lstm_5/TensorArrayV2Stack/TensorListStack?
lstm_5/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
lstm_5/strided_slice_2/stack?
lstm_5/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2 
lstm_5/strided_slice_2/stack_1?
lstm_5/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_5/strided_slice_2/stack_2?
lstm_5/strided_slice_2StridedSlice2lstm_5/TensorArrayV2Stack/TensorListStack:tensor:0%lstm_5/strided_slice_2/stack:output:0'lstm_5/strided_slice_2/stack_1:output:0'lstm_5/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2
lstm_5/strided_slice_2?
lstm_5/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_5/transpose_1/perm?
lstm_5/transpose_1	Transpose2lstm_5/TensorArrayV2Stack/TensorListStack:tensor:0 lstm_5/transpose_1/perm:output:0*
T0*"
_output_shapes
:2
22
lstm_5/transpose_1t
lstm_5/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_5/runtime?
lstm_5/AssignVariableOpAssignVariableOp3lstm_5_lstm_cell_7_matmul_1_readvariableop_resourcelstm_5/while:output:4^lstm_5/ReadVariableOp+^lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_5/AssignVariableOp?
lstm_5/AssignVariableOp_1AssignVariableOp0lstm_5_lstm_cell_7_mul_2_readvariableop_resourcelstm_5/while:output:5^lstm_5/ReadVariableOp_1(^lstm_5/lstm_cell_7/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_5/AssignVariableOp_1?
lstm_4/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_4/transpose/perm?
lstm_4/transpose	Transposelstm_5/transpose_1:y:0lstm_4/transpose/perm:output:0*
T0*"
_output_shapes
:
222
lstm_4/transposeq
lstm_4/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2   2   2
lstm_4/Shape?
lstm_4/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_4/strided_slice/stack?
lstm_4/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_4/strided_slice/stack_1?
lstm_4/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_4/strided_slice/stack_2?
lstm_4/strided_sliceStridedSlicelstm_4/Shape:output:0#lstm_4/strided_slice/stack:output:0%lstm_4/strided_slice/stack_1:output:0%lstm_4/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_4/strided_slice?
"lstm_4/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2$
"lstm_4/TensorArrayV2/element_shape?
lstm_4/TensorArrayV2TensorListReserve+lstm_4/TensorArrayV2/element_shape:output:0lstm_4/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_4/TensorArrayV2?
<lstm_4/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2>
<lstm_4/TensorArrayUnstack/TensorListFromTensor/element_shape?
.lstm_4/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_4/transpose:y:0Elstm_4/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type020
.lstm_4/TensorArrayUnstack/TensorListFromTensor?
lstm_4/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_4/strided_slice_1/stack?
lstm_4/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_4/strided_slice_1/stack_1?
lstm_4/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_4/strided_slice_1/stack_2?
lstm_4/strided_slice_1StridedSlicelstm_4/transpose:y:0%lstm_4/strided_slice_1/stack:output:0'lstm_4/strided_slice_1/stack_1:output:0'lstm_4/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2
lstm_4/strided_slice_1?
(lstm_4/lstm_cell_6/MatMul/ReadVariableOpReadVariableOp1lstm_4_lstm_cell_6_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02*
(lstm_4/lstm_cell_6/MatMul/ReadVariableOp?
lstm_4/lstm_cell_6/MatMulMatMullstm_4/strided_slice_1:output:00lstm_4/lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_4/lstm_cell_6/MatMul?
*lstm_4/lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp3lstm_4_lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02,
*lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp?
,lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1ReadVariableOp5lstm_4_lstm_cell_6_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02.
,lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1?
lstm_4/lstm_cell_6/MatMul_1MatMul2lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp:value:04lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_4/lstm_cell_6/MatMul_1?
lstm_4/lstm_cell_6/addAddV2#lstm_4/lstm_cell_6/MatMul:product:0%lstm_4/lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_4/lstm_cell_6/add?
)lstm_4/lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp2lstm_4_lstm_cell_6_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02+
)lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp?
lstm_4/lstm_cell_6/BiasAddBiasAddlstm_4/lstm_cell_6/add:z:01lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_4/lstm_cell_6/BiasAdd?
"lstm_4/lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"lstm_4/lstm_cell_6/split/split_dim?
lstm_4/lstm_cell_6/splitSplit+lstm_4/lstm_cell_6/split/split_dim:output:0#lstm_4/lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_4/lstm_cell_6/splity
lstm_4/lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_4/lstm_cell_6/Const}
lstm_4/lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_4/lstm_cell_6/Const_1?
lstm_4/lstm_cell_6/MulMul!lstm_4/lstm_cell_6/split:output:0!lstm_4/lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Mul?
lstm_4/lstm_cell_6/Add_1AddV2lstm_4/lstm_cell_6/Mul:z:0#lstm_4/lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Add_1?
*lstm_4/lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2,
*lstm_4/lstm_cell_6/clip_by_value/Minimum/y?
(lstm_4/lstm_cell_6/clip_by_value/MinimumMinimumlstm_4/lstm_cell_6/Add_1:z:03lstm_4/lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(lstm_4/lstm_cell_6/clip_by_value/Minimum?
"lstm_4/lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"lstm_4/lstm_cell_6/clip_by_value/y?
 lstm_4/lstm_cell_6/clip_by_valueMaximum,lstm_4/lstm_cell_6/clip_by_value/Minimum:z:0+lstm_4/lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 lstm_4/lstm_cell_6/clip_by_value}
lstm_4/lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_4/lstm_cell_6/Const_2}
lstm_4/lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_4/lstm_cell_6/Const_3?
lstm_4/lstm_cell_6/Mul_1Mul!lstm_4/lstm_cell_6/split:output:1#lstm_4/lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Mul_1?
lstm_4/lstm_cell_6/Add_2AddV2lstm_4/lstm_cell_6/Mul_1:z:0#lstm_4/lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Add_2?
,lstm_4/lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_4/lstm_cell_6/clip_by_value_1/Minimum/y?
*lstm_4/lstm_cell_6/clip_by_value_1/MinimumMinimumlstm_4/lstm_cell_6/Add_2:z:05lstm_4/lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*lstm_4/lstm_cell_6/clip_by_value_1/Minimum?
$lstm_4/lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_4/lstm_cell_6/clip_by_value_1/y?
"lstm_4/lstm_cell_6/clip_by_value_1Maximum.lstm_4/lstm_cell_6/clip_by_value_1/Minimum:z:0-lstm_4/lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"lstm_4/lstm_cell_6/clip_by_value_1?
'lstm_4/lstm_cell_6/mul_2/ReadVariableOpReadVariableOp0lstm_4_lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02)
'lstm_4/lstm_cell_6/mul_2/ReadVariableOp?
lstm_4/lstm_cell_6/mul_2Mul&lstm_4/lstm_cell_6/clip_by_value_1:z:0/lstm_4/lstm_cell_6/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/mul_2?
lstm_4/lstm_cell_6/TanhTanh!lstm_4/lstm_cell_6/split:output:2*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Tanh?
lstm_4/lstm_cell_6/mul_3Mul$lstm_4/lstm_cell_6/clip_by_value:z:0lstm_4/lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/mul_3?
lstm_4/lstm_cell_6/add_3AddV2lstm_4/lstm_cell_6/mul_2:z:0lstm_4/lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/add_3}
lstm_4/lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_4/lstm_cell_6/Const_4}
lstm_4/lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_4/lstm_cell_6/Const_5?
lstm_4/lstm_cell_6/Mul_4Mul!lstm_4/lstm_cell_6/split:output:3#lstm_4/lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Mul_4?
lstm_4/lstm_cell_6/Add_4AddV2lstm_4/lstm_cell_6/Mul_4:z:0#lstm_4/lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Add_4?
,lstm_4/lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_4/lstm_cell_6/clip_by_value_2/Minimum/y?
*lstm_4/lstm_cell_6/clip_by_value_2/MinimumMinimumlstm_4/lstm_cell_6/Add_4:z:05lstm_4/lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*lstm_4/lstm_cell_6/clip_by_value_2/Minimum?
$lstm_4/lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_4/lstm_cell_6/clip_by_value_2/y?
"lstm_4/lstm_cell_6/clip_by_value_2Maximum.lstm_4/lstm_cell_6/clip_by_value_2/Minimum:z:0-lstm_4/lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"lstm_4/lstm_cell_6/clip_by_value_2?
lstm_4/lstm_cell_6/Tanh_1Tanhlstm_4/lstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Tanh_1?
lstm_4/lstm_cell_6/mul_5Mul&lstm_4/lstm_cell_6/clip_by_value_2:z:0lstm_4/lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/mul_5?
$lstm_4/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2&
$lstm_4/TensorArrayV2_1/element_shape?
lstm_4/TensorArrayV2_1TensorListReserve-lstm_4/TensorArrayV2_1/element_shape:output:0lstm_4/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_4/TensorArrayV2_1\
lstm_4/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_4/time?
lstm_4/ReadVariableOpReadVariableOp3lstm_4_lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_4/ReadVariableOp?
lstm_4/ReadVariableOp_1ReadVariableOp0lstm_4_lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_4/ReadVariableOp_1?
lstm_4/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2!
lstm_4/while/maximum_iterationsx
lstm_4/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_4/while/loop_counter?
lstm_4/whileWhile"lstm_4/while/loop_counter:output:0(lstm_4/while/maximum_iterations:output:0lstm_4/time:output:0lstm_4/TensorArrayV2_1:handle:0lstm_4/ReadVariableOp:value:0lstm_4/ReadVariableOp_1:value:0lstm_4/strided_slice:output:0>lstm_4/TensorArrayUnstack/TensorListFromTensor:output_handle:01lstm_4_lstm_cell_6_matmul_readvariableop_resource5lstm_4_lstm_cell_6_matmul_1_readvariableop_1_resource2lstm_4_lstm_cell_6_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *#
bodyR
lstm_4_while_body_39402*#
condR
lstm_4_while_cond_39401*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
lstm_4/while?
7lstm_4/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7lstm_4/TensorArrayV2Stack/TensorListStack/element_shape?
)lstm_4/TensorArrayV2Stack/TensorListStackTensorListStacklstm_4/while:output:3@lstm_4/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
element_dtype02+
)lstm_4/TensorArrayV2Stack/TensorListStack?
lstm_4/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
lstm_4/strided_slice_2/stack?
lstm_4/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2 
lstm_4/strided_slice_2/stack_1?
lstm_4/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_4/strided_slice_2/stack_2?
lstm_4/strided_slice_2StridedSlice2lstm_4/TensorArrayV2Stack/TensorListStack:tensor:0%lstm_4/strided_slice_2/stack:output:0'lstm_4/strided_slice_2/stack_1:output:0'lstm_4/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2
lstm_4/strided_slice_2?
lstm_4/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_4/transpose_1/perm?
lstm_4/transpose_1	Transpose2lstm_4/TensorArrayV2Stack/TensorListStack:tensor:0 lstm_4/transpose_1/perm:output:0*
T0*"
_output_shapes
:2
22
lstm_4/transpose_1t
lstm_4/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_4/runtime?
lstm_4/AssignVariableOpAssignVariableOp3lstm_4_lstm_cell_6_matmul_1_readvariableop_resourcelstm_4/while:output:4^lstm_4/ReadVariableOp+^lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_4/AssignVariableOp?
lstm_4/AssignVariableOp_1AssignVariableOp0lstm_4_lstm_cell_6_mul_2_readvariableop_resourcelstm_4/while:output:5^lstm_4/ReadVariableOp_1(^lstm_4/lstm_cell_6/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_4/AssignVariableOp_1?
 time_distributed_2/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2"
 time_distributed_2/Reshape/shape?
time_distributed_2/ReshapeReshapelstm_4/transpose_1:y:0)time_distributed_2/Reshape/shape:output:0*
T0*
_output_shapes
:	?22
time_distributed_2/Reshape?
0time_distributed_2/dense_2/MatMul/ReadVariableOpReadVariableOp9time_distributed_2_dense_2_matmul_readvariableop_resource*
_output_shapes

:2*
dtype022
0time_distributed_2/dense_2/MatMul/ReadVariableOp?
!time_distributed_2/dense_2/MatMulMatMul#time_distributed_2/Reshape:output:08time_distributed_2/dense_2/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2#
!time_distributed_2/dense_2/MatMul?
1time_distributed_2/dense_2/BiasAdd/ReadVariableOpReadVariableOp:time_distributed_2_dense_2_biasadd_readvariableop_resource*
_output_shapes
:*
dtype023
1time_distributed_2/dense_2/BiasAdd/ReadVariableOp?
"time_distributed_2/dense_2/BiasAddBiasAdd+time_distributed_2/dense_2/MatMul:product:09time_distributed_2/dense_2/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2$
"time_distributed_2/dense_2/BiasAdd?
"time_distributed_2/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      2$
"time_distributed_2/Reshape_1/shape?
time_distributed_2/Reshape_1Reshape+time_distributed_2/dense_2/BiasAdd:output:0+time_distributed_2/Reshape_1/shape:output:0*
T0*"
_output_shapes
:2
2
time_distributed_2/Reshape_1?
"time_distributed_2/Reshape_2/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2$
"time_distributed_2/Reshape_2/shape?
time_distributed_2/Reshape_2Reshapelstm_4/transpose_1:y:0+time_distributed_2/Reshape_2/shape:output:0*
T0*
_output_shapes
:	?22
time_distributed_2/Reshape_2{
IdentityIdentity%time_distributed_2/Reshape_1:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity?
NoOpNoOp^lstm_4/AssignVariableOp^lstm_4/AssignVariableOp_1^lstm_4/ReadVariableOp^lstm_4/ReadVariableOp_1*^lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp)^lstm_4/lstm_cell_6/MatMul/ReadVariableOp+^lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp-^lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1(^lstm_4/lstm_cell_6/mul_2/ReadVariableOp^lstm_4/while^lstm_5/AssignVariableOp^lstm_5/AssignVariableOp_1^lstm_5/ReadVariableOp^lstm_5/ReadVariableOp_1*^lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp)^lstm_5/lstm_cell_7/MatMul/ReadVariableOp+^lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp-^lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1(^lstm_5/lstm_cell_7/mul_2/ReadVariableOp^lstm_5/while2^time_distributed_2/dense_2/BiasAdd/ReadVariableOp1^time_distributed_2/dense_2/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 22
lstm_4/AssignVariableOplstm_4/AssignVariableOp26
lstm_4/AssignVariableOp_1lstm_4/AssignVariableOp_12.
lstm_4/ReadVariableOplstm_4/ReadVariableOp22
lstm_4/ReadVariableOp_1lstm_4/ReadVariableOp_12V
)lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp)lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp2T
(lstm_4/lstm_cell_6/MatMul/ReadVariableOp(lstm_4/lstm_cell_6/MatMul/ReadVariableOp2X
*lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp*lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp2\
,lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1,lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_12R
'lstm_4/lstm_cell_6/mul_2/ReadVariableOp'lstm_4/lstm_cell_6/mul_2/ReadVariableOp2
lstm_4/whilelstm_4/while22
lstm_5/AssignVariableOplstm_5/AssignVariableOp26
lstm_5/AssignVariableOp_1lstm_5/AssignVariableOp_12.
lstm_5/ReadVariableOplstm_5/ReadVariableOp22
lstm_5/ReadVariableOp_1lstm_5/ReadVariableOp_12V
)lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp)lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp2T
(lstm_5/lstm_cell_7/MatMul/ReadVariableOp(lstm_5/lstm_cell_7/MatMul/ReadVariableOp2X
*lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp*lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp2\
,lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1,lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_12R
'lstm_5/lstm_cell_7/mul_2/ReadVariableOp'lstm_5/lstm_cell_7/mul_2/ReadVariableOp2
lstm_5/whilelstm_5/while2f
1time_distributed_2/dense_2/BiasAdd/ReadVariableOp1time_distributed_2/dense_2/BiasAdd/ReadVariableOp2d
0time_distributed_2/dense_2/MatMul/ReadVariableOp0time_distributed_2/dense_2/MatMul/ReadVariableOp:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
?X
?
while_body_40370
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_7_matmul_readvariableop_resource_0:	?G
4while_lstm_cell_7_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_7_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_7_matmul_readvariableop_resource:	?E
2while_lstm_cell_7_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_7_biasadd_readvariableop_resource:	???(while/lstm_cell_7/BiasAdd/ReadVariableOp?'while/lstm_cell_7/MatMul/ReadVariableOp?)while/lstm_cell_7/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_7/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_7_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02)
'while/lstm_cell_7/MatMul/ReadVariableOp?
while/lstm_cell_7/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/MatMul?
)while/lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_7_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_7/MatMul_1/ReadVariableOp?
while/lstm_cell_7/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_7/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/MatMul_1?
while/lstm_cell_7/addAddV2"while/lstm_cell_7/MatMul:product:0$while/lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/add?
(while/lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_7_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_7/BiasAdd/ReadVariableOp?
while/lstm_cell_7/BiasAddBiasAddwhile/lstm_cell_7/add:z:00while/lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/BiasAdd?
!while/lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_7/split/split_dim?
while/lstm_cell_7/splitSplit*while/lstm_cell_7/split/split_dim:output:0"while/lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_7/splitw
while/lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const{
while/lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_1?
while/lstm_cell_7/MulMul while/lstm_cell_7/split:output:0 while/lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul?
while/lstm_cell_7/Add_1AddV2while/lstm_cell_7/Mul:z:0"while/lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_1?
)while/lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_7/clip_by_value/Minimum/y?
'while/lstm_cell_7/clip_by_value/MinimumMinimumwhile/lstm_cell_7/Add_1:z:02while/lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222)
'while/lstm_cell_7/clip_by_value/Minimum?
!while/lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_7/clip_by_value/y?
while/lstm_cell_7/clip_by_valueMaximum+while/lstm_cell_7/clip_by_value/Minimum:z:0*while/lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222!
while/lstm_cell_7/clip_by_value{
while/lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const_2{
while/lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_3?
while/lstm_cell_7/Mul_1Mul while/lstm_cell_7/split:output:1"while/lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul_1?
while/lstm_cell_7/Add_2AddV2while/lstm_cell_7/Mul_1:z:0"while/lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_2?
+while/lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_7/clip_by_value_1/Minimum/y?
)while/lstm_cell_7/clip_by_value_1/MinimumMinimumwhile/lstm_cell_7/Add_2:z:04while/lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_7/clip_by_value_1/Minimum?
#while/lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_7/clip_by_value_1/y?
!while/lstm_cell_7/clip_by_value_1Maximum-while/lstm_cell_7/clip_by_value_1/Minimum:z:0,while/lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_7/clip_by_value_1?
while/lstm_cell_7/mul_2Mul%while/lstm_cell_7/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_2?
while/lstm_cell_7/TanhTanh while/lstm_cell_7/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_7/Tanh?
while/lstm_cell_7/mul_3Mul#while/lstm_cell_7/clip_by_value:z:0while/lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_3?
while/lstm_cell_7/add_3AddV2while/lstm_cell_7/mul_2:z:0while/lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_7/add_3{
while/lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const_4{
while/lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_5?
while/lstm_cell_7/Mul_4Mul while/lstm_cell_7/split:output:3"while/lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul_4?
while/lstm_cell_7/Add_4AddV2while/lstm_cell_7/Mul_4:z:0"while/lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_4?
+while/lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_7/clip_by_value_2/Minimum/y?
)while/lstm_cell_7/clip_by_value_2/MinimumMinimumwhile/lstm_cell_7/Add_4:z:04while/lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_7/clip_by_value_2/Minimum?
#while/lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_7/clip_by_value_2/y?
!while/lstm_cell_7/clip_by_value_2Maximum-while/lstm_cell_7/clip_by_value_2/Minimum:z:0,while/lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_7/clip_by_value_2?
while/lstm_cell_7/Tanh_1Tanhwhile/lstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Tanh_1?
while/lstm_cell_7/mul_5Mul%while/lstm_cell_7/clip_by_value_2:z:0while/lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_7/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_7/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_7/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_7/BiasAdd/ReadVariableOp(^while/lstm_cell_7/MatMul/ReadVariableOp*^while/lstm_cell_7/MatMul_1/ReadVariableOp*"
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
1while_lstm_cell_7_biasadd_readvariableop_resource3while_lstm_cell_7_biasadd_readvariableop_resource_0"j
2while_lstm_cell_7_matmul_1_readvariableop_resource4while_lstm_cell_7_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_7_matmul_readvariableop_resource2while_lstm_cell_7_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2T
(while/lstm_cell_7/BiasAdd/ReadVariableOp(while/lstm_cell_7/BiasAdd/ReadVariableOp2R
'while/lstm_cell_7/MatMul/ReadVariableOp'while/lstm_cell_7/MatMul/ReadVariableOp2V
)while/lstm_cell_7/MatMul_1/ReadVariableOp)while/lstm_cell_7/MatMul_1/ReadVariableOp: 
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
?	
?
lstm_4_while_cond_39401*
&lstm_4_while_lstm_4_while_loop_counter0
,lstm_4_while_lstm_4_while_maximum_iterations
lstm_4_while_placeholder
lstm_4_while_placeholder_1
lstm_4_while_placeholder_2
lstm_4_while_placeholder_3*
&lstm_4_while_less_lstm_4_strided_sliceA
=lstm_4_while_lstm_4_while_cond_39401___redundant_placeholder0A
=lstm_4_while_lstm_4_while_cond_39401___redundant_placeholder1A
=lstm_4_while_lstm_4_while_cond_39401___redundant_placeholder2A
=lstm_4_while_lstm_4_while_cond_39401___redundant_placeholder3
lstm_4_while_identity
?
lstm_4/while/LessLesslstm_4_while_placeholder&lstm_4_while_less_lstm_4_strided_slice*
T0*
_output_shapes
: 2
lstm_4/while/Lessr
lstm_4/while/IdentityIdentitylstm_4/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_4/while/Identity"7
lstm_4_while_identitylstm_4/while/Identity:output:0*(
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
?m
?
A__inference_lstm_5_layer_call_and_return_conditional_losses_38919

inputs=
*lstm_cell_7_matmul_readvariableop_resource:	?>
,lstm_cell_7_matmul_1_readvariableop_resource:22A
.lstm_cell_7_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_7_biasadd_readvariableop_resource:	?;
)lstm_cell_7_mul_2_readvariableop_resource:22
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_7/BiasAdd/ReadVariableOp?!lstm_cell_7/MatMul/ReadVariableOp?#lstm_cell_7/MatMul_1/ReadVariableOp?%lstm_cell_7/MatMul_1/ReadVariableOp_1? lstm_cell_7/mul_2/ReadVariableOp?whileu
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_7/MatMul/ReadVariableOpReadVariableOp*lstm_cell_7_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02#
!lstm_cell_7/MatMul/ReadVariableOp?
lstm_cell_7/MatMulMatMulstrided_slice_1:output:0)lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/MatMul?
#lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#lstm_cell_7/MatMul_1/ReadVariableOp?
%lstm_cell_7/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_7_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_7/MatMul_1/ReadVariableOp_1?
lstm_cell_7/MatMul_1MatMul+lstm_cell_7/MatMul_1/ReadVariableOp:value:0-lstm_cell_7/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/MatMul_1?
lstm_cell_7/addAddV2lstm_cell_7/MatMul:product:0lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/add?
"lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_7_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_7/BiasAdd/ReadVariableOp?
lstm_cell_7/BiasAddBiasAddlstm_cell_7/add:z:0*lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/BiasAdd|
lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_7/split/split_dim?
lstm_cell_7/splitSplit$lstm_cell_7/split/split_dim:output:0lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_7/splitk
lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Consto
lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_1?
lstm_cell_7/MulMullstm_cell_7/split:output:0lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul?
lstm_cell_7/Add_1AddV2lstm_cell_7/Mul:z:0lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_1?
#lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_7/clip_by_value/Minimum/y?
!lstm_cell_7/clip_by_value/MinimumMinimumlstm_cell_7/Add_1:z:0,lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222#
!lstm_cell_7/clip_by_value/Minimum
lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value/y?
lstm_cell_7/clip_by_valueMaximum%lstm_cell_7/clip_by_value/Minimum:z:0$lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_valueo
lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Const_2o
lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_3?
lstm_cell_7/Mul_1Mullstm_cell_7/split:output:1lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul_1?
lstm_cell_7/Add_2AddV2lstm_cell_7/Mul_1:z:0lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_2?
%lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_7/clip_by_value_1/Minimum/y?
#lstm_cell_7/clip_by_value_1/MinimumMinimumlstm_cell_7/Add_2:z:0.lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_7/clip_by_value_1/Minimum?
lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value_1/y?
lstm_cell_7/clip_by_value_1Maximum'lstm_cell_7/clip_by_value_1/Minimum:z:0&lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_value_1?
 lstm_cell_7/mul_2/ReadVariableOpReadVariableOp)lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02"
 lstm_cell_7/mul_2/ReadVariableOp?
lstm_cell_7/mul_2Mullstm_cell_7/clip_by_value_1:z:0(lstm_cell_7/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_2q
lstm_cell_7/TanhTanhlstm_cell_7/split:output:2*
T0*
_output_shapes

:222
lstm_cell_7/Tanh?
lstm_cell_7/mul_3Mullstm_cell_7/clip_by_value:z:0lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_3?
lstm_cell_7/add_3AddV2lstm_cell_7/mul_2:z:0lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_7/add_3o
lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Const_4o
lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_5?
lstm_cell_7/Mul_4Mullstm_cell_7/split:output:3lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul_4?
lstm_cell_7/Add_4AddV2lstm_cell_7/Mul_4:z:0lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_4?
%lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_7/clip_by_value_2/Minimum/y?
#lstm_cell_7/clip_by_value_2/MinimumMinimumlstm_cell_7/Add_4:z:0.lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_7/clip_by_value_2/Minimum?
lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value_2/y?
lstm_cell_7/clip_by_value_2Maximum'lstm_cell_7/clip_by_value_2/Minimum:z:0&lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_value_2p
lstm_cell_7/Tanh_1Tanhlstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_7/Tanh_1?
lstm_cell_7/mul_5Mullstm_cell_7/clip_by_value_2:z:0lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp,lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_7_matmul_readvariableop_resource.lstm_cell_7_matmul_1_readvariableop_1_resource+lstm_cell_7_biasadd_readvariableop_resource*
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
while_body_38814*
condR
while_cond_38813*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_7_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_7/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_7_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_7/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_7/BiasAdd/ReadVariableOp"^lstm_cell_7/MatMul/ReadVariableOp$^lstm_cell_7/MatMul_1/ReadVariableOp&^lstm_cell_7/MatMul_1/ReadVariableOp_1!^lstm_cell_7/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_7/BiasAdd/ReadVariableOp"lstm_cell_7/BiasAdd/ReadVariableOp2F
!lstm_cell_7/MatMul/ReadVariableOp!lstm_cell_7/MatMul/ReadVariableOp2J
#lstm_cell_7/MatMul_1/ReadVariableOp#lstm_cell_7/MatMul_1/ReadVariableOp2N
%lstm_cell_7/MatMul_1/ReadVariableOp_1%lstm_cell_7/MatMul_1/ReadVariableOp_12D
 lstm_cell_7/mul_2/ReadVariableOp lstm_cell_7/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
?
?
while_cond_41141
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_41141___redundant_placeholder03
/while_while_cond_41141___redundant_placeholder13
/while_while_cond_41141___redundant_placeholder23
/while_while_cond_41141___redundant_placeholder3
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
?

?
#__inference_signature_wrapper_39155
lstm_5_input
unknown:	?
	unknown_0:22
	unknown_1:	2?
	unknown_2:	?
	unknown_3:22
	unknown_4:	2?
	unknown_5:22
	unknown_6:	2?
	unknown_7:	?
	unknown_8:22
	unknown_9:2

unknown_10:
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCalllstm_5_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
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
GPU 2J 8? *)
f$R"
 __inference__wrapped_model_363592
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
StatefulPartitionedCallStatefulPartitionedCall:P L
"
_output_shapes
:2

&
_user_specified_namelstm_5_input
?,
?
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41754

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
:	2?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2	
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
(:22:22:22:22*
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
 *  ??2
clip_by_value/Minimum/y?
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
clip_by_value/y?
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
 *  ??2
clip_by_value_1/Minimum/y?
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
clip_by_value_1/y?
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
 *  ??2
clip_by_value_2/Minimum/y?
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
clip_by_value_2/y?
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
?
?
G__inference_sequential_2_layer_call_and_return_conditional_losses_39085
lstm_5_input
lstm_5_39055:	?
lstm_5_39057:22
lstm_5_39059:	2?
lstm_5_39061:	?
lstm_5_39063:22
lstm_4_39066:	2?
lstm_4_39068:22
lstm_4_39070:	2?
lstm_4_39072:	?
lstm_4_39074:22*
time_distributed_2_39077:2&
time_distributed_2_39079:
identity??lstm_4/StatefulPartitionedCall?lstm_5/StatefulPartitionedCall?*time_distributed_2/StatefulPartitionedCall?
lstm_5/StatefulPartitionedCallStatefulPartitionedCalllstm_5_inputlstm_5_39055lstm_5_39057lstm_5_39059lstm_5_39061lstm_5_39063*
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
GPU 2J 8? *J
fERC
A__inference_lstm_5_layer_call_and_return_conditional_losses_382342 
lstm_5/StatefulPartitionedCall?
lstm_4/StatefulPartitionedCallStatefulPartitionedCall'lstm_5/StatefulPartitionedCall:output:0lstm_4_39066lstm_4_39068lstm_4_39070lstm_4_39072lstm_4_39074*
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
GPU 2J 8? *J
fERC
A__inference_lstm_4_layer_call_and_return_conditional_losses_384232 
lstm_4/StatefulPartitionedCall?
*time_distributed_2/StatefulPartitionedCallStatefulPartitionedCall'lstm_4/StatefulPartitionedCall:output:0time_distributed_2_39077time_distributed_2_39079*
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
GPU 2J 8? *V
fQRO
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_384482,
*time_distributed_2/StatefulPartitionedCall?
 time_distributed_2/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2"
 time_distributed_2/Reshape/shape?
time_distributed_2/ReshapeReshape'lstm_4/StatefulPartitionedCall:output:0)time_distributed_2/Reshape/shape:output:0*
T0*
_output_shapes
:	?22
time_distributed_2/Reshape?
IdentityIdentity3time_distributed_2/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity?
NoOpNoOp^lstm_4/StatefulPartitionedCall^lstm_5/StatefulPartitionedCall+^time_distributed_2/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 2@
lstm_4/StatefulPartitionedCalllstm_4/StatefulPartitionedCall2@
lstm_5/StatefulPartitionedCalllstm_5/StatefulPartitionedCall2X
*time_distributed_2/StatefulPartitionedCall*time_distributed_2/StatefulPartitionedCall:P L
"
_output_shapes
:2

&
_user_specified_namelstm_5_input
?
?
,__inference_sequential_2_layer_call_fn_39912

inputs
unknown:	?
	unknown_0:22
	unknown_1:	2?
	unknown_2:	?
	unknown_3:22
	unknown_4:	2?
	unknown_5:22
	unknown_6:	2?
	unknown_7:	?
	unknown_8:22
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
:2
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_sequential_2_layer_call_and_return_conditional_losses_384572
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
?m
?
A__inference_lstm_5_layer_call_and_return_conditional_losses_40653

inputs=
*lstm_cell_7_matmul_readvariableop_resource:	?>
,lstm_cell_7_matmul_1_readvariableop_resource:22A
.lstm_cell_7_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_7_biasadd_readvariableop_resource:	?;
)lstm_cell_7_mul_2_readvariableop_resource:22
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_7/BiasAdd/ReadVariableOp?!lstm_cell_7/MatMul/ReadVariableOp?#lstm_cell_7/MatMul_1/ReadVariableOp?%lstm_cell_7/MatMul_1/ReadVariableOp_1? lstm_cell_7/mul_2/ReadVariableOp?whileu
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_7/MatMul/ReadVariableOpReadVariableOp*lstm_cell_7_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02#
!lstm_cell_7/MatMul/ReadVariableOp?
lstm_cell_7/MatMulMatMulstrided_slice_1:output:0)lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/MatMul?
#lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#lstm_cell_7/MatMul_1/ReadVariableOp?
%lstm_cell_7/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_7_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_7/MatMul_1/ReadVariableOp_1?
lstm_cell_7/MatMul_1MatMul+lstm_cell_7/MatMul_1/ReadVariableOp:value:0-lstm_cell_7/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/MatMul_1?
lstm_cell_7/addAddV2lstm_cell_7/MatMul:product:0lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/add?
"lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_7_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_7/BiasAdd/ReadVariableOp?
lstm_cell_7/BiasAddBiasAddlstm_cell_7/add:z:0*lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/BiasAdd|
lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_7/split/split_dim?
lstm_cell_7/splitSplit$lstm_cell_7/split/split_dim:output:0lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_7/splitk
lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Consto
lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_1?
lstm_cell_7/MulMullstm_cell_7/split:output:0lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul?
lstm_cell_7/Add_1AddV2lstm_cell_7/Mul:z:0lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_1?
#lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_7/clip_by_value/Minimum/y?
!lstm_cell_7/clip_by_value/MinimumMinimumlstm_cell_7/Add_1:z:0,lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222#
!lstm_cell_7/clip_by_value/Minimum
lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value/y?
lstm_cell_7/clip_by_valueMaximum%lstm_cell_7/clip_by_value/Minimum:z:0$lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_valueo
lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Const_2o
lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_3?
lstm_cell_7/Mul_1Mullstm_cell_7/split:output:1lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul_1?
lstm_cell_7/Add_2AddV2lstm_cell_7/Mul_1:z:0lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_2?
%lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_7/clip_by_value_1/Minimum/y?
#lstm_cell_7/clip_by_value_1/MinimumMinimumlstm_cell_7/Add_2:z:0.lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_7/clip_by_value_1/Minimum?
lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value_1/y?
lstm_cell_7/clip_by_value_1Maximum'lstm_cell_7/clip_by_value_1/Minimum:z:0&lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_value_1?
 lstm_cell_7/mul_2/ReadVariableOpReadVariableOp)lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02"
 lstm_cell_7/mul_2/ReadVariableOp?
lstm_cell_7/mul_2Mullstm_cell_7/clip_by_value_1:z:0(lstm_cell_7/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_2q
lstm_cell_7/TanhTanhlstm_cell_7/split:output:2*
T0*
_output_shapes

:222
lstm_cell_7/Tanh?
lstm_cell_7/mul_3Mullstm_cell_7/clip_by_value:z:0lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_3?
lstm_cell_7/add_3AddV2lstm_cell_7/mul_2:z:0lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_7/add_3o
lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Const_4o
lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_5?
lstm_cell_7/Mul_4Mullstm_cell_7/split:output:3lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul_4?
lstm_cell_7/Add_4AddV2lstm_cell_7/Mul_4:z:0lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_4?
%lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_7/clip_by_value_2/Minimum/y?
#lstm_cell_7/clip_by_value_2/MinimumMinimumlstm_cell_7/Add_4:z:0.lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_7/clip_by_value_2/Minimum?
lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value_2/y?
lstm_cell_7/clip_by_value_2Maximum'lstm_cell_7/clip_by_value_2/Minimum:z:0&lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_value_2p
lstm_cell_7/Tanh_1Tanhlstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_7/Tanh_1?
lstm_cell_7/mul_5Mullstm_cell_7/clip_by_value_2:z:0lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp,lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_7_matmul_readvariableop_resource.lstm_cell_7_matmul_1_readvariableop_1_resource+lstm_cell_7_biasadd_readvariableop_resource*
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
while_body_40548*
condR
while_cond_40547*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_7_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_7/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_7_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_7/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_7/BiasAdd/ReadVariableOp"^lstm_cell_7/MatMul/ReadVariableOp$^lstm_cell_7/MatMul_1/ReadVariableOp&^lstm_cell_7/MatMul_1/ReadVariableOp_1!^lstm_cell_7/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_7/BiasAdd/ReadVariableOp"lstm_cell_7/BiasAdd/ReadVariableOp2F
!lstm_cell_7/MatMul/ReadVariableOp!lstm_cell_7/MatMul/ReadVariableOp2J
#lstm_cell_7/MatMul_1/ReadVariableOp#lstm_cell_7/MatMul_1/ReadVariableOp2N
%lstm_cell_7/MatMul_1/ReadVariableOp_1%lstm_cell_7/MatMul_1/ReadVariableOp_12D
 lstm_cell_7/mul_2/ReadVariableOp lstm_cell_7/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
?
?
while_cond_36825
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_36825___redundant_placeholder03
/while_while_cond_36825___redundant_placeholder13
/while_while_cond_36825___redundant_placeholder23
/while_while_cond_36825___redundant_placeholder3
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
?~
?
$sequential_2_lstm_4_while_body_36242D
@sequential_2_lstm_4_while_sequential_2_lstm_4_while_loop_counterJ
Fsequential_2_lstm_4_while_sequential_2_lstm_4_while_maximum_iterations)
%sequential_2_lstm_4_while_placeholder+
'sequential_2_lstm_4_while_placeholder_1+
'sequential_2_lstm_4_while_placeholder_2+
'sequential_2_lstm_4_while_placeholder_3A
=sequential_2_lstm_4_while_sequential_2_lstm_4_strided_slice_0
{sequential_2_lstm_4_while_tensorarrayv2read_tensorlistgetitem_sequential_2_lstm_4_tensorarrayunstack_tensorlistfromtensor_0Y
Fsequential_2_lstm_4_while_lstm_cell_6_matmul_readvariableop_resource_0:	2?[
Hsequential_2_lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource_0:	2?V
Gsequential_2_lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource_0:	?&
"sequential_2_lstm_4_while_identity(
$sequential_2_lstm_4_while_identity_1(
$sequential_2_lstm_4_while_identity_2(
$sequential_2_lstm_4_while_identity_3(
$sequential_2_lstm_4_while_identity_4(
$sequential_2_lstm_4_while_identity_5?
;sequential_2_lstm_4_while_sequential_2_lstm_4_strided_slice}
ysequential_2_lstm_4_while_tensorarrayv2read_tensorlistgetitem_sequential_2_lstm_4_tensorarrayunstack_tensorlistfromtensorW
Dsequential_2_lstm_4_while_lstm_cell_6_matmul_readvariableop_resource:	2?Y
Fsequential_2_lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource:	2?T
Esequential_2_lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource:	???<sequential_2/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp?;sequential_2/lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp?=sequential_2/lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp?
Ksequential_2/lstm_4/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2M
Ksequential_2/lstm_4/while/TensorArrayV2Read/TensorListGetItem/element_shape?
=sequential_2/lstm_4/while/TensorArrayV2Read/TensorListGetItemTensorListGetItem{sequential_2_lstm_4_while_tensorarrayv2read_tensorlistgetitem_sequential_2_lstm_4_tensorarrayunstack_tensorlistfromtensor_0%sequential_2_lstm_4_while_placeholderTsequential_2/lstm_4/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02?
=sequential_2/lstm_4/while/TensorArrayV2Read/TensorListGetItem?
;sequential_2/lstm_4/while/lstm_cell_6/MatMul/ReadVariableOpReadVariableOpFsequential_2_lstm_4_while_lstm_cell_6_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02=
;sequential_2/lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp?
,sequential_2/lstm_4/while/lstm_cell_6/MatMulMatMulDsequential_2/lstm_4/while/TensorArrayV2Read/TensorListGetItem:item:0Csequential_2/lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2.
,sequential_2/lstm_4/while/lstm_cell_6/MatMul?
=sequential_2/lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOpHsequential_2_lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02?
=sequential_2/lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp?
.sequential_2/lstm_4/while/lstm_cell_6/MatMul_1MatMul'sequential_2_lstm_4_while_placeholder_2Esequential_2/lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?20
.sequential_2/lstm_4/while/lstm_cell_6/MatMul_1?
)sequential_2/lstm_4/while/lstm_cell_6/addAddV26sequential_2/lstm_4/while/lstm_cell_6/MatMul:product:08sequential_2/lstm_4/while/lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2+
)sequential_2/lstm_4/while/lstm_cell_6/add?
<sequential_2/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOpGsequential_2_lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02>
<sequential_2/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp?
-sequential_2/lstm_4/while/lstm_cell_6/BiasAddBiasAdd-sequential_2/lstm_4/while/lstm_cell_6/add:z:0Dsequential_2/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2/
-sequential_2/lstm_4/while/lstm_cell_6/BiasAdd?
5sequential_2/lstm_4/while/lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :27
5sequential_2/lstm_4/while/lstm_cell_6/split/split_dim?
+sequential_2/lstm_4/while/lstm_cell_6/splitSplit>sequential_2/lstm_4/while/lstm_cell_6/split/split_dim:output:06sequential_2/lstm_4/while/lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2-
+sequential_2/lstm_4/while/lstm_cell_6/split?
+sequential_2/lstm_4/while/lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2-
+sequential_2/lstm_4/while/lstm_cell_6/Const?
-sequential_2/lstm_4/while/lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2/
-sequential_2/lstm_4/while/lstm_cell_6/Const_1?
)sequential_2/lstm_4/while/lstm_cell_6/MulMul4sequential_2/lstm_4/while/lstm_cell_6/split:output:04sequential_2/lstm_4/while/lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222+
)sequential_2/lstm_4/while/lstm_cell_6/Mul?
+sequential_2/lstm_4/while/lstm_cell_6/Add_1AddV2-sequential_2/lstm_4/while/lstm_cell_6/Mul:z:06sequential_2/lstm_4/while/lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_4/while/lstm_cell_6/Add_1?
=sequential_2/lstm_4/while/lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2?
=sequential_2/lstm_4/while/lstm_cell_6/clip_by_value/Minimum/y?
;sequential_2/lstm_4/while/lstm_cell_6/clip_by_value/MinimumMinimum/sequential_2/lstm_4/while/lstm_cell_6/Add_1:z:0Fsequential_2/lstm_4/while/lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222=
;sequential_2/lstm_4/while/lstm_cell_6/clip_by_value/Minimum?
5sequential_2/lstm_4/while/lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    27
5sequential_2/lstm_4/while/lstm_cell_6/clip_by_value/y?
3sequential_2/lstm_4/while/lstm_cell_6/clip_by_valueMaximum?sequential_2/lstm_4/while/lstm_cell_6/clip_by_value/Minimum:z:0>sequential_2/lstm_4/while/lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:2225
3sequential_2/lstm_4/while/lstm_cell_6/clip_by_value?
-sequential_2/lstm_4/while/lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2/
-sequential_2/lstm_4/while/lstm_cell_6/Const_2?
-sequential_2/lstm_4/while/lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2/
-sequential_2/lstm_4/while/lstm_cell_6/Const_3?
+sequential_2/lstm_4/while/lstm_cell_6/Mul_1Mul4sequential_2/lstm_4/while/lstm_cell_6/split:output:16sequential_2/lstm_4/while/lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_4/while/lstm_cell_6/Mul_1?
+sequential_2/lstm_4/while/lstm_cell_6/Add_2AddV2/sequential_2/lstm_4/while/lstm_cell_6/Mul_1:z:06sequential_2/lstm_4/while/lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_4/while/lstm_cell_6/Add_2?
?sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2A
?sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum/y?
=sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_1/MinimumMinimum/sequential_2/lstm_4/while/lstm_cell_6/Add_2:z:0Hsequential_2/lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222?
=sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum?
7sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    29
7sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_1/y?
5sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_1MaximumAsequential_2/lstm_4/while/lstm_cell_6/clip_by_value_1/Minimum:z:0@sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:2227
5sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_1?
+sequential_2/lstm_4/while/lstm_cell_6/mul_2Mul9sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_1:z:0'sequential_2_lstm_4_while_placeholder_3*
T0*
_output_shapes

:222-
+sequential_2/lstm_4/while/lstm_cell_6/mul_2?
*sequential_2/lstm_4/while/lstm_cell_6/TanhTanh4sequential_2/lstm_4/while/lstm_cell_6/split:output:2*
T0*
_output_shapes

:222,
*sequential_2/lstm_4/while/lstm_cell_6/Tanh?
+sequential_2/lstm_4/while/lstm_cell_6/mul_3Mul7sequential_2/lstm_4/while/lstm_cell_6/clip_by_value:z:0.sequential_2/lstm_4/while/lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_4/while/lstm_cell_6/mul_3?
+sequential_2/lstm_4/while/lstm_cell_6/add_3AddV2/sequential_2/lstm_4/while/lstm_cell_6/mul_2:z:0/sequential_2/lstm_4/while/lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_4/while/lstm_cell_6/add_3?
-sequential_2/lstm_4/while/lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2/
-sequential_2/lstm_4/while/lstm_cell_6/Const_4?
-sequential_2/lstm_4/while/lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2/
-sequential_2/lstm_4/while/lstm_cell_6/Const_5?
+sequential_2/lstm_4/while/lstm_cell_6/Mul_4Mul4sequential_2/lstm_4/while/lstm_cell_6/split:output:36sequential_2/lstm_4/while/lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_4/while/lstm_cell_6/Mul_4?
+sequential_2/lstm_4/while/lstm_cell_6/Add_4AddV2/sequential_2/lstm_4/while/lstm_cell_6/Mul_4:z:06sequential_2/lstm_4/while/lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_4/while/lstm_cell_6/Add_4?
?sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2A
?sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum/y?
=sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_2/MinimumMinimum/sequential_2/lstm_4/while/lstm_cell_6/Add_4:z:0Hsequential_2/lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222?
=sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum?
7sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    29
7sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_2/y?
5sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_2MaximumAsequential_2/lstm_4/while/lstm_cell_6/clip_by_value_2/Minimum:z:0@sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:2227
5sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_2?
,sequential_2/lstm_4/while/lstm_cell_6/Tanh_1Tanh/sequential_2/lstm_4/while/lstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222.
,sequential_2/lstm_4/while/lstm_cell_6/Tanh_1?
+sequential_2/lstm_4/while/lstm_cell_6/mul_5Mul9sequential_2/lstm_4/while/lstm_cell_6/clip_by_value_2:z:00sequential_2/lstm_4/while/lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_4/while/lstm_cell_6/mul_5?
>sequential_2/lstm_4/while/TensorArrayV2Write/TensorListSetItemTensorListSetItem'sequential_2_lstm_4_while_placeholder_1%sequential_2_lstm_4_while_placeholder/sequential_2/lstm_4/while/lstm_cell_6/mul_5:z:0*
_output_shapes
: *
element_dtype02@
>sequential_2/lstm_4/while/TensorArrayV2Write/TensorListSetItem?
sequential_2/lstm_4/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2!
sequential_2/lstm_4/while/add/y?
sequential_2/lstm_4/while/addAddV2%sequential_2_lstm_4_while_placeholder(sequential_2/lstm_4/while/add/y:output:0*
T0*
_output_shapes
: 2
sequential_2/lstm_4/while/add?
!sequential_2/lstm_4/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2#
!sequential_2/lstm_4/while/add_1/y?
sequential_2/lstm_4/while/add_1AddV2@sequential_2_lstm_4_while_sequential_2_lstm_4_while_loop_counter*sequential_2/lstm_4/while/add_1/y:output:0*
T0*
_output_shapes
: 2!
sequential_2/lstm_4/while/add_1?
"sequential_2/lstm_4/while/IdentityIdentity#sequential_2/lstm_4/while/add_1:z:0^sequential_2/lstm_4/while/NoOp*
T0*
_output_shapes
: 2$
"sequential_2/lstm_4/while/Identity?
$sequential_2/lstm_4/while/Identity_1IdentityFsequential_2_lstm_4_while_sequential_2_lstm_4_while_maximum_iterations^sequential_2/lstm_4/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_2/lstm_4/while/Identity_1?
$sequential_2/lstm_4/while/Identity_2Identity!sequential_2/lstm_4/while/add:z:0^sequential_2/lstm_4/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_2/lstm_4/while/Identity_2?
$sequential_2/lstm_4/while/Identity_3IdentityNsequential_2/lstm_4/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^sequential_2/lstm_4/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_2/lstm_4/while/Identity_3?
$sequential_2/lstm_4/while/Identity_4Identity/sequential_2/lstm_4/while/lstm_cell_6/mul_5:z:0^sequential_2/lstm_4/while/NoOp*
T0*
_output_shapes

:222&
$sequential_2/lstm_4/while/Identity_4?
$sequential_2/lstm_4/while/Identity_5Identity/sequential_2/lstm_4/while/lstm_cell_6/add_3:z:0^sequential_2/lstm_4/while/NoOp*
T0*
_output_shapes

:222&
$sequential_2/lstm_4/while/Identity_5?
sequential_2/lstm_4/while/NoOpNoOp=^sequential_2/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp<^sequential_2/lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp>^sequential_2/lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2 
sequential_2/lstm_4/while/NoOp"Q
"sequential_2_lstm_4_while_identity+sequential_2/lstm_4/while/Identity:output:0"U
$sequential_2_lstm_4_while_identity_1-sequential_2/lstm_4/while/Identity_1:output:0"U
$sequential_2_lstm_4_while_identity_2-sequential_2/lstm_4/while/Identity_2:output:0"U
$sequential_2_lstm_4_while_identity_3-sequential_2/lstm_4/while/Identity_3:output:0"U
$sequential_2_lstm_4_while_identity_4-sequential_2/lstm_4/while/Identity_4:output:0"U
$sequential_2_lstm_4_while_identity_5-sequential_2/lstm_4/while/Identity_5:output:0"?
Esequential_2_lstm_4_while_lstm_cell_6_biasadd_readvariableop_resourceGsequential_2_lstm_4_while_lstm_cell_6_biasadd_readvariableop_resource_0"?
Fsequential_2_lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resourceHsequential_2_lstm_4_while_lstm_cell_6_matmul_1_readvariableop_resource_0"?
Dsequential_2_lstm_4_while_lstm_cell_6_matmul_readvariableop_resourceFsequential_2_lstm_4_while_lstm_cell_6_matmul_readvariableop_resource_0"|
;sequential_2_lstm_4_while_sequential_2_lstm_4_strided_slice=sequential_2_lstm_4_while_sequential_2_lstm_4_strided_slice_0"?
ysequential_2_lstm_4_while_tensorarrayv2read_tensorlistgetitem_sequential_2_lstm_4_tensorarrayunstack_tensorlistfromtensor{sequential_2_lstm_4_while_tensorarrayv2read_tensorlistgetitem_sequential_2_lstm_4_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2|
<sequential_2/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp<sequential_2/lstm_4/while/lstm_cell_6/BiasAdd/ReadVariableOp2z
;sequential_2/lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp;sequential_2/lstm_4/while/lstm_cell_6/MatMul/ReadVariableOp2~
=sequential_2/lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp=sequential_2/lstm_4/while/lstm_cell_6/MatMul_1/ReadVariableOp: 
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
?,
?
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42156

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
:	2?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2	
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
(:22:22:22:22*
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
 *  ??2
clip_by_value/Minimum/y?
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
clip_by_value/y?
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
 *  ??2
clip_by_value_1/Minimum/y?
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
clip_by_value_1/y?
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
 *  ??2
clip_by_value_2/Minimum/y?
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
clip_by_value_2/y?
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
?e
?

lstm_5_while_body_39228*
&lstm_5_while_lstm_5_while_loop_counter0
,lstm_5_while_lstm_5_while_maximum_iterations
lstm_5_while_placeholder
lstm_5_while_placeholder_1
lstm_5_while_placeholder_2
lstm_5_while_placeholder_3'
#lstm_5_while_lstm_5_strided_slice_0e
alstm_5_while_tensorarrayv2read_tensorlistgetitem_lstm_5_tensorarrayunstack_tensorlistfromtensor_0L
9lstm_5_while_lstm_cell_7_matmul_readvariableop_resource_0:	?N
;lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource_0:	2?I
:lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource_0:	?
lstm_5_while_identity
lstm_5_while_identity_1
lstm_5_while_identity_2
lstm_5_while_identity_3
lstm_5_while_identity_4
lstm_5_while_identity_5%
!lstm_5_while_lstm_5_strided_slicec
_lstm_5_while_tensorarrayv2read_tensorlistgetitem_lstm_5_tensorarrayunstack_tensorlistfromtensorJ
7lstm_5_while_lstm_cell_7_matmul_readvariableop_resource:	?L
9lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource:	2?G
8lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource:	???/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp?.lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp?0lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp?
>lstm_5/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      2@
>lstm_5/while/TensorArrayV2Read/TensorListGetItem/element_shape?
0lstm_5/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemalstm_5_while_tensorarrayv2read_tensorlistgetitem_lstm_5_tensorarrayunstack_tensorlistfromtensor_0lstm_5_while_placeholderGlstm_5/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype022
0lstm_5/while/TensorArrayV2Read/TensorListGetItem?
.lstm_5/while/lstm_cell_7/MatMul/ReadVariableOpReadVariableOp9lstm_5_while_lstm_cell_7_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype020
.lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp?
lstm_5/while/lstm_cell_7/MatMulMatMul7lstm_5/while/TensorArrayV2Read/TensorListGetItem:item:06lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2!
lstm_5/while/lstm_cell_7/MatMul?
0lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp;lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype022
0lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp?
!lstm_5/while/lstm_cell_7/MatMul_1MatMullstm_5_while_placeholder_28lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2#
!lstm_5/while/lstm_cell_7/MatMul_1?
lstm_5/while/lstm_cell_7/addAddV2)lstm_5/while/lstm_cell_7/MatMul:product:0+lstm_5/while/lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_5/while/lstm_cell_7/add?
/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp:lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype021
/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp?
 lstm_5/while/lstm_cell_7/BiasAddBiasAdd lstm_5/while/lstm_cell_7/add:z:07lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2"
 lstm_5/while/lstm_cell_7/BiasAdd?
(lstm_5/while/lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2*
(lstm_5/while/lstm_cell_7/split/split_dim?
lstm_5/while/lstm_cell_7/splitSplit1lstm_5/while/lstm_cell_7/split/split_dim:output:0)lstm_5/while/lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2 
lstm_5/while/lstm_cell_7/split?
lstm_5/while/lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2 
lstm_5/while/lstm_cell_7/Const?
 lstm_5/while/lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_5/while/lstm_cell_7/Const_1?
lstm_5/while/lstm_cell_7/MulMul'lstm_5/while/lstm_cell_7/split:output:0'lstm_5/while/lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
lstm_5/while/lstm_cell_7/Mul?
lstm_5/while/lstm_cell_7/Add_1AddV2 lstm_5/while/lstm_cell_7/Mul:z:0)lstm_5/while/lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/Add_1?
0lstm_5/while/lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??22
0lstm_5/while/lstm_cell_7/clip_by_value/Minimum/y?
.lstm_5/while/lstm_cell_7/clip_by_value/MinimumMinimum"lstm_5/while/lstm_cell_7/Add_1:z:09lstm_5/while/lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:2220
.lstm_5/while/lstm_cell_7/clip_by_value/Minimum?
(lstm_5/while/lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2*
(lstm_5/while/lstm_cell_7/clip_by_value/y?
&lstm_5/while/lstm_cell_7/clip_by_valueMaximum2lstm_5/while/lstm_cell_7/clip_by_value/Minimum:z:01lstm_5/while/lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222(
&lstm_5/while/lstm_cell_7/clip_by_value?
 lstm_5/while/lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_5/while/lstm_cell_7/Const_2?
 lstm_5/while/lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_5/while/lstm_cell_7/Const_3?
lstm_5/while/lstm_cell_7/Mul_1Mul'lstm_5/while/lstm_cell_7/split:output:1)lstm_5/while/lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/Mul_1?
lstm_5/while/lstm_cell_7/Add_2AddV2"lstm_5/while/lstm_cell_7/Mul_1:z:0)lstm_5/while/lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/Add_2?
2lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum/y?
0lstm_5/while/lstm_cell_7/clip_by_value_1/MinimumMinimum"lstm_5/while/lstm_cell_7/Add_2:z:0;lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:2222
0lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum?
*lstm_5/while/lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_5/while/lstm_cell_7/clip_by_value_1/y?
(lstm_5/while/lstm_cell_7/clip_by_value_1Maximum4lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum:z:03lstm_5/while/lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222*
(lstm_5/while/lstm_cell_7/clip_by_value_1?
lstm_5/while/lstm_cell_7/mul_2Mul,lstm_5/while/lstm_cell_7/clip_by_value_1:z:0lstm_5_while_placeholder_3*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/mul_2?
lstm_5/while/lstm_cell_7/TanhTanh'lstm_5/while/lstm_cell_7/split:output:2*
T0*
_output_shapes

:222
lstm_5/while/lstm_cell_7/Tanh?
lstm_5/while/lstm_cell_7/mul_3Mul*lstm_5/while/lstm_cell_7/clip_by_value:z:0!lstm_5/while/lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/mul_3?
lstm_5/while/lstm_cell_7/add_3AddV2"lstm_5/while/lstm_cell_7/mul_2:z:0"lstm_5/while/lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/add_3?
 lstm_5/while/lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2"
 lstm_5/while/lstm_cell_7/Const_4?
 lstm_5/while/lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2"
 lstm_5/while/lstm_cell_7/Const_5?
lstm_5/while/lstm_cell_7/Mul_4Mul'lstm_5/while/lstm_cell_7/split:output:3)lstm_5/while/lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/Mul_4?
lstm_5/while/lstm_cell_7/Add_4AddV2"lstm_5/while/lstm_cell_7/Mul_4:z:0)lstm_5/while/lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/Add_4?
2lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??24
2lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum/y?
0lstm_5/while/lstm_cell_7/clip_by_value_2/MinimumMinimum"lstm_5/while/lstm_cell_7/Add_4:z:0;lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:2222
0lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum?
*lstm_5/while/lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2,
*lstm_5/while/lstm_cell_7/clip_by_value_2/y?
(lstm_5/while/lstm_cell_7/clip_by_value_2Maximum4lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum:z:03lstm_5/while/lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222*
(lstm_5/while/lstm_cell_7/clip_by_value_2?
lstm_5/while/lstm_cell_7/Tanh_1Tanh"lstm_5/while/lstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222!
lstm_5/while/lstm_cell_7/Tanh_1?
lstm_5/while/lstm_cell_7/mul_5Mul,lstm_5/while/lstm_cell_7/clip_by_value_2:z:0#lstm_5/while/lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222 
lstm_5/while/lstm_cell_7/mul_5?
1lstm_5/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_5_while_placeholder_1lstm_5_while_placeholder"lstm_5/while/lstm_cell_7/mul_5:z:0*
_output_shapes
: *
element_dtype023
1lstm_5/while/TensorArrayV2Write/TensorListSetItemj
lstm_5/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_5/while/add/y?
lstm_5/while/addAddV2lstm_5_while_placeholderlstm_5/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_5/while/addn
lstm_5/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_5/while/add_1/y?
lstm_5/while/add_1AddV2&lstm_5_while_lstm_5_while_loop_counterlstm_5/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_5/while/add_1?
lstm_5/while/IdentityIdentitylstm_5/while/add_1:z:0^lstm_5/while/NoOp*
T0*
_output_shapes
: 2
lstm_5/while/Identity?
lstm_5/while/Identity_1Identity,lstm_5_while_lstm_5_while_maximum_iterations^lstm_5/while/NoOp*
T0*
_output_shapes
: 2
lstm_5/while/Identity_1?
lstm_5/while/Identity_2Identitylstm_5/while/add:z:0^lstm_5/while/NoOp*
T0*
_output_shapes
: 2
lstm_5/while/Identity_2?
lstm_5/while/Identity_3IdentityAlstm_5/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^lstm_5/while/NoOp*
T0*
_output_shapes
: 2
lstm_5/while/Identity_3?
lstm_5/while/Identity_4Identity"lstm_5/while/lstm_cell_7/mul_5:z:0^lstm_5/while/NoOp*
T0*
_output_shapes

:222
lstm_5/while/Identity_4?
lstm_5/while/Identity_5Identity"lstm_5/while/lstm_cell_7/add_3:z:0^lstm_5/while/NoOp*
T0*
_output_shapes

:222
lstm_5/while/Identity_5?
lstm_5/while/NoOpNoOp0^lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp/^lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp1^lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
lstm_5/while/NoOp"7
lstm_5_while_identitylstm_5/while/Identity:output:0";
lstm_5_while_identity_1 lstm_5/while/Identity_1:output:0";
lstm_5_while_identity_2 lstm_5/while/Identity_2:output:0";
lstm_5_while_identity_3 lstm_5/while/Identity_3:output:0";
lstm_5_while_identity_4 lstm_5/while/Identity_4:output:0";
lstm_5_while_identity_5 lstm_5/while/Identity_5:output:0"H
!lstm_5_while_lstm_5_strided_slice#lstm_5_while_lstm_5_strided_slice_0"v
8lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource:lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource_0"x
9lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource;lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource_0"t
7lstm_5_while_lstm_cell_7_matmul_readvariableop_resource9lstm_5_while_lstm_cell_7_matmul_readvariableop_resource_0"?
_lstm_5_while_tensorarrayv2read_tensorlistgetitem_lstm_5_tensorarrayunstack_tensorlistfromtensoralstm_5_while_tensorarrayv2read_tensorlistgetitem_lstm_5_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2b
/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp2`
.lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp.lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp2d
0lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp0lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp: 
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
?
?
+__inference_lstm_cell_7_layer_call_fn_41845

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
:22:22:22*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_366662
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
?9
?
A__inference_lstm_4_layer_call_and_return_conditional_losses_37354

inputs#
lstm_cell_6_37213:22#
lstm_cell_6_37215:22$
lstm_cell_6_37217:	2?$
lstm_cell_6_37219:	2? 
lstm_cell_6_37221:	?
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?#lstm_cell_6/StatefulPartitionedCall?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:?????????222
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1?
#lstm_cell_6/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_6_37213lstm_cell_6_37215lstm_cell_6_37217lstm_cell_6_37219lstm_cell_6_37221*
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
GPU 2J 8? *O
fJRH
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_372122%
#lstm_cell_6/StatefulPartitionedCall?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOplstm_cell_6_37213*
_output_shapes

:22*
dtype02
ReadVariableOpv
ReadVariableOp_1ReadVariableOplstm_cell_6_37215*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_6_37217lstm_cell_6_37219lstm_cell_6_37221*
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
while_body_37232*
condR
while_cond_37231*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????22*
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

:22*
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
:2?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOplstm_cell_6_37213while:output:4^ReadVariableOp$^lstm_cell_6/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOplstm_cell_6_37215while:output:5^ReadVariableOp_1$^lstm_cell_6/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_6/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2?????????2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_6/StatefulPartitionedCall#lstm_cell_6/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:2?????????2
 
_user_specified_nameinputs
?m
?
A__inference_lstm_4_layer_call_and_return_conditional_losses_40891
inputs_0=
*lstm_cell_6_matmul_readvariableop_resource:	2?>
,lstm_cell_6_matmul_1_readvariableop_resource:22A
.lstm_cell_6_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_6_biasadd_readvariableop_resource:	?;
)lstm_cell_6_mul_2_readvariableop_resource:22
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_6/BiasAdd/ReadVariableOp?!lstm_cell_6/MatMul/ReadVariableOp?#lstm_cell_6/MatMul_1/ReadVariableOp?%lstm_cell_6/MatMul_1/ReadVariableOp_1? lstm_cell_6/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:?????????222
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_6/MatMul/ReadVariableOpReadVariableOp*lstm_cell_6_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02#
!lstm_cell_6/MatMul/ReadVariableOp?
lstm_cell_6/MatMulMatMulstrided_slice_1:output:0)lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/MatMul?
#lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#lstm_cell_6/MatMul_1/ReadVariableOp?
%lstm_cell_6/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_6_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_6/MatMul_1/ReadVariableOp_1?
lstm_cell_6/MatMul_1MatMul+lstm_cell_6/MatMul_1/ReadVariableOp:value:0-lstm_cell_6/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/MatMul_1?
lstm_cell_6/addAddV2lstm_cell_6/MatMul:product:0lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/add?
"lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_6_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_6/BiasAdd/ReadVariableOp?
lstm_cell_6/BiasAddBiasAddlstm_cell_6/add:z:0*lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/BiasAdd|
lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_6/split/split_dim?
lstm_cell_6/splitSplit$lstm_cell_6/split/split_dim:output:0lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_6/splitk
lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Consto
lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_1?
lstm_cell_6/MulMullstm_cell_6/split:output:0lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul?
lstm_cell_6/Add_1AddV2lstm_cell_6/Mul:z:0lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_1?
#lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_6/clip_by_value/Minimum/y?
!lstm_cell_6/clip_by_value/MinimumMinimumlstm_cell_6/Add_1:z:0,lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222#
!lstm_cell_6/clip_by_value/Minimum
lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value/y?
lstm_cell_6/clip_by_valueMaximum%lstm_cell_6/clip_by_value/Minimum:z:0$lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_valueo
lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Const_2o
lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_3?
lstm_cell_6/Mul_1Mullstm_cell_6/split:output:1lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul_1?
lstm_cell_6/Add_2AddV2lstm_cell_6/Mul_1:z:0lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_2?
%lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_6/clip_by_value_1/Minimum/y?
#lstm_cell_6/clip_by_value_1/MinimumMinimumlstm_cell_6/Add_2:z:0.lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_6/clip_by_value_1/Minimum?
lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value_1/y?
lstm_cell_6/clip_by_value_1Maximum'lstm_cell_6/clip_by_value_1/Minimum:z:0&lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_value_1?
 lstm_cell_6/mul_2/ReadVariableOpReadVariableOp)lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02"
 lstm_cell_6/mul_2/ReadVariableOp?
lstm_cell_6/mul_2Mullstm_cell_6/clip_by_value_1:z:0(lstm_cell_6/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_2q
lstm_cell_6/TanhTanhlstm_cell_6/split:output:2*
T0*
_output_shapes

:222
lstm_cell_6/Tanh?
lstm_cell_6/mul_3Mullstm_cell_6/clip_by_value:z:0lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_3?
lstm_cell_6/add_3AddV2lstm_cell_6/mul_2:z:0lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_6/add_3o
lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Const_4o
lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_5?
lstm_cell_6/Mul_4Mullstm_cell_6/split:output:3lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul_4?
lstm_cell_6/Add_4AddV2lstm_cell_6/Mul_4:z:0lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_4?
%lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_6/clip_by_value_2/Minimum/y?
#lstm_cell_6/clip_by_value_2/MinimumMinimumlstm_cell_6/Add_4:z:0.lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_6/clip_by_value_2/Minimum?
lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value_2/y?
lstm_cell_6/clip_by_value_2Maximum'lstm_cell_6/clip_by_value_2/Minimum:z:0&lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_value_2p
lstm_cell_6/Tanh_1Tanhlstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_6/Tanh_1?
lstm_cell_6/mul_5Mullstm_cell_6/clip_by_value_2:z:0lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp,lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_6_matmul_readvariableop_resource.lstm_cell_6_matmul_1_readvariableop_1_resource+lstm_cell_6_biasadd_readvariableop_resource*
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
while_body_40786*
condR
while_cond_40785*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????22*
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

:22*
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
:2?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_6_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_6/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_6_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_6/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_6/BiasAdd/ReadVariableOp"^lstm_cell_6/MatMul/ReadVariableOp$^lstm_cell_6/MatMul_1/ReadVariableOp&^lstm_cell_6/MatMul_1/ReadVariableOp_1!^lstm_cell_6/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2?????????2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_6/BiasAdd/ReadVariableOp"lstm_cell_6/BiasAdd/ReadVariableOp2F
!lstm_cell_6/MatMul/ReadVariableOp!lstm_cell_6/MatMul/ReadVariableOp2J
#lstm_cell_6/MatMul_1/ReadVariableOp#lstm_cell_6/MatMul_1/ReadVariableOp2N
%lstm_cell_6/MatMul_1/ReadVariableOp_1%lstm_cell_6/MatMul_1/ReadVariableOp_12D
 lstm_cell_6/mul_2/ReadVariableOp lstm_cell_6/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:2?????????2
"
_user_specified_name
inputs/0
??
?
!__inference__traced_restore_42669
file_prefix$
assignvariableop_adam_iter:	 (
assignvariableop_1_adam_beta_1: (
assignvariableop_2_adam_beta_2: '
assignvariableop_3_adam_decay: /
%assignvariableop_4_adam_learning_rate: ?
,assignvariableop_5_lstm_5_lstm_cell_7_kernel:	?I
6assignvariableop_6_lstm_5_lstm_cell_7_recurrent_kernel:	2?9
*assignvariableop_7_lstm_5_lstm_cell_7_bias:	??
,assignvariableop_8_lstm_4_lstm_cell_6_kernel:	2?I
6assignvariableop_9_lstm_4_lstm_cell_6_recurrent_kernel:	2?:
+assignvariableop_10_lstm_4_lstm_cell_6_bias:	??
-assignvariableop_11_time_distributed_2_kernel:29
+assignvariableop_12_time_distributed_2_bias:5
#assignvariableop_13_lstm_5_variable:227
%assignvariableop_14_lstm_5_variable_1:225
#assignvariableop_15_lstm_4_variable:227
%assignvariableop_16_lstm_4_variable_1:22#
assignvariableop_17_total: #
assignvariableop_18_count: %
assignvariableop_19_total_1: %
assignvariableop_20_count_1: G
4assignvariableop_21_adam_lstm_5_lstm_cell_7_kernel_m:	?Q
>assignvariableop_22_adam_lstm_5_lstm_cell_7_recurrent_kernel_m:	2?A
2assignvariableop_23_adam_lstm_5_lstm_cell_7_bias_m:	?G
4assignvariableop_24_adam_lstm_4_lstm_cell_6_kernel_m:	2?Q
>assignvariableop_25_adam_lstm_4_lstm_cell_6_recurrent_kernel_m:	2?A
2assignvariableop_26_adam_lstm_4_lstm_cell_6_bias_m:	?F
4assignvariableop_27_adam_time_distributed_2_kernel_m:2@
2assignvariableop_28_adam_time_distributed_2_bias_m:G
4assignvariableop_29_adam_lstm_5_lstm_cell_7_kernel_v:	?Q
>assignvariableop_30_adam_lstm_5_lstm_cell_7_recurrent_kernel_v:	2?A
2assignvariableop_31_adam_lstm_5_lstm_cell_7_bias_v:	?G
4assignvariableop_32_adam_lstm_4_lstm_cell_6_kernel_v:	2?Q
>assignvariableop_33_adam_lstm_4_lstm_cell_6_recurrent_kernel_v:	2?A
2assignvariableop_34_adam_lstm_4_lstm_cell_6_bias_v:	?F
4assignvariableop_35_adam_time_distributed_2_kernel_v:2@
2assignvariableop_36_adam_time_distributed_2_bias_v:
identity_38??AssignVariableOp?AssignVariableOp_1?AssignVariableOp_10?AssignVariableOp_11?AssignVariableOp_12?AssignVariableOp_13?AssignVariableOp_14?AssignVariableOp_15?AssignVariableOp_16?AssignVariableOp_17?AssignVariableOp_18?AssignVariableOp_19?AssignVariableOp_2?AssignVariableOp_20?AssignVariableOp_21?AssignVariableOp_22?AssignVariableOp_23?AssignVariableOp_24?AssignVariableOp_25?AssignVariableOp_26?AssignVariableOp_27?AssignVariableOp_28?AssignVariableOp_29?AssignVariableOp_3?AssignVariableOp_30?AssignVariableOp_31?AssignVariableOp_32?AssignVariableOp_33?AssignVariableOp_34?AssignVariableOp_35?AssignVariableOp_36?AssignVariableOp_4?AssignVariableOp_5?AssignVariableOp_6?AssignVariableOp_7?AssignVariableOp_8?AssignVariableOp_9?
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:&*
dtype0*?
value?B?&B)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB&variables/0/.ATTRIBUTES/VARIABLE_VALUEB&variables/1/.ATTRIBUTES/VARIABLE_VALUEB&variables/2/.ATTRIBUTES/VARIABLE_VALUEB&variables/3/.ATTRIBUTES/VARIABLE_VALUEB&variables/4/.ATTRIBUTES/VARIABLE_VALUEB&variables/5/.ATTRIBUTES/VARIABLE_VALUEB&variables/6/.ATTRIBUTES/VARIABLE_VALUEB&variables/7/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEBBvariables/0/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/1/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/2/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/3/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/4/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/5/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/6/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/7/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/0/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/1/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/2/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/3/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/4/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/5/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/6/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/7/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
RestoreV2/tensor_names?
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:&*
dtype0*_
valueVBT&B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B 2
RestoreV2/shape_and_slices?
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*?
_output_shapes?
?::::::::::::::::::::::::::::::::::::::*4
dtypes*
(2&	2
	RestoreV2g
IdentityIdentityRestoreV2:tensors:0"/device:CPU:0*
T0	*
_output_shapes
:2

Identity?
AssignVariableOpAssignVariableOpassignvariableop_adam_iterIdentity:output:0"/device:CPU:0*
_output_shapes
 *
dtype0	2
AssignVariableOpk

Identity_1IdentityRestoreV2:tensors:1"/device:CPU:0*
T0*
_output_shapes
:2

Identity_1?
AssignVariableOp_1AssignVariableOpassignvariableop_1_adam_beta_1Identity_1:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_1k

Identity_2IdentityRestoreV2:tensors:2"/device:CPU:0*
T0*
_output_shapes
:2

Identity_2?
AssignVariableOp_2AssignVariableOpassignvariableop_2_adam_beta_2Identity_2:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_2k

Identity_3IdentityRestoreV2:tensors:3"/device:CPU:0*
T0*
_output_shapes
:2

Identity_3?
AssignVariableOp_3AssignVariableOpassignvariableop_3_adam_decayIdentity_3:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_3k

Identity_4IdentityRestoreV2:tensors:4"/device:CPU:0*
T0*
_output_shapes
:2

Identity_4?
AssignVariableOp_4AssignVariableOp%assignvariableop_4_adam_learning_rateIdentity_4:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_4k

Identity_5IdentityRestoreV2:tensors:5"/device:CPU:0*
T0*
_output_shapes
:2

Identity_5?
AssignVariableOp_5AssignVariableOp,assignvariableop_5_lstm_5_lstm_cell_7_kernelIdentity_5:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_5k

Identity_6IdentityRestoreV2:tensors:6"/device:CPU:0*
T0*
_output_shapes
:2

Identity_6?
AssignVariableOp_6AssignVariableOp6assignvariableop_6_lstm_5_lstm_cell_7_recurrent_kernelIdentity_6:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_6k

Identity_7IdentityRestoreV2:tensors:7"/device:CPU:0*
T0*
_output_shapes
:2

Identity_7?
AssignVariableOp_7AssignVariableOp*assignvariableop_7_lstm_5_lstm_cell_7_biasIdentity_7:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_7k

Identity_8IdentityRestoreV2:tensors:8"/device:CPU:0*
T0*
_output_shapes
:2

Identity_8?
AssignVariableOp_8AssignVariableOp,assignvariableop_8_lstm_4_lstm_cell_6_kernelIdentity_8:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_8k

Identity_9IdentityRestoreV2:tensors:9"/device:CPU:0*
T0*
_output_shapes
:2

Identity_9?
AssignVariableOp_9AssignVariableOp6assignvariableop_9_lstm_4_lstm_cell_6_recurrent_kernelIdentity_9:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_9n
Identity_10IdentityRestoreV2:tensors:10"/device:CPU:0*
T0*
_output_shapes
:2
Identity_10?
AssignVariableOp_10AssignVariableOp+assignvariableop_10_lstm_4_lstm_cell_6_biasIdentity_10:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_10n
Identity_11IdentityRestoreV2:tensors:11"/device:CPU:0*
T0*
_output_shapes
:2
Identity_11?
AssignVariableOp_11AssignVariableOp-assignvariableop_11_time_distributed_2_kernelIdentity_11:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_11n
Identity_12IdentityRestoreV2:tensors:12"/device:CPU:0*
T0*
_output_shapes
:2
Identity_12?
AssignVariableOp_12AssignVariableOp+assignvariableop_12_time_distributed_2_biasIdentity_12:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_12n
Identity_13IdentityRestoreV2:tensors:13"/device:CPU:0*
T0*
_output_shapes
:2
Identity_13?
AssignVariableOp_13AssignVariableOp#assignvariableop_13_lstm_5_variableIdentity_13:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_13n
Identity_14IdentityRestoreV2:tensors:14"/device:CPU:0*
T0*
_output_shapes
:2
Identity_14?
AssignVariableOp_14AssignVariableOp%assignvariableop_14_lstm_5_variable_1Identity_14:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_14n
Identity_15IdentityRestoreV2:tensors:15"/device:CPU:0*
T0*
_output_shapes
:2
Identity_15?
AssignVariableOp_15AssignVariableOp#assignvariableop_15_lstm_4_variableIdentity_15:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_15n
Identity_16IdentityRestoreV2:tensors:16"/device:CPU:0*
T0*
_output_shapes
:2
Identity_16?
AssignVariableOp_16AssignVariableOp%assignvariableop_16_lstm_4_variable_1Identity_16:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_16n
Identity_17IdentityRestoreV2:tensors:17"/device:CPU:0*
T0*
_output_shapes
:2
Identity_17?
AssignVariableOp_17AssignVariableOpassignvariableop_17_totalIdentity_17:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_17n
Identity_18IdentityRestoreV2:tensors:18"/device:CPU:0*
T0*
_output_shapes
:2
Identity_18?
AssignVariableOp_18AssignVariableOpassignvariableop_18_countIdentity_18:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_18n
Identity_19IdentityRestoreV2:tensors:19"/device:CPU:0*
T0*
_output_shapes
:2
Identity_19?
AssignVariableOp_19AssignVariableOpassignvariableop_19_total_1Identity_19:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_19n
Identity_20IdentityRestoreV2:tensors:20"/device:CPU:0*
T0*
_output_shapes
:2
Identity_20?
AssignVariableOp_20AssignVariableOpassignvariableop_20_count_1Identity_20:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_20n
Identity_21IdentityRestoreV2:tensors:21"/device:CPU:0*
T0*
_output_shapes
:2
Identity_21?
AssignVariableOp_21AssignVariableOp4assignvariableop_21_adam_lstm_5_lstm_cell_7_kernel_mIdentity_21:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_21n
Identity_22IdentityRestoreV2:tensors:22"/device:CPU:0*
T0*
_output_shapes
:2
Identity_22?
AssignVariableOp_22AssignVariableOp>assignvariableop_22_adam_lstm_5_lstm_cell_7_recurrent_kernel_mIdentity_22:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_22n
Identity_23IdentityRestoreV2:tensors:23"/device:CPU:0*
T0*
_output_shapes
:2
Identity_23?
AssignVariableOp_23AssignVariableOp2assignvariableop_23_adam_lstm_5_lstm_cell_7_bias_mIdentity_23:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_23n
Identity_24IdentityRestoreV2:tensors:24"/device:CPU:0*
T0*
_output_shapes
:2
Identity_24?
AssignVariableOp_24AssignVariableOp4assignvariableop_24_adam_lstm_4_lstm_cell_6_kernel_mIdentity_24:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_24n
Identity_25IdentityRestoreV2:tensors:25"/device:CPU:0*
T0*
_output_shapes
:2
Identity_25?
AssignVariableOp_25AssignVariableOp>assignvariableop_25_adam_lstm_4_lstm_cell_6_recurrent_kernel_mIdentity_25:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_25n
Identity_26IdentityRestoreV2:tensors:26"/device:CPU:0*
T0*
_output_shapes
:2
Identity_26?
AssignVariableOp_26AssignVariableOp2assignvariableop_26_adam_lstm_4_lstm_cell_6_bias_mIdentity_26:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_26n
Identity_27IdentityRestoreV2:tensors:27"/device:CPU:0*
T0*
_output_shapes
:2
Identity_27?
AssignVariableOp_27AssignVariableOp4assignvariableop_27_adam_time_distributed_2_kernel_mIdentity_27:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_27n
Identity_28IdentityRestoreV2:tensors:28"/device:CPU:0*
T0*
_output_shapes
:2
Identity_28?
AssignVariableOp_28AssignVariableOp2assignvariableop_28_adam_time_distributed_2_bias_mIdentity_28:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_28n
Identity_29IdentityRestoreV2:tensors:29"/device:CPU:0*
T0*
_output_shapes
:2
Identity_29?
AssignVariableOp_29AssignVariableOp4assignvariableop_29_adam_lstm_5_lstm_cell_7_kernel_vIdentity_29:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_29n
Identity_30IdentityRestoreV2:tensors:30"/device:CPU:0*
T0*
_output_shapes
:2
Identity_30?
AssignVariableOp_30AssignVariableOp>assignvariableop_30_adam_lstm_5_lstm_cell_7_recurrent_kernel_vIdentity_30:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_30n
Identity_31IdentityRestoreV2:tensors:31"/device:CPU:0*
T0*
_output_shapes
:2
Identity_31?
AssignVariableOp_31AssignVariableOp2assignvariableop_31_adam_lstm_5_lstm_cell_7_bias_vIdentity_31:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_31n
Identity_32IdentityRestoreV2:tensors:32"/device:CPU:0*
T0*
_output_shapes
:2
Identity_32?
AssignVariableOp_32AssignVariableOp4assignvariableop_32_adam_lstm_4_lstm_cell_6_kernel_vIdentity_32:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_32n
Identity_33IdentityRestoreV2:tensors:33"/device:CPU:0*
T0*
_output_shapes
:2
Identity_33?
AssignVariableOp_33AssignVariableOp>assignvariableop_33_adam_lstm_4_lstm_cell_6_recurrent_kernel_vIdentity_33:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_33n
Identity_34IdentityRestoreV2:tensors:34"/device:CPU:0*
T0*
_output_shapes
:2
Identity_34?
AssignVariableOp_34AssignVariableOp2assignvariableop_34_adam_lstm_4_lstm_cell_6_bias_vIdentity_34:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_34n
Identity_35IdentityRestoreV2:tensors:35"/device:CPU:0*
T0*
_output_shapes
:2
Identity_35?
AssignVariableOp_35AssignVariableOp4assignvariableop_35_adam_time_distributed_2_kernel_vIdentity_35:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_35n
Identity_36IdentityRestoreV2:tensors:36"/device:CPU:0*
T0*
_output_shapes
:2
Identity_36?
AssignVariableOp_36AssignVariableOp2assignvariableop_36_adam_time_distributed_2_bias_vIdentity_36:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_369
NoOpNoOp"/device:CPU:0*
_output_shapes
 2
NoOp?
Identity_37Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_23^AssignVariableOp_24^AssignVariableOp_25^AssignVariableOp_26^AssignVariableOp_27^AssignVariableOp_28^AssignVariableOp_29^AssignVariableOp_3^AssignVariableOp_30^AssignVariableOp_31^AssignVariableOp_32^AssignVariableOp_33^AssignVariableOp_34^AssignVariableOp_35^AssignVariableOp_36^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9^NoOp"/device:CPU:0*
T0*
_output_shapes
: 2
Identity_37f
Identity_38IdentityIdentity_37:output:0^NoOp_1*
T0*
_output_shapes
: 2
Identity_38?
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
?	
?
&__inference_lstm_5_layer_call_fn_40683
inputs_0
unknown:22
	unknown_0:22
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
:2?????????2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_5_layer_call_and_return_conditional_losses_368952
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:2?????????22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2?????????: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:2?????????
"
_user_specified_name
inputs/0
?	
?
lstm_5_while_cond_39227*
&lstm_5_while_lstm_5_while_loop_counter0
,lstm_5_while_lstm_5_while_maximum_iterations
lstm_5_while_placeholder
lstm_5_while_placeholder_1
lstm_5_while_placeholder_2
lstm_5_while_placeholder_3*
&lstm_5_while_less_lstm_5_strided_sliceA
=lstm_5_while_lstm_5_while_cond_39227___redundant_placeholder0A
=lstm_5_while_lstm_5_while_cond_39227___redundant_placeholder1A
=lstm_5_while_lstm_5_while_cond_39227___redundant_placeholder2A
=lstm_5_while_lstm_5_while_cond_39227___redundant_placeholder3
lstm_5_while_identity
?
lstm_5/while/LessLesslstm_5_while_placeholder&lstm_5_while_less_lstm_5_strided_slice*
T0*
_output_shapes
: 2
lstm_5/while/Lessr
lstm_5/while/IdentityIdentitylstm_5/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_5/while/Identity"7
lstm_5_while_identitylstm_5/while/Identity:output:0*(
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
?
?
&__inference_lstm_4_layer_call_fn_41485

inputs
unknown:	2?
	unknown_0:22
	unknown_1:	2?
	unknown_2:	?
	unknown_3:22
identity??StatefulPartitionedCall?
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
GPU 2J 8? *J
fERC
A__inference_lstm_4_layer_call_and_return_conditional_losses_387112
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
?
?
2__inference_time_distributed_2_layer_call_fn_41573

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
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_379942
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
?
?
2__inference_time_distributed_2_layer_call_fn_41591

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
:2
*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *V
fQRO
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_385092
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
?m
?
A__inference_lstm_4_layer_call_and_return_conditional_losses_38423

inputs=
*lstm_cell_6_matmul_readvariableop_resource:	2?>
,lstm_cell_6_matmul_1_readvariableop_resource:22A
.lstm_cell_6_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_6_biasadd_readvariableop_resource:	?;
)lstm_cell_6_mul_2_readvariableop_resource:22
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_6/BiasAdd/ReadVariableOp?!lstm_cell_6/MatMul/ReadVariableOp?#lstm_cell_6/MatMul_1/ReadVariableOp?%lstm_cell_6/MatMul_1/ReadVariableOp_1? lstm_cell_6/mul_2/ReadVariableOp?whileu
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_6/MatMul/ReadVariableOpReadVariableOp*lstm_cell_6_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02#
!lstm_cell_6/MatMul/ReadVariableOp?
lstm_cell_6/MatMulMatMulstrided_slice_1:output:0)lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/MatMul?
#lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#lstm_cell_6/MatMul_1/ReadVariableOp?
%lstm_cell_6/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_6_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_6/MatMul_1/ReadVariableOp_1?
lstm_cell_6/MatMul_1MatMul+lstm_cell_6/MatMul_1/ReadVariableOp:value:0-lstm_cell_6/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/MatMul_1?
lstm_cell_6/addAddV2lstm_cell_6/MatMul:product:0lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/add?
"lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_6_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_6/BiasAdd/ReadVariableOp?
lstm_cell_6/BiasAddBiasAddlstm_cell_6/add:z:0*lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/BiasAdd|
lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_6/split/split_dim?
lstm_cell_6/splitSplit$lstm_cell_6/split/split_dim:output:0lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_6/splitk
lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Consto
lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_1?
lstm_cell_6/MulMullstm_cell_6/split:output:0lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul?
lstm_cell_6/Add_1AddV2lstm_cell_6/Mul:z:0lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_1?
#lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_6/clip_by_value/Minimum/y?
!lstm_cell_6/clip_by_value/MinimumMinimumlstm_cell_6/Add_1:z:0,lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222#
!lstm_cell_6/clip_by_value/Minimum
lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value/y?
lstm_cell_6/clip_by_valueMaximum%lstm_cell_6/clip_by_value/Minimum:z:0$lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_valueo
lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Const_2o
lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_3?
lstm_cell_6/Mul_1Mullstm_cell_6/split:output:1lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul_1?
lstm_cell_6/Add_2AddV2lstm_cell_6/Mul_1:z:0lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_2?
%lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_6/clip_by_value_1/Minimum/y?
#lstm_cell_6/clip_by_value_1/MinimumMinimumlstm_cell_6/Add_2:z:0.lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_6/clip_by_value_1/Minimum?
lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value_1/y?
lstm_cell_6/clip_by_value_1Maximum'lstm_cell_6/clip_by_value_1/Minimum:z:0&lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_value_1?
 lstm_cell_6/mul_2/ReadVariableOpReadVariableOp)lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02"
 lstm_cell_6/mul_2/ReadVariableOp?
lstm_cell_6/mul_2Mullstm_cell_6/clip_by_value_1:z:0(lstm_cell_6/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_2q
lstm_cell_6/TanhTanhlstm_cell_6/split:output:2*
T0*
_output_shapes

:222
lstm_cell_6/Tanh?
lstm_cell_6/mul_3Mullstm_cell_6/clip_by_value:z:0lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_3?
lstm_cell_6/add_3AddV2lstm_cell_6/mul_2:z:0lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_6/add_3o
lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Const_4o
lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_5?
lstm_cell_6/Mul_4Mullstm_cell_6/split:output:3lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul_4?
lstm_cell_6/Add_4AddV2lstm_cell_6/Mul_4:z:0lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_4?
%lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_6/clip_by_value_2/Minimum/y?
#lstm_cell_6/clip_by_value_2/MinimumMinimumlstm_cell_6/Add_4:z:0.lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_6/clip_by_value_2/Minimum?
lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value_2/y?
lstm_cell_6/clip_by_value_2Maximum'lstm_cell_6/clip_by_value_2/Minimum:z:0&lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_value_2p
lstm_cell_6/Tanh_1Tanhlstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_6/Tanh_1?
lstm_cell_6/mul_5Mullstm_cell_6/clip_by_value_2:z:0lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp,lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_6_matmul_readvariableop_resource.lstm_cell_6_matmul_1_readvariableop_1_resource+lstm_cell_6_biasadd_readvariableop_resource*
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
while_body_38318*
condR
while_cond_38317*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_6_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_6/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_6_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_6/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_6/BiasAdd/ReadVariableOp"^lstm_cell_6/MatMul/ReadVariableOp$^lstm_cell_6/MatMul_1/ReadVariableOp&^lstm_cell_6/MatMul_1/ReadVariableOp_1!^lstm_cell_6/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_6/BiasAdd/ReadVariableOp"lstm_cell_6/BiasAdd/ReadVariableOp2F
!lstm_cell_6/MatMul/ReadVariableOp!lstm_cell_6/MatMul/ReadVariableOp2J
#lstm_cell_6/MatMul_1/ReadVariableOp#lstm_cell_6/MatMul_1/ReadVariableOp2N
%lstm_cell_6/MatMul_1/ReadVariableOp_1%lstm_cell_6/MatMul_1/ReadVariableOp_12D
 lstm_cell_6/mul_2/ReadVariableOp lstm_cell_6/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
?,
?
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_36666

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
:	2?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2	
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
(:22:22:22:22*
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
 *  ??2
clip_by_value/Minimum/y?
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
clip_by_value/y?
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
 *  ??2
clip_by_value_1/Minimum/y?
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
clip_by_value_1/y?
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
 *  ??2
clip_by_value_2/Minimum/y?
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
clip_by_value_2/y?
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
?,
?
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42103

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
:	2?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2	
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
(:22:22:22:22*
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
 *  ??2
clip_by_value/Minimum/y?
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
clip_by_value/y?
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
 *  ??2
clip_by_value_1/Minimum/y?
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
clip_by_value_1/y?
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
 *  ??2
clip_by_value_2/Minimum/y?
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
clip_by_value_2/y?
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
?X
?
while_body_38606
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_6_matmul_readvariableop_resource_0:	2?G
4while_lstm_cell_6_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_6_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_6_matmul_readvariableop_resource:	2?E
2while_lstm_cell_6_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_6_biasadd_readvariableop_resource:	???(while/lstm_cell_6/BiasAdd/ReadVariableOp?'while/lstm_cell_6/MatMul/ReadVariableOp?)while/lstm_cell_6/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_6/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_6_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02)
'while/lstm_cell_6/MatMul/ReadVariableOp?
while/lstm_cell_6/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/MatMul?
)while/lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_6_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_6/MatMul_1/ReadVariableOp?
while/lstm_cell_6/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_6/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/MatMul_1?
while/lstm_cell_6/addAddV2"while/lstm_cell_6/MatMul:product:0$while/lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/add?
(while/lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_6_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_6/BiasAdd/ReadVariableOp?
while/lstm_cell_6/BiasAddBiasAddwhile/lstm_cell_6/add:z:00while/lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/BiasAdd?
!while/lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_6/split/split_dim?
while/lstm_cell_6/splitSplit*while/lstm_cell_6/split/split_dim:output:0"while/lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_6/splitw
while/lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const{
while/lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_1?
while/lstm_cell_6/MulMul while/lstm_cell_6/split:output:0 while/lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul?
while/lstm_cell_6/Add_1AddV2while/lstm_cell_6/Mul:z:0"while/lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_1?
)while/lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_6/clip_by_value/Minimum/y?
'while/lstm_cell_6/clip_by_value/MinimumMinimumwhile/lstm_cell_6/Add_1:z:02while/lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222)
'while/lstm_cell_6/clip_by_value/Minimum?
!while/lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_6/clip_by_value/y?
while/lstm_cell_6/clip_by_valueMaximum+while/lstm_cell_6/clip_by_value/Minimum:z:0*while/lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222!
while/lstm_cell_6/clip_by_value{
while/lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const_2{
while/lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_3?
while/lstm_cell_6/Mul_1Mul while/lstm_cell_6/split:output:1"while/lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul_1?
while/lstm_cell_6/Add_2AddV2while/lstm_cell_6/Mul_1:z:0"while/lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_2?
+while/lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_6/clip_by_value_1/Minimum/y?
)while/lstm_cell_6/clip_by_value_1/MinimumMinimumwhile/lstm_cell_6/Add_2:z:04while/lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_6/clip_by_value_1/Minimum?
#while/lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_6/clip_by_value_1/y?
!while/lstm_cell_6/clip_by_value_1Maximum-while/lstm_cell_6/clip_by_value_1/Minimum:z:0,while/lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_6/clip_by_value_1?
while/lstm_cell_6/mul_2Mul%while/lstm_cell_6/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_2?
while/lstm_cell_6/TanhTanh while/lstm_cell_6/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_6/Tanh?
while/lstm_cell_6/mul_3Mul#while/lstm_cell_6/clip_by_value:z:0while/lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_3?
while/lstm_cell_6/add_3AddV2while/lstm_cell_6/mul_2:z:0while/lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_6/add_3{
while/lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const_4{
while/lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_5?
while/lstm_cell_6/Mul_4Mul while/lstm_cell_6/split:output:3"while/lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul_4?
while/lstm_cell_6/Add_4AddV2while/lstm_cell_6/Mul_4:z:0"while/lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_4?
+while/lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_6/clip_by_value_2/Minimum/y?
)while/lstm_cell_6/clip_by_value_2/MinimumMinimumwhile/lstm_cell_6/Add_4:z:04while/lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_6/clip_by_value_2/Minimum?
#while/lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_6/clip_by_value_2/y?
!while/lstm_cell_6/clip_by_value_2Maximum-while/lstm_cell_6/clip_by_value_2/Minimum:z:0,while/lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_6/clip_by_value_2?
while/lstm_cell_6/Tanh_1Tanhwhile/lstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Tanh_1?
while/lstm_cell_6/mul_5Mul%while/lstm_cell_6/clip_by_value_2:z:0while/lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_6/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_6/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_6/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_6/BiasAdd/ReadVariableOp(^while/lstm_cell_6/MatMul/ReadVariableOp*^while/lstm_cell_6/MatMul_1/ReadVariableOp*"
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
1while_lstm_cell_6_biasadd_readvariableop_resource3while_lstm_cell_6_biasadd_readvariableop_resource_0"j
2while_lstm_cell_6_matmul_1_readvariableop_resource4while_lstm_cell_6_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_6_matmul_readvariableop_resource2while_lstm_cell_6_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2T
(while/lstm_cell_6/BiasAdd/ReadVariableOp(while/lstm_cell_6/BiasAdd/ReadVariableOp2R
'while/lstm_cell_6/MatMul/ReadVariableOp'while/lstm_cell_6/MatMul/ReadVariableOp2V
)while/lstm_cell_6/MatMul_1/ReadVariableOp)while/lstm_cell_6/MatMul_1/ReadVariableOp: 
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
?
?
+__inference_lstm_cell_6_layer_call_fn_42247

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
:22:22:22*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_374422
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
?
?
'__inference_dense_2_layer_call_fn_42414

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
B__inference_dense_2_layer_call_and_return_conditional_losses_379352
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
?X
?
while_body_40548
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_7_matmul_readvariableop_resource_0:	?G
4while_lstm_cell_7_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_7_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_7_matmul_readvariableop_resource:	?E
2while_lstm_cell_7_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_7_biasadd_readvariableop_resource:	???(while/lstm_cell_7/BiasAdd/ReadVariableOp?'while/lstm_cell_7/MatMul/ReadVariableOp?)while/lstm_cell_7/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_7/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_7_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02)
'while/lstm_cell_7/MatMul/ReadVariableOp?
while/lstm_cell_7/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/MatMul?
)while/lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_7_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_7/MatMul_1/ReadVariableOp?
while/lstm_cell_7/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_7/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/MatMul_1?
while/lstm_cell_7/addAddV2"while/lstm_cell_7/MatMul:product:0$while/lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/add?
(while/lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_7_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_7/BiasAdd/ReadVariableOp?
while/lstm_cell_7/BiasAddBiasAddwhile/lstm_cell_7/add:z:00while/lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/BiasAdd?
!while/lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_7/split/split_dim?
while/lstm_cell_7/splitSplit*while/lstm_cell_7/split/split_dim:output:0"while/lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_7/splitw
while/lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const{
while/lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_1?
while/lstm_cell_7/MulMul while/lstm_cell_7/split:output:0 while/lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul?
while/lstm_cell_7/Add_1AddV2while/lstm_cell_7/Mul:z:0"while/lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_1?
)while/lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_7/clip_by_value/Minimum/y?
'while/lstm_cell_7/clip_by_value/MinimumMinimumwhile/lstm_cell_7/Add_1:z:02while/lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222)
'while/lstm_cell_7/clip_by_value/Minimum?
!while/lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_7/clip_by_value/y?
while/lstm_cell_7/clip_by_valueMaximum+while/lstm_cell_7/clip_by_value/Minimum:z:0*while/lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222!
while/lstm_cell_7/clip_by_value{
while/lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const_2{
while/lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_3?
while/lstm_cell_7/Mul_1Mul while/lstm_cell_7/split:output:1"while/lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul_1?
while/lstm_cell_7/Add_2AddV2while/lstm_cell_7/Mul_1:z:0"while/lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_2?
+while/lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_7/clip_by_value_1/Minimum/y?
)while/lstm_cell_7/clip_by_value_1/MinimumMinimumwhile/lstm_cell_7/Add_2:z:04while/lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_7/clip_by_value_1/Minimum?
#while/lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_7/clip_by_value_1/y?
!while/lstm_cell_7/clip_by_value_1Maximum-while/lstm_cell_7/clip_by_value_1/Minimum:z:0,while/lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_7/clip_by_value_1?
while/lstm_cell_7/mul_2Mul%while/lstm_cell_7/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_2?
while/lstm_cell_7/TanhTanh while/lstm_cell_7/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_7/Tanh?
while/lstm_cell_7/mul_3Mul#while/lstm_cell_7/clip_by_value:z:0while/lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_3?
while/lstm_cell_7/add_3AddV2while/lstm_cell_7/mul_2:z:0while/lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_7/add_3{
while/lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const_4{
while/lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_5?
while/lstm_cell_7/Mul_4Mul while/lstm_cell_7/split:output:3"while/lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul_4?
while/lstm_cell_7/Add_4AddV2while/lstm_cell_7/Mul_4:z:0"while/lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_4?
+while/lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_7/clip_by_value_2/Minimum/y?
)while/lstm_cell_7/clip_by_value_2/MinimumMinimumwhile/lstm_cell_7/Add_4:z:04while/lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_7/clip_by_value_2/Minimum?
#while/lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_7/clip_by_value_2/y?
!while/lstm_cell_7/clip_by_value_2Maximum-while/lstm_cell_7/clip_by_value_2/Minimum:z:0,while/lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_7/clip_by_value_2?
while/lstm_cell_7/Tanh_1Tanhwhile/lstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Tanh_1?
while/lstm_cell_7/mul_5Mul%while/lstm_cell_7/clip_by_value_2:z:0while/lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_7/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_7/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_7/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_7/BiasAdd/ReadVariableOp(^while/lstm_cell_7/MatMul/ReadVariableOp*^while/lstm_cell_7/MatMul_1/ReadVariableOp*"
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
1while_lstm_cell_7_biasadd_readvariableop_resource3while_lstm_cell_7_biasadd_readvariableop_resource_0"j
2while_lstm_cell_7_matmul_1_readvariableop_resource4while_lstm_cell_7_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_7_matmul_readvariableop_resource2while_lstm_cell_7_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2T
(while/lstm_cell_7/BiasAdd/ReadVariableOp(while/lstm_cell_7/BiasAdd/ReadVariableOp2R
'while/lstm_cell_7/MatMul/ReadVariableOp'while/lstm_cell_7/MatMul/ReadVariableOp2V
)while/lstm_cell_7/MatMul_1/ReadVariableOp)while/lstm_cell_7/MatMul_1/ReadVariableOp: 
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
?X
?
while_body_38318
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_6_matmul_readvariableop_resource_0:	2?G
4while_lstm_cell_6_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_6_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_6_matmul_readvariableop_resource:	2?E
2while_lstm_cell_6_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_6_biasadd_readvariableop_resource:	???(while/lstm_cell_6/BiasAdd/ReadVariableOp?'while/lstm_cell_6/MatMul/ReadVariableOp?)while/lstm_cell_6/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_6/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_6_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02)
'while/lstm_cell_6/MatMul/ReadVariableOp?
while/lstm_cell_6/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/MatMul?
)while/lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_6_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_6/MatMul_1/ReadVariableOp?
while/lstm_cell_6/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_6/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/MatMul_1?
while/lstm_cell_6/addAddV2"while/lstm_cell_6/MatMul:product:0$while/lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/add?
(while/lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_6_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_6/BiasAdd/ReadVariableOp?
while/lstm_cell_6/BiasAddBiasAddwhile/lstm_cell_6/add:z:00while/lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/BiasAdd?
!while/lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_6/split/split_dim?
while/lstm_cell_6/splitSplit*while/lstm_cell_6/split/split_dim:output:0"while/lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_6/splitw
while/lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const{
while/lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_1?
while/lstm_cell_6/MulMul while/lstm_cell_6/split:output:0 while/lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul?
while/lstm_cell_6/Add_1AddV2while/lstm_cell_6/Mul:z:0"while/lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_1?
)while/lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_6/clip_by_value/Minimum/y?
'while/lstm_cell_6/clip_by_value/MinimumMinimumwhile/lstm_cell_6/Add_1:z:02while/lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222)
'while/lstm_cell_6/clip_by_value/Minimum?
!while/lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_6/clip_by_value/y?
while/lstm_cell_6/clip_by_valueMaximum+while/lstm_cell_6/clip_by_value/Minimum:z:0*while/lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222!
while/lstm_cell_6/clip_by_value{
while/lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const_2{
while/lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_3?
while/lstm_cell_6/Mul_1Mul while/lstm_cell_6/split:output:1"while/lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul_1?
while/lstm_cell_6/Add_2AddV2while/lstm_cell_6/Mul_1:z:0"while/lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_2?
+while/lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_6/clip_by_value_1/Minimum/y?
)while/lstm_cell_6/clip_by_value_1/MinimumMinimumwhile/lstm_cell_6/Add_2:z:04while/lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_6/clip_by_value_1/Minimum?
#while/lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_6/clip_by_value_1/y?
!while/lstm_cell_6/clip_by_value_1Maximum-while/lstm_cell_6/clip_by_value_1/Minimum:z:0,while/lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_6/clip_by_value_1?
while/lstm_cell_6/mul_2Mul%while/lstm_cell_6/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_2?
while/lstm_cell_6/TanhTanh while/lstm_cell_6/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_6/Tanh?
while/lstm_cell_6/mul_3Mul#while/lstm_cell_6/clip_by_value:z:0while/lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_3?
while/lstm_cell_6/add_3AddV2while/lstm_cell_6/mul_2:z:0while/lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_6/add_3{
while/lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const_4{
while/lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_5?
while/lstm_cell_6/Mul_4Mul while/lstm_cell_6/split:output:3"while/lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul_4?
while/lstm_cell_6/Add_4AddV2while/lstm_cell_6/Mul_4:z:0"while/lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_4?
+while/lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_6/clip_by_value_2/Minimum/y?
)while/lstm_cell_6/clip_by_value_2/MinimumMinimumwhile/lstm_cell_6/Add_4:z:04while/lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_6/clip_by_value_2/Minimum?
#while/lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_6/clip_by_value_2/y?
!while/lstm_cell_6/clip_by_value_2Maximum-while/lstm_cell_6/clip_by_value_2/Minimum:z:0,while/lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_6/clip_by_value_2?
while/lstm_cell_6/Tanh_1Tanhwhile/lstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Tanh_1?
while/lstm_cell_6/mul_5Mul%while/lstm_cell_6/clip_by_value_2:z:0while/lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_6/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_6/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_6/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_6/BiasAdd/ReadVariableOp(^while/lstm_cell_6/MatMul/ReadVariableOp*^while/lstm_cell_6/MatMul_1/ReadVariableOp*"
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
1while_lstm_cell_6_biasadd_readvariableop_resource3while_lstm_cell_6_biasadd_readvariableop_resource_0"j
2while_lstm_cell_6_matmul_1_readvariableop_resource4while_lstm_cell_6_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_6_matmul_readvariableop_resource2while_lstm_cell_6_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2T
(while/lstm_cell_6/BiasAdd/ReadVariableOp(while/lstm_cell_6/BiasAdd/ReadVariableOp2R
'while/lstm_cell_6/MatMul/ReadVariableOp'while/lstm_cell_6/MatMul/ReadVariableOp2V
)while/lstm_cell_6/MatMul_1/ReadVariableOp)while/lstm_cell_6/MatMul_1/ReadVariableOp: 
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
?
?
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_41506

inputs8
&dense_2_matmul_readvariableop_resource:25
'dense_2_biasadd_readvariableop_resource:
identity??dense_2/BiasAdd/ReadVariableOp?dense_2/MatMul/ReadVariableOpD
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
dense_2/MatMul/ReadVariableOpReadVariableOp&dense_2_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_2/MatMul/ReadVariableOp?
dense_2/MatMulMatMulReshape:output:0%dense_2/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_2/MatMul?
dense_2/BiasAdd/ReadVariableOpReadVariableOp'dense_2_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_2/BiasAdd/ReadVariableOp?
dense_2/BiasAddBiasAdddense_2/MatMul:product:0&dense_2/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_2/BiasAddq
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
	Reshape_1Reshapedense_2/BiasAdd:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :??????????????????2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :??????????????????2

Identity?
NoOpNoOp^dense_2/BiasAdd/ReadVariableOp^dense_2/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:??????????????????2: : 2@
dense_2/BiasAdd/ReadVariableOpdense_2/BiasAdd/ReadVariableOp2>
dense_2/MatMul/ReadVariableOpdense_2/MatMul/ReadVariableOp:\ X
4
_output_shapes"
 :??????????????????2
 
_user_specified_nameinputs
?

?
B__inference_dense_2_layer_call_and_return_conditional_losses_37935

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
?
?
G__inference_sequential_2_layer_call_and_return_conditional_losses_39118
lstm_5_input
lstm_5_39088:	?
lstm_5_39090:22
lstm_5_39092:	2?
lstm_5_39094:	?
lstm_5_39096:22
lstm_4_39099:	2?
lstm_4_39101:22
lstm_4_39103:	2?
lstm_4_39105:	?
lstm_4_39107:22*
time_distributed_2_39110:2&
time_distributed_2_39112:
identity??lstm_4/StatefulPartitionedCall?lstm_5/StatefulPartitionedCall?*time_distributed_2/StatefulPartitionedCall?
lstm_5/StatefulPartitionedCallStatefulPartitionedCalllstm_5_inputlstm_5_39088lstm_5_39090lstm_5_39092lstm_5_39094lstm_5_39096*
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
GPU 2J 8? *J
fERC
A__inference_lstm_5_layer_call_and_return_conditional_losses_389192 
lstm_5/StatefulPartitionedCall?
lstm_4/StatefulPartitionedCallStatefulPartitionedCall'lstm_5/StatefulPartitionedCall:output:0lstm_4_39099lstm_4_39101lstm_4_39103lstm_4_39105lstm_4_39107*
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
GPU 2J 8? *J
fERC
A__inference_lstm_4_layer_call_and_return_conditional_losses_387112 
lstm_4/StatefulPartitionedCall?
*time_distributed_2/StatefulPartitionedCallStatefulPartitionedCall'lstm_4/StatefulPartitionedCall:output:0time_distributed_2_39110time_distributed_2_39112*
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
GPU 2J 8? *V
fQRO
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_385092,
*time_distributed_2/StatefulPartitionedCall?
 time_distributed_2/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2"
 time_distributed_2/Reshape/shape?
time_distributed_2/ReshapeReshape'lstm_4/StatefulPartitionedCall:output:0)time_distributed_2/Reshape/shape:output:0*
T0*
_output_shapes
:	?22
time_distributed_2/Reshape?
IdentityIdentity3time_distributed_2/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity?
NoOpNoOp^lstm_4/StatefulPartitionedCall^lstm_5/StatefulPartitionedCall+^time_distributed_2/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 2@
lstm_4/StatefulPartitionedCalllstm_4/StatefulPartitionedCall2@
lstm_5/StatefulPartitionedCalllstm_5/StatefulPartitionedCall2X
*time_distributed_2/StatefulPartitionedCall*time_distributed_2/StatefulPartitionedCall:P L
"
_output_shapes
:2

&
_user_specified_namelstm_5_input
?
?
while_cond_40191
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_40191___redundant_placeholder03
/while_while_cond_40191___redundant_placeholder13
/while_while_cond_40191___redundant_placeholder23
/while_while_cond_40191___redundant_placeholder3
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
?
?
while_cond_41319
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_41319___redundant_placeholder03
/while_while_cond_41319___redundant_placeholder13
/while_while_cond_41319___redundant_placeholder23
/while_while_cond_41319___redundant_placeholder3
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
?X
?
while_body_40192
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_7_matmul_readvariableop_resource_0:	?G
4while_lstm_cell_7_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_7_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_7_matmul_readvariableop_resource:	?E
2while_lstm_cell_7_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_7_biasadd_readvariableop_resource:	???(while/lstm_cell_7/BiasAdd/ReadVariableOp?'while/lstm_cell_7/MatMul/ReadVariableOp?)while/lstm_cell_7/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_7/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_7_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02)
'while/lstm_cell_7/MatMul/ReadVariableOp?
while/lstm_cell_7/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/MatMul?
)while/lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_7_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_7/MatMul_1/ReadVariableOp?
while/lstm_cell_7/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_7/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/MatMul_1?
while/lstm_cell_7/addAddV2"while/lstm_cell_7/MatMul:product:0$while/lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/add?
(while/lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_7_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_7/BiasAdd/ReadVariableOp?
while/lstm_cell_7/BiasAddBiasAddwhile/lstm_cell_7/add:z:00while/lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_7/BiasAdd?
!while/lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_7/split/split_dim?
while/lstm_cell_7/splitSplit*while/lstm_cell_7/split/split_dim:output:0"while/lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_7/splitw
while/lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const{
while/lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_1?
while/lstm_cell_7/MulMul while/lstm_cell_7/split:output:0 while/lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul?
while/lstm_cell_7/Add_1AddV2while/lstm_cell_7/Mul:z:0"while/lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_1?
)while/lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_7/clip_by_value/Minimum/y?
'while/lstm_cell_7/clip_by_value/MinimumMinimumwhile/lstm_cell_7/Add_1:z:02while/lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222)
'while/lstm_cell_7/clip_by_value/Minimum?
!while/lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_7/clip_by_value/y?
while/lstm_cell_7/clip_by_valueMaximum+while/lstm_cell_7/clip_by_value/Minimum:z:0*while/lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222!
while/lstm_cell_7/clip_by_value{
while/lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const_2{
while/lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_3?
while/lstm_cell_7/Mul_1Mul while/lstm_cell_7/split:output:1"while/lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul_1?
while/lstm_cell_7/Add_2AddV2while/lstm_cell_7/Mul_1:z:0"while/lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_2?
+while/lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_7/clip_by_value_1/Minimum/y?
)while/lstm_cell_7/clip_by_value_1/MinimumMinimumwhile/lstm_cell_7/Add_2:z:04while/lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_7/clip_by_value_1/Minimum?
#while/lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_7/clip_by_value_1/y?
!while/lstm_cell_7/clip_by_value_1Maximum-while/lstm_cell_7/clip_by_value_1/Minimum:z:0,while/lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_7/clip_by_value_1?
while/lstm_cell_7/mul_2Mul%while/lstm_cell_7/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_2?
while/lstm_cell_7/TanhTanh while/lstm_cell_7/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_7/Tanh?
while/lstm_cell_7/mul_3Mul#while/lstm_cell_7/clip_by_value:z:0while/lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_3?
while/lstm_cell_7/add_3AddV2while/lstm_cell_7/mul_2:z:0while/lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_7/add_3{
while/lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_7/Const_4{
while/lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_7/Const_5?
while/lstm_cell_7/Mul_4Mul while/lstm_cell_7/split:output:3"while/lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Mul_4?
while/lstm_cell_7/Add_4AddV2while/lstm_cell_7/Mul_4:z:0"while/lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Add_4?
+while/lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_7/clip_by_value_2/Minimum/y?
)while/lstm_cell_7/clip_by_value_2/MinimumMinimumwhile/lstm_cell_7/Add_4:z:04while/lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_7/clip_by_value_2/Minimum?
#while/lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_7/clip_by_value_2/y?
!while/lstm_cell_7/clip_by_value_2Maximum-while/lstm_cell_7/clip_by_value_2/Minimum:z:0,while/lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_7/clip_by_value_2?
while/lstm_cell_7/Tanh_1Tanhwhile/lstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_7/Tanh_1?
while/lstm_cell_7/mul_5Mul%while/lstm_cell_7/clip_by_value_2:z:0while/lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_7/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_7/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_7/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_7/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_7/BiasAdd/ReadVariableOp(^while/lstm_cell_7/MatMul/ReadVariableOp*^while/lstm_cell_7/MatMul_1/ReadVariableOp*"
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
1while_lstm_cell_7_biasadd_readvariableop_resource3while_lstm_cell_7_biasadd_readvariableop_resource_0"j
2while_lstm_cell_7_matmul_1_readvariableop_resource4while_lstm_cell_7_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_7_matmul_readvariableop_resource2while_lstm_cell_7_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2T
(while/lstm_cell_7/BiasAdd/ReadVariableOp(while/lstm_cell_7/BiasAdd/ReadVariableOp2R
'while/lstm_cell_7/MatMul/ReadVariableOp'while/lstm_cell_7/MatMul/ReadVariableOp2V
)while/lstm_cell_7/MatMul_1/ReadVariableOp)while/lstm_cell_7/MatMul_1/ReadVariableOp: 
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
?	
?
lstm_5_while_cond_39591*
&lstm_5_while_lstm_5_while_loop_counter0
,lstm_5_while_lstm_5_while_maximum_iterations
lstm_5_while_placeholder
lstm_5_while_placeholder_1
lstm_5_while_placeholder_2
lstm_5_while_placeholder_3*
&lstm_5_while_less_lstm_5_strided_sliceA
=lstm_5_while_lstm_5_while_cond_39591___redundant_placeholder0A
=lstm_5_while_lstm_5_while_cond_39591___redundant_placeholder1A
=lstm_5_while_lstm_5_while_cond_39591___redundant_placeholder2A
=lstm_5_while_lstm_5_while_cond_39591___redundant_placeholder3
lstm_5_while_identity
?
lstm_5/while/LessLesslstm_5_while_placeholder&lstm_5_while_less_lstm_5_strided_slice*
T0*
_output_shapes
: 2
lstm_5/while/Lessr
lstm_5/while/IdentityIdentitylstm_5/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_5/while/Identity"7
lstm_5_while_identitylstm_5/while/Identity:output:0*(
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
?
?
&__inference_lstm_5_layer_call_fn_40713

inputs
unknown:	?
	unknown_0:22
	unknown_1:	2?
	unknown_2:	?
	unknown_3:22
identity??StatefulPartitionedCall?
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
GPU 2J 8? *J
fERC
A__inference_lstm_5_layer_call_and_return_conditional_losses_389192
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
?,
?
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_36532

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
:	2?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2	
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
(:22:22:22:22*
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
 *  ??2
clip_by_value/Minimum/y?
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
clip_by_value/y?
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
 *  ??2
clip_by_value_1/Minimum/y?
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
clip_by_value_1/y?
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
 *  ??2
clip_by_value_2/Minimum/y?
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
clip_by_value_2/y?
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
?
?
while_cond_40013
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_40013___redundant_placeholder03
/while_while_cond_40013___redundant_placeholder13
/while_while_cond_40013___redundant_placeholder23
/while_while_cond_40013___redundant_placeholder3
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
?
?
+__inference_lstm_cell_6_layer_call_fn_42230

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
:22:22:22*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_373082
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
??
?
 __inference__wrapped_model_36359
lstm_5_inputQ
>sequential_2_lstm_5_lstm_cell_7_matmul_readvariableop_resource:	?R
@sequential_2_lstm_5_lstm_cell_7_matmul_1_readvariableop_resource:22U
Bsequential_2_lstm_5_lstm_cell_7_matmul_1_readvariableop_1_resource:	2?N
?sequential_2_lstm_5_lstm_cell_7_biasadd_readvariableop_resource:	?O
=sequential_2_lstm_5_lstm_cell_7_mul_2_readvariableop_resource:22Q
>sequential_2_lstm_4_lstm_cell_6_matmul_readvariableop_resource:	2?R
@sequential_2_lstm_4_lstm_cell_6_matmul_1_readvariableop_resource:22U
Bsequential_2_lstm_4_lstm_cell_6_matmul_1_readvariableop_1_resource:	2?N
?sequential_2_lstm_4_lstm_cell_6_biasadd_readvariableop_resource:	?O
=sequential_2_lstm_4_lstm_cell_6_mul_2_readvariableop_resource:22X
Fsequential_2_time_distributed_2_dense_2_matmul_readvariableop_resource:2U
Gsequential_2_time_distributed_2_dense_2_biasadd_readvariableop_resource:
identity??$sequential_2/lstm_4/AssignVariableOp?&sequential_2/lstm_4/AssignVariableOp_1?"sequential_2/lstm_4/ReadVariableOp?$sequential_2/lstm_4/ReadVariableOp_1?6sequential_2/lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp?5sequential_2/lstm_4/lstm_cell_6/MatMul/ReadVariableOp?7sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp?9sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1?4sequential_2/lstm_4/lstm_cell_6/mul_2/ReadVariableOp?sequential_2/lstm_4/while?$sequential_2/lstm_5/AssignVariableOp?&sequential_2/lstm_5/AssignVariableOp_1?"sequential_2/lstm_5/ReadVariableOp?$sequential_2/lstm_5/ReadVariableOp_1?6sequential_2/lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp?5sequential_2/lstm_5/lstm_cell_7/MatMul/ReadVariableOp?7sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp?9sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1?4sequential_2/lstm_5/lstm_cell_7/mul_2/ReadVariableOp?sequential_2/lstm_5/while?>sequential_2/time_distributed_2/dense_2/BiasAdd/ReadVariableOp?=sequential_2/time_distributed_2/dense_2/MatMul/ReadVariableOp?
"sequential_2/lstm_5/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2$
"sequential_2/lstm_5/transpose/perm?
sequential_2/lstm_5/transpose	Transposelstm_5_input+sequential_2/lstm_5/transpose/perm:output:0*
T0*"
_output_shapes
:
22
sequential_2/lstm_5/transpose?
sequential_2/lstm_5/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2      2
sequential_2/lstm_5/Shape?
'sequential_2/lstm_5/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2)
'sequential_2/lstm_5/strided_slice/stack?
)sequential_2/lstm_5/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2+
)sequential_2/lstm_5/strided_slice/stack_1?
)sequential_2/lstm_5/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2+
)sequential_2/lstm_5/strided_slice/stack_2?
!sequential_2/lstm_5/strided_sliceStridedSlice"sequential_2/lstm_5/Shape:output:00sequential_2/lstm_5/strided_slice/stack:output:02sequential_2/lstm_5/strided_slice/stack_1:output:02sequential_2/lstm_5/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2#
!sequential_2/lstm_5/strided_slice?
/sequential_2/lstm_5/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????21
/sequential_2/lstm_5/TensorArrayV2/element_shape?
!sequential_2/lstm_5/TensorArrayV2TensorListReserve8sequential_2/lstm_5/TensorArrayV2/element_shape:output:0*sequential_2/lstm_5/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02#
!sequential_2/lstm_5/TensorArrayV2?
Isequential_2/lstm_5/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      2K
Isequential_2/lstm_5/TensorArrayUnstack/TensorListFromTensor/element_shape?
;sequential_2/lstm_5/TensorArrayUnstack/TensorListFromTensorTensorListFromTensor!sequential_2/lstm_5/transpose:y:0Rsequential_2/lstm_5/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02=
;sequential_2/lstm_5/TensorArrayUnstack/TensorListFromTensor?
)sequential_2/lstm_5/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2+
)sequential_2/lstm_5/strided_slice_1/stack?
+sequential_2/lstm_5/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_2/lstm_5/strided_slice_1/stack_1?
+sequential_2/lstm_5/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_2/lstm_5/strided_slice_1/stack_2?
#sequential_2/lstm_5/strided_slice_1StridedSlice!sequential_2/lstm_5/transpose:y:02sequential_2/lstm_5/strided_slice_1/stack:output:04sequential_2/lstm_5/strided_slice_1/stack_1:output:04sequential_2/lstm_5/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2%
#sequential_2/lstm_5/strided_slice_1?
5sequential_2/lstm_5/lstm_cell_7/MatMul/ReadVariableOpReadVariableOp>sequential_2_lstm_5_lstm_cell_7_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype027
5sequential_2/lstm_5/lstm_cell_7/MatMul/ReadVariableOp?
&sequential_2/lstm_5/lstm_cell_7/MatMulMatMul,sequential_2/lstm_5/strided_slice_1:output:0=sequential_2/lstm_5/lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2(
&sequential_2/lstm_5/lstm_cell_7/MatMul?
7sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp@sequential_2_lstm_5_lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype029
7sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp?
9sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1ReadVariableOpBsequential_2_lstm_5_lstm_cell_7_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02;
9sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1?
(sequential_2/lstm_5/lstm_cell_7/MatMul_1MatMul?sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp:value:0Asequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2*
(sequential_2/lstm_5/lstm_cell_7/MatMul_1?
#sequential_2/lstm_5/lstm_cell_7/addAddV20sequential_2/lstm_5/lstm_cell_7/MatMul:product:02sequential_2/lstm_5/lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2%
#sequential_2/lstm_5/lstm_cell_7/add?
6sequential_2/lstm_5/lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp?sequential_2_lstm_5_lstm_cell_7_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype028
6sequential_2/lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp?
'sequential_2/lstm_5/lstm_cell_7/BiasAddBiasAdd'sequential_2/lstm_5/lstm_cell_7/add:z:0>sequential_2/lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2)
'sequential_2/lstm_5/lstm_cell_7/BiasAdd?
/sequential_2/lstm_5/lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :21
/sequential_2/lstm_5/lstm_cell_7/split/split_dim?
%sequential_2/lstm_5/lstm_cell_7/splitSplit8sequential_2/lstm_5/lstm_cell_7/split/split_dim:output:00sequential_2/lstm_5/lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2'
%sequential_2/lstm_5/lstm_cell_7/split?
%sequential_2/lstm_5/lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2'
%sequential_2/lstm_5/lstm_cell_7/Const?
'sequential_2/lstm_5/lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2)
'sequential_2/lstm_5/lstm_cell_7/Const_1?
#sequential_2/lstm_5/lstm_cell_7/MulMul.sequential_2/lstm_5/lstm_cell_7/split:output:0.sequential_2/lstm_5/lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222%
#sequential_2/lstm_5/lstm_cell_7/Mul?
%sequential_2/lstm_5/lstm_cell_7/Add_1AddV2'sequential_2/lstm_5/lstm_cell_7/Mul:z:00sequential_2/lstm_5/lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_5/lstm_cell_7/Add_1?
7sequential_2/lstm_5/lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??29
7sequential_2/lstm_5/lstm_cell_7/clip_by_value/Minimum/y?
5sequential_2/lstm_5/lstm_cell_7/clip_by_value/MinimumMinimum)sequential_2/lstm_5/lstm_cell_7/Add_1:z:0@sequential_2/lstm_5/lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:2227
5sequential_2/lstm_5/lstm_cell_7/clip_by_value/Minimum?
/sequential_2/lstm_5/lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    21
/sequential_2/lstm_5/lstm_cell_7/clip_by_value/y?
-sequential_2/lstm_5/lstm_cell_7/clip_by_valueMaximum9sequential_2/lstm_5/lstm_cell_7/clip_by_value/Minimum:z:08sequential_2/lstm_5/lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222/
-sequential_2/lstm_5/lstm_cell_7/clip_by_value?
'sequential_2/lstm_5/lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2)
'sequential_2/lstm_5/lstm_cell_7/Const_2?
'sequential_2/lstm_5/lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2)
'sequential_2/lstm_5/lstm_cell_7/Const_3?
%sequential_2/lstm_5/lstm_cell_7/Mul_1Mul.sequential_2/lstm_5/lstm_cell_7/split:output:10sequential_2/lstm_5/lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_5/lstm_cell_7/Mul_1?
%sequential_2/lstm_5/lstm_cell_7/Add_2AddV2)sequential_2/lstm_5/lstm_cell_7/Mul_1:z:00sequential_2/lstm_5/lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_5/lstm_cell_7/Add_2?
9sequential_2/lstm_5/lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2;
9sequential_2/lstm_5/lstm_cell_7/clip_by_value_1/Minimum/y?
7sequential_2/lstm_5/lstm_cell_7/clip_by_value_1/MinimumMinimum)sequential_2/lstm_5/lstm_cell_7/Add_2:z:0Bsequential_2/lstm_5/lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:2229
7sequential_2/lstm_5/lstm_cell_7/clip_by_value_1/Minimum?
1sequential_2/lstm_5/lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    23
1sequential_2/lstm_5/lstm_cell_7/clip_by_value_1/y?
/sequential_2/lstm_5/lstm_cell_7/clip_by_value_1Maximum;sequential_2/lstm_5/lstm_cell_7/clip_by_value_1/Minimum:z:0:sequential_2/lstm_5/lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:2221
/sequential_2/lstm_5/lstm_cell_7/clip_by_value_1?
4sequential_2/lstm_5/lstm_cell_7/mul_2/ReadVariableOpReadVariableOp=sequential_2_lstm_5_lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype026
4sequential_2/lstm_5/lstm_cell_7/mul_2/ReadVariableOp?
%sequential_2/lstm_5/lstm_cell_7/mul_2Mul3sequential_2/lstm_5/lstm_cell_7/clip_by_value_1:z:0<sequential_2/lstm_5/lstm_cell_7/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_5/lstm_cell_7/mul_2?
$sequential_2/lstm_5/lstm_cell_7/TanhTanh.sequential_2/lstm_5/lstm_cell_7/split:output:2*
T0*
_output_shapes

:222&
$sequential_2/lstm_5/lstm_cell_7/Tanh?
%sequential_2/lstm_5/lstm_cell_7/mul_3Mul1sequential_2/lstm_5/lstm_cell_7/clip_by_value:z:0(sequential_2/lstm_5/lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_5/lstm_cell_7/mul_3?
%sequential_2/lstm_5/lstm_cell_7/add_3AddV2)sequential_2/lstm_5/lstm_cell_7/mul_2:z:0)sequential_2/lstm_5/lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_5/lstm_cell_7/add_3?
'sequential_2/lstm_5/lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2)
'sequential_2/lstm_5/lstm_cell_7/Const_4?
'sequential_2/lstm_5/lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2)
'sequential_2/lstm_5/lstm_cell_7/Const_5?
%sequential_2/lstm_5/lstm_cell_7/Mul_4Mul.sequential_2/lstm_5/lstm_cell_7/split:output:30sequential_2/lstm_5/lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_5/lstm_cell_7/Mul_4?
%sequential_2/lstm_5/lstm_cell_7/Add_4AddV2)sequential_2/lstm_5/lstm_cell_7/Mul_4:z:00sequential_2/lstm_5/lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_5/lstm_cell_7/Add_4?
9sequential_2/lstm_5/lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2;
9sequential_2/lstm_5/lstm_cell_7/clip_by_value_2/Minimum/y?
7sequential_2/lstm_5/lstm_cell_7/clip_by_value_2/MinimumMinimum)sequential_2/lstm_5/lstm_cell_7/Add_4:z:0Bsequential_2/lstm_5/lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:2229
7sequential_2/lstm_5/lstm_cell_7/clip_by_value_2/Minimum?
1sequential_2/lstm_5/lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    23
1sequential_2/lstm_5/lstm_cell_7/clip_by_value_2/y?
/sequential_2/lstm_5/lstm_cell_7/clip_by_value_2Maximum;sequential_2/lstm_5/lstm_cell_7/clip_by_value_2/Minimum:z:0:sequential_2/lstm_5/lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:2221
/sequential_2/lstm_5/lstm_cell_7/clip_by_value_2?
&sequential_2/lstm_5/lstm_cell_7/Tanh_1Tanh)sequential_2/lstm_5/lstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222(
&sequential_2/lstm_5/lstm_cell_7/Tanh_1?
%sequential_2/lstm_5/lstm_cell_7/mul_5Mul3sequential_2/lstm_5/lstm_cell_7/clip_by_value_2:z:0*sequential_2/lstm_5/lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_5/lstm_cell_7/mul_5?
1sequential_2/lstm_5/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   23
1sequential_2/lstm_5/TensorArrayV2_1/element_shape?
#sequential_2/lstm_5/TensorArrayV2_1TensorListReserve:sequential_2/lstm_5/TensorArrayV2_1/element_shape:output:0*sequential_2/lstm_5/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02%
#sequential_2/lstm_5/TensorArrayV2_1v
sequential_2/lstm_5/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential_2/lstm_5/time?
"sequential_2/lstm_5/ReadVariableOpReadVariableOp@sequential_2_lstm_5_lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02$
"sequential_2/lstm_5/ReadVariableOp?
$sequential_2/lstm_5/ReadVariableOp_1ReadVariableOp=sequential_2_lstm_5_lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02&
$sequential_2/lstm_5/ReadVariableOp_1?
,sequential_2/lstm_5/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2.
,sequential_2/lstm_5/while/maximum_iterations?
&sequential_2/lstm_5/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2(
&sequential_2/lstm_5/while/loop_counter?
sequential_2/lstm_5/whileWhile/sequential_2/lstm_5/while/loop_counter:output:05sequential_2/lstm_5/while/maximum_iterations:output:0!sequential_2/lstm_5/time:output:0,sequential_2/lstm_5/TensorArrayV2_1:handle:0*sequential_2/lstm_5/ReadVariableOp:value:0,sequential_2/lstm_5/ReadVariableOp_1:value:0*sequential_2/lstm_5/strided_slice:output:0Ksequential_2/lstm_5/TensorArrayUnstack/TensorListFromTensor:output_handle:0>sequential_2_lstm_5_lstm_cell_7_matmul_readvariableop_resourceBsequential_2_lstm_5_lstm_cell_7_matmul_1_readvariableop_1_resource?sequential_2_lstm_5_lstm_cell_7_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *0
body(R&
$sequential_2_lstm_5_while_body_36068*0
cond(R&
$sequential_2_lstm_5_while_cond_36067*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
sequential_2/lstm_5/while?
Dsequential_2/lstm_5/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2F
Dsequential_2/lstm_5/TensorArrayV2Stack/TensorListStack/element_shape?
6sequential_2/lstm_5/TensorArrayV2Stack/TensorListStackTensorListStack"sequential_2/lstm_5/while:output:3Msequential_2/lstm_5/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
element_dtype028
6sequential_2/lstm_5/TensorArrayV2Stack/TensorListStack?
)sequential_2/lstm_5/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2+
)sequential_2/lstm_5/strided_slice_2/stack?
+sequential_2/lstm_5/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2-
+sequential_2/lstm_5/strided_slice_2/stack_1?
+sequential_2/lstm_5/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_2/lstm_5/strided_slice_2/stack_2?
#sequential_2/lstm_5/strided_slice_2StridedSlice?sequential_2/lstm_5/TensorArrayV2Stack/TensorListStack:tensor:02sequential_2/lstm_5/strided_slice_2/stack:output:04sequential_2/lstm_5/strided_slice_2/stack_1:output:04sequential_2/lstm_5/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2%
#sequential_2/lstm_5/strided_slice_2?
$sequential_2/lstm_5/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2&
$sequential_2/lstm_5/transpose_1/perm?
sequential_2/lstm_5/transpose_1	Transpose?sequential_2/lstm_5/TensorArrayV2Stack/TensorListStack:tensor:0-sequential_2/lstm_5/transpose_1/perm:output:0*
T0*"
_output_shapes
:2
22!
sequential_2/lstm_5/transpose_1?
sequential_2/lstm_5/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential_2/lstm_5/runtime?
$sequential_2/lstm_5/AssignVariableOpAssignVariableOp@sequential_2_lstm_5_lstm_cell_7_matmul_1_readvariableop_resource"sequential_2/lstm_5/while:output:4#^sequential_2/lstm_5/ReadVariableOp8^sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02&
$sequential_2/lstm_5/AssignVariableOp?
&sequential_2/lstm_5/AssignVariableOp_1AssignVariableOp=sequential_2_lstm_5_lstm_cell_7_mul_2_readvariableop_resource"sequential_2/lstm_5/while:output:5%^sequential_2/lstm_5/ReadVariableOp_15^sequential_2/lstm_5/lstm_cell_7/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02(
&sequential_2/lstm_5/AssignVariableOp_1?
"sequential_2/lstm_4/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2$
"sequential_2/lstm_4/transpose/perm?
sequential_2/lstm_4/transpose	Transpose#sequential_2/lstm_5/transpose_1:y:0+sequential_2/lstm_4/transpose/perm:output:0*
T0*"
_output_shapes
:
222
sequential_2/lstm_4/transpose?
sequential_2/lstm_4/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2   2   2
sequential_2/lstm_4/Shape?
'sequential_2/lstm_4/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2)
'sequential_2/lstm_4/strided_slice/stack?
)sequential_2/lstm_4/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2+
)sequential_2/lstm_4/strided_slice/stack_1?
)sequential_2/lstm_4/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2+
)sequential_2/lstm_4/strided_slice/stack_2?
!sequential_2/lstm_4/strided_sliceStridedSlice"sequential_2/lstm_4/Shape:output:00sequential_2/lstm_4/strided_slice/stack:output:02sequential_2/lstm_4/strided_slice/stack_1:output:02sequential_2/lstm_4/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2#
!sequential_2/lstm_4/strided_slice?
/sequential_2/lstm_4/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????21
/sequential_2/lstm_4/TensorArrayV2/element_shape?
!sequential_2/lstm_4/TensorArrayV2TensorListReserve8sequential_2/lstm_4/TensorArrayV2/element_shape:output:0*sequential_2/lstm_4/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02#
!sequential_2/lstm_4/TensorArrayV2?
Isequential_2/lstm_4/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2K
Isequential_2/lstm_4/TensorArrayUnstack/TensorListFromTensor/element_shape?
;sequential_2/lstm_4/TensorArrayUnstack/TensorListFromTensorTensorListFromTensor!sequential_2/lstm_4/transpose:y:0Rsequential_2/lstm_4/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02=
;sequential_2/lstm_4/TensorArrayUnstack/TensorListFromTensor?
)sequential_2/lstm_4/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2+
)sequential_2/lstm_4/strided_slice_1/stack?
+sequential_2/lstm_4/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_2/lstm_4/strided_slice_1/stack_1?
+sequential_2/lstm_4/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_2/lstm_4/strided_slice_1/stack_2?
#sequential_2/lstm_4/strided_slice_1StridedSlice!sequential_2/lstm_4/transpose:y:02sequential_2/lstm_4/strided_slice_1/stack:output:04sequential_2/lstm_4/strided_slice_1/stack_1:output:04sequential_2/lstm_4/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2%
#sequential_2/lstm_4/strided_slice_1?
5sequential_2/lstm_4/lstm_cell_6/MatMul/ReadVariableOpReadVariableOp>sequential_2_lstm_4_lstm_cell_6_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype027
5sequential_2/lstm_4/lstm_cell_6/MatMul/ReadVariableOp?
&sequential_2/lstm_4/lstm_cell_6/MatMulMatMul,sequential_2/lstm_4/strided_slice_1:output:0=sequential_2/lstm_4/lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2(
&sequential_2/lstm_4/lstm_cell_6/MatMul?
7sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp@sequential_2_lstm_4_lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype029
7sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp?
9sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1ReadVariableOpBsequential_2_lstm_4_lstm_cell_6_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02;
9sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1?
(sequential_2/lstm_4/lstm_cell_6/MatMul_1MatMul?sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp:value:0Asequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2*
(sequential_2/lstm_4/lstm_cell_6/MatMul_1?
#sequential_2/lstm_4/lstm_cell_6/addAddV20sequential_2/lstm_4/lstm_cell_6/MatMul:product:02sequential_2/lstm_4/lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2%
#sequential_2/lstm_4/lstm_cell_6/add?
6sequential_2/lstm_4/lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp?sequential_2_lstm_4_lstm_cell_6_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype028
6sequential_2/lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp?
'sequential_2/lstm_4/lstm_cell_6/BiasAddBiasAdd'sequential_2/lstm_4/lstm_cell_6/add:z:0>sequential_2/lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2)
'sequential_2/lstm_4/lstm_cell_6/BiasAdd?
/sequential_2/lstm_4/lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :21
/sequential_2/lstm_4/lstm_cell_6/split/split_dim?
%sequential_2/lstm_4/lstm_cell_6/splitSplit8sequential_2/lstm_4/lstm_cell_6/split/split_dim:output:00sequential_2/lstm_4/lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2'
%sequential_2/lstm_4/lstm_cell_6/split?
%sequential_2/lstm_4/lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2'
%sequential_2/lstm_4/lstm_cell_6/Const?
'sequential_2/lstm_4/lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2)
'sequential_2/lstm_4/lstm_cell_6/Const_1?
#sequential_2/lstm_4/lstm_cell_6/MulMul.sequential_2/lstm_4/lstm_cell_6/split:output:0.sequential_2/lstm_4/lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222%
#sequential_2/lstm_4/lstm_cell_6/Mul?
%sequential_2/lstm_4/lstm_cell_6/Add_1AddV2'sequential_2/lstm_4/lstm_cell_6/Mul:z:00sequential_2/lstm_4/lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_4/lstm_cell_6/Add_1?
7sequential_2/lstm_4/lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??29
7sequential_2/lstm_4/lstm_cell_6/clip_by_value/Minimum/y?
5sequential_2/lstm_4/lstm_cell_6/clip_by_value/MinimumMinimum)sequential_2/lstm_4/lstm_cell_6/Add_1:z:0@sequential_2/lstm_4/lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:2227
5sequential_2/lstm_4/lstm_cell_6/clip_by_value/Minimum?
/sequential_2/lstm_4/lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    21
/sequential_2/lstm_4/lstm_cell_6/clip_by_value/y?
-sequential_2/lstm_4/lstm_cell_6/clip_by_valueMaximum9sequential_2/lstm_4/lstm_cell_6/clip_by_value/Minimum:z:08sequential_2/lstm_4/lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222/
-sequential_2/lstm_4/lstm_cell_6/clip_by_value?
'sequential_2/lstm_4/lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2)
'sequential_2/lstm_4/lstm_cell_6/Const_2?
'sequential_2/lstm_4/lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2)
'sequential_2/lstm_4/lstm_cell_6/Const_3?
%sequential_2/lstm_4/lstm_cell_6/Mul_1Mul.sequential_2/lstm_4/lstm_cell_6/split:output:10sequential_2/lstm_4/lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_4/lstm_cell_6/Mul_1?
%sequential_2/lstm_4/lstm_cell_6/Add_2AddV2)sequential_2/lstm_4/lstm_cell_6/Mul_1:z:00sequential_2/lstm_4/lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_4/lstm_cell_6/Add_2?
9sequential_2/lstm_4/lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2;
9sequential_2/lstm_4/lstm_cell_6/clip_by_value_1/Minimum/y?
7sequential_2/lstm_4/lstm_cell_6/clip_by_value_1/MinimumMinimum)sequential_2/lstm_4/lstm_cell_6/Add_2:z:0Bsequential_2/lstm_4/lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:2229
7sequential_2/lstm_4/lstm_cell_6/clip_by_value_1/Minimum?
1sequential_2/lstm_4/lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    23
1sequential_2/lstm_4/lstm_cell_6/clip_by_value_1/y?
/sequential_2/lstm_4/lstm_cell_6/clip_by_value_1Maximum;sequential_2/lstm_4/lstm_cell_6/clip_by_value_1/Minimum:z:0:sequential_2/lstm_4/lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:2221
/sequential_2/lstm_4/lstm_cell_6/clip_by_value_1?
4sequential_2/lstm_4/lstm_cell_6/mul_2/ReadVariableOpReadVariableOp=sequential_2_lstm_4_lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype026
4sequential_2/lstm_4/lstm_cell_6/mul_2/ReadVariableOp?
%sequential_2/lstm_4/lstm_cell_6/mul_2Mul3sequential_2/lstm_4/lstm_cell_6/clip_by_value_1:z:0<sequential_2/lstm_4/lstm_cell_6/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_4/lstm_cell_6/mul_2?
$sequential_2/lstm_4/lstm_cell_6/TanhTanh.sequential_2/lstm_4/lstm_cell_6/split:output:2*
T0*
_output_shapes

:222&
$sequential_2/lstm_4/lstm_cell_6/Tanh?
%sequential_2/lstm_4/lstm_cell_6/mul_3Mul1sequential_2/lstm_4/lstm_cell_6/clip_by_value:z:0(sequential_2/lstm_4/lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_4/lstm_cell_6/mul_3?
%sequential_2/lstm_4/lstm_cell_6/add_3AddV2)sequential_2/lstm_4/lstm_cell_6/mul_2:z:0)sequential_2/lstm_4/lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_4/lstm_cell_6/add_3?
'sequential_2/lstm_4/lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2)
'sequential_2/lstm_4/lstm_cell_6/Const_4?
'sequential_2/lstm_4/lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2)
'sequential_2/lstm_4/lstm_cell_6/Const_5?
%sequential_2/lstm_4/lstm_cell_6/Mul_4Mul.sequential_2/lstm_4/lstm_cell_6/split:output:30sequential_2/lstm_4/lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_4/lstm_cell_6/Mul_4?
%sequential_2/lstm_4/lstm_cell_6/Add_4AddV2)sequential_2/lstm_4/lstm_cell_6/Mul_4:z:00sequential_2/lstm_4/lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_4/lstm_cell_6/Add_4?
9sequential_2/lstm_4/lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2;
9sequential_2/lstm_4/lstm_cell_6/clip_by_value_2/Minimum/y?
7sequential_2/lstm_4/lstm_cell_6/clip_by_value_2/MinimumMinimum)sequential_2/lstm_4/lstm_cell_6/Add_4:z:0Bsequential_2/lstm_4/lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:2229
7sequential_2/lstm_4/lstm_cell_6/clip_by_value_2/Minimum?
1sequential_2/lstm_4/lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    23
1sequential_2/lstm_4/lstm_cell_6/clip_by_value_2/y?
/sequential_2/lstm_4/lstm_cell_6/clip_by_value_2Maximum;sequential_2/lstm_4/lstm_cell_6/clip_by_value_2/Minimum:z:0:sequential_2/lstm_4/lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:2221
/sequential_2/lstm_4/lstm_cell_6/clip_by_value_2?
&sequential_2/lstm_4/lstm_cell_6/Tanh_1Tanh)sequential_2/lstm_4/lstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222(
&sequential_2/lstm_4/lstm_cell_6/Tanh_1?
%sequential_2/lstm_4/lstm_cell_6/mul_5Mul3sequential_2/lstm_4/lstm_cell_6/clip_by_value_2:z:0*sequential_2/lstm_4/lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222'
%sequential_2/lstm_4/lstm_cell_6/mul_5?
1sequential_2/lstm_4/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   23
1sequential_2/lstm_4/TensorArrayV2_1/element_shape?
#sequential_2/lstm_4/TensorArrayV2_1TensorListReserve:sequential_2/lstm_4/TensorArrayV2_1/element_shape:output:0*sequential_2/lstm_4/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02%
#sequential_2/lstm_4/TensorArrayV2_1v
sequential_2/lstm_4/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential_2/lstm_4/time?
"sequential_2/lstm_4/ReadVariableOpReadVariableOp@sequential_2_lstm_4_lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02$
"sequential_2/lstm_4/ReadVariableOp?
$sequential_2/lstm_4/ReadVariableOp_1ReadVariableOp=sequential_2_lstm_4_lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02&
$sequential_2/lstm_4/ReadVariableOp_1?
,sequential_2/lstm_4/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2.
,sequential_2/lstm_4/while/maximum_iterations?
&sequential_2/lstm_4/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2(
&sequential_2/lstm_4/while/loop_counter?
sequential_2/lstm_4/whileWhile/sequential_2/lstm_4/while/loop_counter:output:05sequential_2/lstm_4/while/maximum_iterations:output:0!sequential_2/lstm_4/time:output:0,sequential_2/lstm_4/TensorArrayV2_1:handle:0*sequential_2/lstm_4/ReadVariableOp:value:0,sequential_2/lstm_4/ReadVariableOp_1:value:0*sequential_2/lstm_4/strided_slice:output:0Ksequential_2/lstm_4/TensorArrayUnstack/TensorListFromTensor:output_handle:0>sequential_2_lstm_4_lstm_cell_6_matmul_readvariableop_resourceBsequential_2_lstm_4_lstm_cell_6_matmul_1_readvariableop_1_resource?sequential_2_lstm_4_lstm_cell_6_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *0
body(R&
$sequential_2_lstm_4_while_body_36242*0
cond(R&
$sequential_2_lstm_4_while_cond_36241*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
sequential_2/lstm_4/while?
Dsequential_2/lstm_4/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2F
Dsequential_2/lstm_4/TensorArrayV2Stack/TensorListStack/element_shape?
6sequential_2/lstm_4/TensorArrayV2Stack/TensorListStackTensorListStack"sequential_2/lstm_4/while:output:3Msequential_2/lstm_4/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
element_dtype028
6sequential_2/lstm_4/TensorArrayV2Stack/TensorListStack?
)sequential_2/lstm_4/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2+
)sequential_2/lstm_4/strided_slice_2/stack?
+sequential_2/lstm_4/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2-
+sequential_2/lstm_4/strided_slice_2/stack_1?
+sequential_2/lstm_4/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2-
+sequential_2/lstm_4/strided_slice_2/stack_2?
#sequential_2/lstm_4/strided_slice_2StridedSlice?sequential_2/lstm_4/TensorArrayV2Stack/TensorListStack:tensor:02sequential_2/lstm_4/strided_slice_2/stack:output:04sequential_2/lstm_4/strided_slice_2/stack_1:output:04sequential_2/lstm_4/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2%
#sequential_2/lstm_4/strided_slice_2?
$sequential_2/lstm_4/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2&
$sequential_2/lstm_4/transpose_1/perm?
sequential_2/lstm_4/transpose_1	Transpose?sequential_2/lstm_4/TensorArrayV2Stack/TensorListStack:tensor:0-sequential_2/lstm_4/transpose_1/perm:output:0*
T0*"
_output_shapes
:2
22!
sequential_2/lstm_4/transpose_1?
sequential_2/lstm_4/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential_2/lstm_4/runtime?
$sequential_2/lstm_4/AssignVariableOpAssignVariableOp@sequential_2_lstm_4_lstm_cell_6_matmul_1_readvariableop_resource"sequential_2/lstm_4/while:output:4#^sequential_2/lstm_4/ReadVariableOp8^sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02&
$sequential_2/lstm_4/AssignVariableOp?
&sequential_2/lstm_4/AssignVariableOp_1AssignVariableOp=sequential_2_lstm_4_lstm_cell_6_mul_2_readvariableop_resource"sequential_2/lstm_4/while:output:5%^sequential_2/lstm_4/ReadVariableOp_15^sequential_2/lstm_4/lstm_cell_6/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02(
&sequential_2/lstm_4/AssignVariableOp_1?
-sequential_2/time_distributed_2/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2/
-sequential_2/time_distributed_2/Reshape/shape?
'sequential_2/time_distributed_2/ReshapeReshape#sequential_2/lstm_4/transpose_1:y:06sequential_2/time_distributed_2/Reshape/shape:output:0*
T0*
_output_shapes
:	?22)
'sequential_2/time_distributed_2/Reshape?
=sequential_2/time_distributed_2/dense_2/MatMul/ReadVariableOpReadVariableOpFsequential_2_time_distributed_2_dense_2_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02?
=sequential_2/time_distributed_2/dense_2/MatMul/ReadVariableOp?
.sequential_2/time_distributed_2/dense_2/MatMulMatMul0sequential_2/time_distributed_2/Reshape:output:0Esequential_2/time_distributed_2/dense_2/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?20
.sequential_2/time_distributed_2/dense_2/MatMul?
>sequential_2/time_distributed_2/dense_2/BiasAdd/ReadVariableOpReadVariableOpGsequential_2_time_distributed_2_dense_2_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02@
>sequential_2/time_distributed_2/dense_2/BiasAdd/ReadVariableOp?
/sequential_2/time_distributed_2/dense_2/BiasAddBiasAdd8sequential_2/time_distributed_2/dense_2/MatMul:product:0Fsequential_2/time_distributed_2/dense_2/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?21
/sequential_2/time_distributed_2/dense_2/BiasAdd?
/sequential_2/time_distributed_2/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      21
/sequential_2/time_distributed_2/Reshape_1/shape?
)sequential_2/time_distributed_2/Reshape_1Reshape8sequential_2/time_distributed_2/dense_2/BiasAdd:output:08sequential_2/time_distributed_2/Reshape_1/shape:output:0*
T0*"
_output_shapes
:2
2+
)sequential_2/time_distributed_2/Reshape_1?
/sequential_2/time_distributed_2/Reshape_2/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   21
/sequential_2/time_distributed_2/Reshape_2/shape?
)sequential_2/time_distributed_2/Reshape_2Reshape#sequential_2/lstm_4/transpose_1:y:08sequential_2/time_distributed_2/Reshape_2/shape:output:0*
T0*
_output_shapes
:	?22+
)sequential_2/time_distributed_2/Reshape_2?
IdentityIdentity2sequential_2/time_distributed_2/Reshape_1:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity?
NoOpNoOp%^sequential_2/lstm_4/AssignVariableOp'^sequential_2/lstm_4/AssignVariableOp_1#^sequential_2/lstm_4/ReadVariableOp%^sequential_2/lstm_4/ReadVariableOp_17^sequential_2/lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp6^sequential_2/lstm_4/lstm_cell_6/MatMul/ReadVariableOp8^sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp:^sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_15^sequential_2/lstm_4/lstm_cell_6/mul_2/ReadVariableOp^sequential_2/lstm_4/while%^sequential_2/lstm_5/AssignVariableOp'^sequential_2/lstm_5/AssignVariableOp_1#^sequential_2/lstm_5/ReadVariableOp%^sequential_2/lstm_5/ReadVariableOp_17^sequential_2/lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp6^sequential_2/lstm_5/lstm_cell_7/MatMul/ReadVariableOp8^sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp:^sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_15^sequential_2/lstm_5/lstm_cell_7/mul_2/ReadVariableOp^sequential_2/lstm_5/while?^sequential_2/time_distributed_2/dense_2/BiasAdd/ReadVariableOp>^sequential_2/time_distributed_2/dense_2/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 2L
$sequential_2/lstm_4/AssignVariableOp$sequential_2/lstm_4/AssignVariableOp2P
&sequential_2/lstm_4/AssignVariableOp_1&sequential_2/lstm_4/AssignVariableOp_12H
"sequential_2/lstm_4/ReadVariableOp"sequential_2/lstm_4/ReadVariableOp2L
$sequential_2/lstm_4/ReadVariableOp_1$sequential_2/lstm_4/ReadVariableOp_12p
6sequential_2/lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp6sequential_2/lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp2n
5sequential_2/lstm_4/lstm_cell_6/MatMul/ReadVariableOp5sequential_2/lstm_4/lstm_cell_6/MatMul/ReadVariableOp2r
7sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp7sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp2v
9sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_19sequential_2/lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_12l
4sequential_2/lstm_4/lstm_cell_6/mul_2/ReadVariableOp4sequential_2/lstm_4/lstm_cell_6/mul_2/ReadVariableOp26
sequential_2/lstm_4/whilesequential_2/lstm_4/while2L
$sequential_2/lstm_5/AssignVariableOp$sequential_2/lstm_5/AssignVariableOp2P
&sequential_2/lstm_5/AssignVariableOp_1&sequential_2/lstm_5/AssignVariableOp_12H
"sequential_2/lstm_5/ReadVariableOp"sequential_2/lstm_5/ReadVariableOp2L
$sequential_2/lstm_5/ReadVariableOp_1$sequential_2/lstm_5/ReadVariableOp_12p
6sequential_2/lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp6sequential_2/lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp2n
5sequential_2/lstm_5/lstm_cell_7/MatMul/ReadVariableOp5sequential_2/lstm_5/lstm_cell_7/MatMul/ReadVariableOp2r
7sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp7sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp2v
9sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_19sequential_2/lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_12l
4sequential_2/lstm_5/lstm_cell_7/mul_2/ReadVariableOp4sequential_2/lstm_5/lstm_cell_7/mul_2/ReadVariableOp26
sequential_2/lstm_5/whilesequential_2/lstm_5/while2?
>sequential_2/time_distributed_2/dense_2/BiasAdd/ReadVariableOp>sequential_2/time_distributed_2/dense_2/BiasAdd/ReadVariableOp2~
=sequential_2/time_distributed_2/dense_2/MatMul/ReadVariableOp=sequential_2/time_distributed_2/dense_2/MatMul/ReadVariableOp:P L
"
_output_shapes
:2

&
_user_specified_namelstm_5_input
?Q
?
__inference__traced_save_42548
file_prefix(
$savev2_adam_iter_read_readvariableop	*
&savev2_adam_beta_1_read_readvariableop*
&savev2_adam_beta_2_read_readvariableop)
%savev2_adam_decay_read_readvariableop1
-savev2_adam_learning_rate_read_readvariableop8
4savev2_lstm_5_lstm_cell_7_kernel_read_readvariableopB
>savev2_lstm_5_lstm_cell_7_recurrent_kernel_read_readvariableop6
2savev2_lstm_5_lstm_cell_7_bias_read_readvariableop8
4savev2_lstm_4_lstm_cell_6_kernel_read_readvariableopB
>savev2_lstm_4_lstm_cell_6_recurrent_kernel_read_readvariableop6
2savev2_lstm_4_lstm_cell_6_bias_read_readvariableop8
4savev2_time_distributed_2_kernel_read_readvariableop6
2savev2_time_distributed_2_bias_read_readvariableop.
*savev2_lstm_5_variable_read_readvariableop0
,savev2_lstm_5_variable_1_read_readvariableop.
*savev2_lstm_4_variable_read_readvariableop0
,savev2_lstm_4_variable_1_read_readvariableop$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop&
"savev2_total_1_read_readvariableop&
"savev2_count_1_read_readvariableop?
;savev2_adam_lstm_5_lstm_cell_7_kernel_m_read_readvariableopI
Esavev2_adam_lstm_5_lstm_cell_7_recurrent_kernel_m_read_readvariableop=
9savev2_adam_lstm_5_lstm_cell_7_bias_m_read_readvariableop?
;savev2_adam_lstm_4_lstm_cell_6_kernel_m_read_readvariableopI
Esavev2_adam_lstm_4_lstm_cell_6_recurrent_kernel_m_read_readvariableop=
9savev2_adam_lstm_4_lstm_cell_6_bias_m_read_readvariableop?
;savev2_adam_time_distributed_2_kernel_m_read_readvariableop=
9savev2_adam_time_distributed_2_bias_m_read_readvariableop?
;savev2_adam_lstm_5_lstm_cell_7_kernel_v_read_readvariableopI
Esavev2_adam_lstm_5_lstm_cell_7_recurrent_kernel_v_read_readvariableop=
9savev2_adam_lstm_5_lstm_cell_7_bias_v_read_readvariableop?
;savev2_adam_lstm_4_lstm_cell_6_kernel_v_read_readvariableopI
Esavev2_adam_lstm_4_lstm_cell_6_recurrent_kernel_v_read_readvariableop=
9savev2_adam_lstm_4_lstm_cell_6_bias_v_read_readvariableop?
;savev2_adam_time_distributed_2_kernel_v_read_readvariableop=
9savev2_adam_time_distributed_2_bias_v_read_readvariableop
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
ShardedFilename?
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:&*
dtype0*?
value?B?&B)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB&variables/0/.ATTRIBUTES/VARIABLE_VALUEB&variables/1/.ATTRIBUTES/VARIABLE_VALUEB&variables/2/.ATTRIBUTES/VARIABLE_VALUEB&variables/3/.ATTRIBUTES/VARIABLE_VALUEB&variables/4/.ATTRIBUTES/VARIABLE_VALUEB&variables/5/.ATTRIBUTES/VARIABLE_VALUEB&variables/6/.ATTRIBUTES/VARIABLE_VALUEB&variables/7/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-0/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/0/.ATTRIBUTES/VARIABLE_VALUEBBlayer_with_weights-1/keras_api/states/1/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/1/count/.ATTRIBUTES/VARIABLE_VALUEBBvariables/0/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/1/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/2/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/3/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/4/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/5/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/6/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/7/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBBvariables/0/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/1/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/2/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/3/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/4/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/5/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/6/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBBvariables/7/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
SaveV2/tensor_names?
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:&*
dtype0*_
valueVBT&B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B 2
SaveV2/shape_and_slices?
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:0$savev2_adam_iter_read_readvariableop&savev2_adam_beta_1_read_readvariableop&savev2_adam_beta_2_read_readvariableop%savev2_adam_decay_read_readvariableop-savev2_adam_learning_rate_read_readvariableop4savev2_lstm_5_lstm_cell_7_kernel_read_readvariableop>savev2_lstm_5_lstm_cell_7_recurrent_kernel_read_readvariableop2savev2_lstm_5_lstm_cell_7_bias_read_readvariableop4savev2_lstm_4_lstm_cell_6_kernel_read_readvariableop>savev2_lstm_4_lstm_cell_6_recurrent_kernel_read_readvariableop2savev2_lstm_4_lstm_cell_6_bias_read_readvariableop4savev2_time_distributed_2_kernel_read_readvariableop2savev2_time_distributed_2_bias_read_readvariableop*savev2_lstm_5_variable_read_readvariableop,savev2_lstm_5_variable_1_read_readvariableop*savev2_lstm_4_variable_read_readvariableop,savev2_lstm_4_variable_1_read_readvariableop savev2_total_read_readvariableop savev2_count_read_readvariableop"savev2_total_1_read_readvariableop"savev2_count_1_read_readvariableop;savev2_adam_lstm_5_lstm_cell_7_kernel_m_read_readvariableopEsavev2_adam_lstm_5_lstm_cell_7_recurrent_kernel_m_read_readvariableop9savev2_adam_lstm_5_lstm_cell_7_bias_m_read_readvariableop;savev2_adam_lstm_4_lstm_cell_6_kernel_m_read_readvariableopEsavev2_adam_lstm_4_lstm_cell_6_recurrent_kernel_m_read_readvariableop9savev2_adam_lstm_4_lstm_cell_6_bias_m_read_readvariableop;savev2_adam_time_distributed_2_kernel_m_read_readvariableop9savev2_adam_time_distributed_2_bias_m_read_readvariableop;savev2_adam_lstm_5_lstm_cell_7_kernel_v_read_readvariableopEsavev2_adam_lstm_5_lstm_cell_7_recurrent_kernel_v_read_readvariableop9savev2_adam_lstm_5_lstm_cell_7_bias_v_read_readvariableop;savev2_adam_lstm_4_lstm_cell_6_kernel_v_read_readvariableopEsavev2_adam_lstm_4_lstm_cell_6_recurrent_kernel_v_read_readvariableop9savev2_adam_lstm_4_lstm_cell_6_bias_v_read_readvariableop;savev2_adam_time_distributed_2_kernel_v_read_readvariableop9savev2_adam_time_distributed_2_bias_v_read_readvariableopsavev2_const"/device:CPU:0*
_output_shapes
 *4
dtypes*
(2&	2
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

identity_1Identity_1:output:0*?
_input_shapes?
?: : : : : : :	?:	2?:?:	2?:	2?:?:2::22:22:22:22: : : : :	?:	2?:?:	2?:	2?:?:2::	?:	2?:?:	2?:	2?:?:2:: 2(
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
:	?:%!

_output_shapes
:	2?:!

_output_shapes	
:?:%	!

_output_shapes
:	2?:%
!

_output_shapes
:	2?:!

_output_shapes	
:?:$ 

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
:	?:%!

_output_shapes
:	2?:!

_output_shapes	
:?:%!

_output_shapes
:	2?:%!

_output_shapes
:	2?:!

_output_shapes	
:?:$ 

_output_shapes

:2: 

_output_shapes
::%!

_output_shapes
:	?:%!

_output_shapes
:	2?:! 

_output_shapes	
:?:%!!

_output_shapes
:	2?:%"!

_output_shapes
:	2?:!#

_output_shapes	
:?:$$ 

_output_shapes

:2: %

_output_shapes
::&

_output_shapes
: 
?
?
while_cond_38317
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_38317___redundant_placeholder03
/while_while_cond_38317___redundant_placeholder13
/while_while_cond_38317___redundant_placeholder23
/while_while_cond_38317___redundant_placeholder3
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
?
?
G__inference_sequential_2_layer_call_and_return_conditional_losses_38996

inputs
lstm_5_38966:	?
lstm_5_38968:22
lstm_5_38970:	2?
lstm_5_38972:	?
lstm_5_38974:22
lstm_4_38977:	2?
lstm_4_38979:22
lstm_4_38981:	2?
lstm_4_38983:	?
lstm_4_38985:22*
time_distributed_2_38988:2&
time_distributed_2_38990:
identity??lstm_4/StatefulPartitionedCall?lstm_5/StatefulPartitionedCall?*time_distributed_2/StatefulPartitionedCall?
lstm_5/StatefulPartitionedCallStatefulPartitionedCallinputslstm_5_38966lstm_5_38968lstm_5_38970lstm_5_38972lstm_5_38974*
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
GPU 2J 8? *J
fERC
A__inference_lstm_5_layer_call_and_return_conditional_losses_389192 
lstm_5/StatefulPartitionedCall?
lstm_4/StatefulPartitionedCallStatefulPartitionedCall'lstm_5/StatefulPartitionedCall:output:0lstm_4_38977lstm_4_38979lstm_4_38981lstm_4_38983lstm_4_38985*
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
GPU 2J 8? *J
fERC
A__inference_lstm_4_layer_call_and_return_conditional_losses_387112 
lstm_4/StatefulPartitionedCall?
*time_distributed_2/StatefulPartitionedCallStatefulPartitionedCall'lstm_4/StatefulPartitionedCall:output:0time_distributed_2_38988time_distributed_2_38990*
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
GPU 2J 8? *V
fQRO
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_385092,
*time_distributed_2/StatefulPartitionedCall?
 time_distributed_2/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2"
 time_distributed_2/Reshape/shape?
time_distributed_2/ReshapeReshape'lstm_4/StatefulPartitionedCall:output:0)time_distributed_2/Reshape/shape:output:0*
T0*
_output_shapes
:	?22
time_distributed_2/Reshape?
IdentityIdentity3time_distributed_2/StatefulPartitionedCall:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity?
NoOpNoOp^lstm_4/StatefulPartitionedCall^lstm_5/StatefulPartitionedCall+^time_distributed_2/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 2@
lstm_4/StatefulPartitionedCalllstm_4/StatefulPartitionedCall2@
lstm_5/StatefulPartitionedCalllstm_5/StatefulPartitionedCall2X
*time_distributed_2/StatefulPartitionedCall*time_distributed_2/StatefulPartitionedCall:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
?.
?
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42308

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
:	2?2
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
?	
?
&__inference_lstm_4_layer_call_fn_41455
inputs_0
unknown:22
	unknown_0:22
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
:2?????????2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_4_layer_call_and_return_conditional_losses_376712
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:2?????????22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2?????????2: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:2?????????2
"
_user_specified_name
inputs/0
?
?
while_cond_37601
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_37601___redundant_placeholder03
/while_while_cond_37601___redundant_placeholder13
/while_while_cond_37601___redundant_placeholder23
/while_while_cond_37601___redundant_placeholder3
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
?
?
while_cond_40547
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_40547___redundant_placeholder03
/while_while_cond_40547___redundant_placeholder13
/while_while_cond_40547___redundant_placeholder23
/while_while_cond_40547___redundant_placeholder3
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
?.
?
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41906

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
:	2?2
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
?9
?
A__inference_lstm_5_layer_call_and_return_conditional_losses_36895

inputs#
lstm_cell_7_36807:22#
lstm_cell_7_36809:22$
lstm_cell_7_36811:	?$
lstm_cell_7_36813:	2? 
lstm_cell_7_36815:	?
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?#lstm_cell_7/StatefulPartitionedCall?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:?????????22
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1?
#lstm_cell_7/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_1:output:0lstm_cell_7_36807lstm_cell_7_36809lstm_cell_7_36811lstm_cell_7_36813lstm_cell_7_36815*
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
GPU 2J 8? *O
fJRH
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_367572%
#lstm_cell_7/StatefulPartitionedCall?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOplstm_cell_7_36807*
_output_shapes

:22*
dtype02
ReadVariableOpv
ReadVariableOp_1ReadVariableOplstm_cell_7_36809*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_7_36811lstm_cell_7_36813lstm_cell_7_36815*
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
while_body_36826*
condR
while_cond_36825*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????22*
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

:22*
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
:2?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOplstm_cell_7_36807while:output:4^ReadVariableOp$^lstm_cell_7/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOplstm_cell_7_36809while:output:5^ReadVariableOp_1$^lstm_cell_7/StatefulPartitionedCall*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1$^lstm_cell_7/StatefulPartitionedCall^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2?????????: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12J
#lstm_cell_7/StatefulPartitionedCall#lstm_cell_7/StatefulPartitionedCall2
whilewhile:S O
+
_output_shapes
:2?????????
 
_user_specified_nameinputs
?X
?
while_body_40964
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_6_matmul_readvariableop_resource_0:	2?G
4while_lstm_cell_6_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_6_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_6_matmul_readvariableop_resource:	2?E
2while_lstm_cell_6_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_6_biasadd_readvariableop_resource:	???(while/lstm_cell_6/BiasAdd/ReadVariableOp?'while/lstm_cell_6/MatMul/ReadVariableOp?)while/lstm_cell_6/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_6/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_6_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02)
'while/lstm_cell_6/MatMul/ReadVariableOp?
while/lstm_cell_6/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/MatMul?
)while/lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_6_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_6/MatMul_1/ReadVariableOp?
while/lstm_cell_6/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_6/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/MatMul_1?
while/lstm_cell_6/addAddV2"while/lstm_cell_6/MatMul:product:0$while/lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/add?
(while/lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_6_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_6/BiasAdd/ReadVariableOp?
while/lstm_cell_6/BiasAddBiasAddwhile/lstm_cell_6/add:z:00while/lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/BiasAdd?
!while/lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_6/split/split_dim?
while/lstm_cell_6/splitSplit*while/lstm_cell_6/split/split_dim:output:0"while/lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_6/splitw
while/lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const{
while/lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_1?
while/lstm_cell_6/MulMul while/lstm_cell_6/split:output:0 while/lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul?
while/lstm_cell_6/Add_1AddV2while/lstm_cell_6/Mul:z:0"while/lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_1?
)while/lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_6/clip_by_value/Minimum/y?
'while/lstm_cell_6/clip_by_value/MinimumMinimumwhile/lstm_cell_6/Add_1:z:02while/lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222)
'while/lstm_cell_6/clip_by_value/Minimum?
!while/lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_6/clip_by_value/y?
while/lstm_cell_6/clip_by_valueMaximum+while/lstm_cell_6/clip_by_value/Minimum:z:0*while/lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222!
while/lstm_cell_6/clip_by_value{
while/lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const_2{
while/lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_3?
while/lstm_cell_6/Mul_1Mul while/lstm_cell_6/split:output:1"while/lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul_1?
while/lstm_cell_6/Add_2AddV2while/lstm_cell_6/Mul_1:z:0"while/lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_2?
+while/lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_6/clip_by_value_1/Minimum/y?
)while/lstm_cell_6/clip_by_value_1/MinimumMinimumwhile/lstm_cell_6/Add_2:z:04while/lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_6/clip_by_value_1/Minimum?
#while/lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_6/clip_by_value_1/y?
!while/lstm_cell_6/clip_by_value_1Maximum-while/lstm_cell_6/clip_by_value_1/Minimum:z:0,while/lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_6/clip_by_value_1?
while/lstm_cell_6/mul_2Mul%while/lstm_cell_6/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_2?
while/lstm_cell_6/TanhTanh while/lstm_cell_6/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_6/Tanh?
while/lstm_cell_6/mul_3Mul#while/lstm_cell_6/clip_by_value:z:0while/lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_3?
while/lstm_cell_6/add_3AddV2while/lstm_cell_6/mul_2:z:0while/lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_6/add_3{
while/lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const_4{
while/lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_5?
while/lstm_cell_6/Mul_4Mul while/lstm_cell_6/split:output:3"while/lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul_4?
while/lstm_cell_6/Add_4AddV2while/lstm_cell_6/Mul_4:z:0"while/lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_4?
+while/lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_6/clip_by_value_2/Minimum/y?
)while/lstm_cell_6/clip_by_value_2/MinimumMinimumwhile/lstm_cell_6/Add_4:z:04while/lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_6/clip_by_value_2/Minimum?
#while/lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_6/clip_by_value_2/y?
!while/lstm_cell_6/clip_by_value_2Maximum-while/lstm_cell_6/clip_by_value_2/Minimum:z:0,while/lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_6/clip_by_value_2?
while/lstm_cell_6/Tanh_1Tanhwhile/lstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Tanh_1?
while/lstm_cell_6/mul_5Mul%while/lstm_cell_6/clip_by_value_2:z:0while/lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_6/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_6/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_6/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_6/BiasAdd/ReadVariableOp(^while/lstm_cell_6/MatMul/ReadVariableOp*^while/lstm_cell_6/MatMul_1/ReadVariableOp*"
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
1while_lstm_cell_6_biasadd_readvariableop_resource3while_lstm_cell_6_biasadd_readvariableop_resource_0"j
2while_lstm_cell_6_matmul_1_readvariableop_resource4while_lstm_cell_6_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_6_matmul_readvariableop_resource2while_lstm_cell_6_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2T
(while/lstm_cell_6/BiasAdd/ReadVariableOp(while/lstm_cell_6/BiasAdd/ReadVariableOp2R
'while/lstm_cell_6/MatMul/ReadVariableOp'while/lstm_cell_6/MatMul/ReadVariableOp2V
)while/lstm_cell_6/MatMul_1/ReadVariableOp)while/lstm_cell_6/MatMul_1/ReadVariableOp: 
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
?.
?
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42213

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
:	2?2
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
?
?
while_cond_38813
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_38813___redundant_placeholder03
/while_while_cond_38813___redundant_placeholder13
/while_while_cond_38813___redundant_placeholder23
/while_while_cond_38813___redundant_placeholder3
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
ȋ
?
G__inference_sequential_2_layer_call_and_return_conditional_losses_39883

inputsD
1lstm_5_lstm_cell_7_matmul_readvariableop_resource:	?E
3lstm_5_lstm_cell_7_matmul_1_readvariableop_resource:22H
5lstm_5_lstm_cell_7_matmul_1_readvariableop_1_resource:	2?A
2lstm_5_lstm_cell_7_biasadd_readvariableop_resource:	?B
0lstm_5_lstm_cell_7_mul_2_readvariableop_resource:22D
1lstm_4_lstm_cell_6_matmul_readvariableop_resource:	2?E
3lstm_4_lstm_cell_6_matmul_1_readvariableop_resource:22H
5lstm_4_lstm_cell_6_matmul_1_readvariableop_1_resource:	2?A
2lstm_4_lstm_cell_6_biasadd_readvariableop_resource:	?B
0lstm_4_lstm_cell_6_mul_2_readvariableop_resource:22K
9time_distributed_2_dense_2_matmul_readvariableop_resource:2H
:time_distributed_2_dense_2_biasadd_readvariableop_resource:
identity??lstm_4/AssignVariableOp?lstm_4/AssignVariableOp_1?lstm_4/ReadVariableOp?lstm_4/ReadVariableOp_1?)lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp?(lstm_4/lstm_cell_6/MatMul/ReadVariableOp?*lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp?,lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1?'lstm_4/lstm_cell_6/mul_2/ReadVariableOp?lstm_4/while?lstm_5/AssignVariableOp?lstm_5/AssignVariableOp_1?lstm_5/ReadVariableOp?lstm_5/ReadVariableOp_1?)lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp?(lstm_5/lstm_cell_7/MatMul/ReadVariableOp?*lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp?,lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1?'lstm_5/lstm_cell_7/mul_2/ReadVariableOp?lstm_5/while?1time_distributed_2/dense_2/BiasAdd/ReadVariableOp?0time_distributed_2/dense_2/MatMul/ReadVariableOp?
lstm_5/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_5/transpose/perm?
lstm_5/transpose	Transposeinputslstm_5/transpose/perm:output:0*
T0*"
_output_shapes
:
22
lstm_5/transposeq
lstm_5/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2      2
lstm_5/Shape?
lstm_5/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_5/strided_slice/stack?
lstm_5/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_5/strided_slice/stack_1?
lstm_5/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_5/strided_slice/stack_2?
lstm_5/strided_sliceStridedSlicelstm_5/Shape:output:0#lstm_5/strided_slice/stack:output:0%lstm_5/strided_slice/stack_1:output:0%lstm_5/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_5/strided_slice?
"lstm_5/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2$
"lstm_5/TensorArrayV2/element_shape?
lstm_5/TensorArrayV2TensorListReserve+lstm_5/TensorArrayV2/element_shape:output:0lstm_5/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_5/TensorArrayV2?
<lstm_5/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      2>
<lstm_5/TensorArrayUnstack/TensorListFromTensor/element_shape?
.lstm_5/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_5/transpose:y:0Elstm_5/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type020
.lstm_5/TensorArrayUnstack/TensorListFromTensor?
lstm_5/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_5/strided_slice_1/stack?
lstm_5/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_5/strided_slice_1/stack_1?
lstm_5/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_5/strided_slice_1/stack_2?
lstm_5/strided_slice_1StridedSlicelstm_5/transpose:y:0%lstm_5/strided_slice_1/stack:output:0'lstm_5/strided_slice_1/stack_1:output:0'lstm_5/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:2*
shrink_axis_mask2
lstm_5/strided_slice_1?
(lstm_5/lstm_cell_7/MatMul/ReadVariableOpReadVariableOp1lstm_5_lstm_cell_7_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02*
(lstm_5/lstm_cell_7/MatMul/ReadVariableOp?
lstm_5/lstm_cell_7/MatMulMatMullstm_5/strided_slice_1:output:00lstm_5/lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_5/lstm_cell_7/MatMul?
*lstm_5/lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp3lstm_5_lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02,
*lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp?
,lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1ReadVariableOp5lstm_5_lstm_cell_7_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02.
,lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1?
lstm_5/lstm_cell_7/MatMul_1MatMul2lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp:value:04lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_5/lstm_cell_7/MatMul_1?
lstm_5/lstm_cell_7/addAddV2#lstm_5/lstm_cell_7/MatMul:product:0%lstm_5/lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_5/lstm_cell_7/add?
)lstm_5/lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp2lstm_5_lstm_cell_7_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02+
)lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp?
lstm_5/lstm_cell_7/BiasAddBiasAddlstm_5/lstm_cell_7/add:z:01lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_5/lstm_cell_7/BiasAdd?
"lstm_5/lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"lstm_5/lstm_cell_7/split/split_dim?
lstm_5/lstm_cell_7/splitSplit+lstm_5/lstm_cell_7/split/split_dim:output:0#lstm_5/lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_5/lstm_cell_7/splity
lstm_5/lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_5/lstm_cell_7/Const}
lstm_5/lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_5/lstm_cell_7/Const_1?
lstm_5/lstm_cell_7/MulMul!lstm_5/lstm_cell_7/split:output:0!lstm_5/lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Mul?
lstm_5/lstm_cell_7/Add_1AddV2lstm_5/lstm_cell_7/Mul:z:0#lstm_5/lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Add_1?
*lstm_5/lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2,
*lstm_5/lstm_cell_7/clip_by_value/Minimum/y?
(lstm_5/lstm_cell_7/clip_by_value/MinimumMinimumlstm_5/lstm_cell_7/Add_1:z:03lstm_5/lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(lstm_5/lstm_cell_7/clip_by_value/Minimum?
"lstm_5/lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"lstm_5/lstm_cell_7/clip_by_value/y?
 lstm_5/lstm_cell_7/clip_by_valueMaximum,lstm_5/lstm_cell_7/clip_by_value/Minimum:z:0+lstm_5/lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 lstm_5/lstm_cell_7/clip_by_value}
lstm_5/lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_5/lstm_cell_7/Const_2}
lstm_5/lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_5/lstm_cell_7/Const_3?
lstm_5/lstm_cell_7/Mul_1Mul!lstm_5/lstm_cell_7/split:output:1#lstm_5/lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Mul_1?
lstm_5/lstm_cell_7/Add_2AddV2lstm_5/lstm_cell_7/Mul_1:z:0#lstm_5/lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Add_2?
,lstm_5/lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_5/lstm_cell_7/clip_by_value_1/Minimum/y?
*lstm_5/lstm_cell_7/clip_by_value_1/MinimumMinimumlstm_5/lstm_cell_7/Add_2:z:05lstm_5/lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*lstm_5/lstm_cell_7/clip_by_value_1/Minimum?
$lstm_5/lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_5/lstm_cell_7/clip_by_value_1/y?
"lstm_5/lstm_cell_7/clip_by_value_1Maximum.lstm_5/lstm_cell_7/clip_by_value_1/Minimum:z:0-lstm_5/lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"lstm_5/lstm_cell_7/clip_by_value_1?
'lstm_5/lstm_cell_7/mul_2/ReadVariableOpReadVariableOp0lstm_5_lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02)
'lstm_5/lstm_cell_7/mul_2/ReadVariableOp?
lstm_5/lstm_cell_7/mul_2Mul&lstm_5/lstm_cell_7/clip_by_value_1:z:0/lstm_5/lstm_cell_7/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/mul_2?
lstm_5/lstm_cell_7/TanhTanh!lstm_5/lstm_cell_7/split:output:2*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Tanh?
lstm_5/lstm_cell_7/mul_3Mul$lstm_5/lstm_cell_7/clip_by_value:z:0lstm_5/lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/mul_3?
lstm_5/lstm_cell_7/add_3AddV2lstm_5/lstm_cell_7/mul_2:z:0lstm_5/lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/add_3}
lstm_5/lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_5/lstm_cell_7/Const_4}
lstm_5/lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_5/lstm_cell_7/Const_5?
lstm_5/lstm_cell_7/Mul_4Mul!lstm_5/lstm_cell_7/split:output:3#lstm_5/lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Mul_4?
lstm_5/lstm_cell_7/Add_4AddV2lstm_5/lstm_cell_7/Mul_4:z:0#lstm_5/lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Add_4?
,lstm_5/lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_5/lstm_cell_7/clip_by_value_2/Minimum/y?
*lstm_5/lstm_cell_7/clip_by_value_2/MinimumMinimumlstm_5/lstm_cell_7/Add_4:z:05lstm_5/lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*lstm_5/lstm_cell_7/clip_by_value_2/Minimum?
$lstm_5/lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_5/lstm_cell_7/clip_by_value_2/y?
"lstm_5/lstm_cell_7/clip_by_value_2Maximum.lstm_5/lstm_cell_7/clip_by_value_2/Minimum:z:0-lstm_5/lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"lstm_5/lstm_cell_7/clip_by_value_2?
lstm_5/lstm_cell_7/Tanh_1Tanhlstm_5/lstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/Tanh_1?
lstm_5/lstm_cell_7/mul_5Mul&lstm_5/lstm_cell_7/clip_by_value_2:z:0lstm_5/lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_5/lstm_cell_7/mul_5?
$lstm_5/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2&
$lstm_5/TensorArrayV2_1/element_shape?
lstm_5/TensorArrayV2_1TensorListReserve-lstm_5/TensorArrayV2_1/element_shape:output:0lstm_5/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_5/TensorArrayV2_1\
lstm_5/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_5/time?
lstm_5/ReadVariableOpReadVariableOp3lstm_5_lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_5/ReadVariableOp?
lstm_5/ReadVariableOp_1ReadVariableOp0lstm_5_lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_5/ReadVariableOp_1?
lstm_5/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2!
lstm_5/while/maximum_iterationsx
lstm_5/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_5/while/loop_counter?
lstm_5/whileWhile"lstm_5/while/loop_counter:output:0(lstm_5/while/maximum_iterations:output:0lstm_5/time:output:0lstm_5/TensorArrayV2_1:handle:0lstm_5/ReadVariableOp:value:0lstm_5/ReadVariableOp_1:value:0lstm_5/strided_slice:output:0>lstm_5/TensorArrayUnstack/TensorListFromTensor:output_handle:01lstm_5_lstm_cell_7_matmul_readvariableop_resource5lstm_5_lstm_cell_7_matmul_1_readvariableop_1_resource2lstm_5_lstm_cell_7_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *#
bodyR
lstm_5_while_body_39592*#
condR
lstm_5_while_cond_39591*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
lstm_5/while?
7lstm_5/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7lstm_5/TensorArrayV2Stack/TensorListStack/element_shape?
)lstm_5/TensorArrayV2Stack/TensorListStackTensorListStacklstm_5/while:output:3@lstm_5/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
element_dtype02+
)lstm_5/TensorArrayV2Stack/TensorListStack?
lstm_5/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
lstm_5/strided_slice_2/stack?
lstm_5/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2 
lstm_5/strided_slice_2/stack_1?
lstm_5/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_5/strided_slice_2/stack_2?
lstm_5/strided_slice_2StridedSlice2lstm_5/TensorArrayV2Stack/TensorListStack:tensor:0%lstm_5/strided_slice_2/stack:output:0'lstm_5/strided_slice_2/stack_1:output:0'lstm_5/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2
lstm_5/strided_slice_2?
lstm_5/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_5/transpose_1/perm?
lstm_5/transpose_1	Transpose2lstm_5/TensorArrayV2Stack/TensorListStack:tensor:0 lstm_5/transpose_1/perm:output:0*
T0*"
_output_shapes
:2
22
lstm_5/transpose_1t
lstm_5/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_5/runtime?
lstm_5/AssignVariableOpAssignVariableOp3lstm_5_lstm_cell_7_matmul_1_readvariableop_resourcelstm_5/while:output:4^lstm_5/ReadVariableOp+^lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_5/AssignVariableOp?
lstm_5/AssignVariableOp_1AssignVariableOp0lstm_5_lstm_cell_7_mul_2_readvariableop_resourcelstm_5/while:output:5^lstm_5/ReadVariableOp_1(^lstm_5/lstm_cell_7/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_5/AssignVariableOp_1?
lstm_4/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_4/transpose/perm?
lstm_4/transpose	Transposelstm_5/transpose_1:y:0lstm_4/transpose/perm:output:0*
T0*"
_output_shapes
:
222
lstm_4/transposeq
lstm_4/ShapeConst*
_output_shapes
:*
dtype0*!
valueB"
   2   2   2
lstm_4/Shape?
lstm_4/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_4/strided_slice/stack?
lstm_4/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_4/strided_slice/stack_1?
lstm_4/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_4/strided_slice/stack_2?
lstm_4/strided_sliceStridedSlicelstm_4/Shape:output:0#lstm_4/strided_slice/stack:output:0%lstm_4/strided_slice/stack_1:output:0%lstm_4/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_4/strided_slice?
"lstm_4/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2$
"lstm_4/TensorArrayV2/element_shape?
lstm_4/TensorArrayV2TensorListReserve+lstm_4/TensorArrayV2/element_shape:output:0lstm_4/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_4/TensorArrayV2?
<lstm_4/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2>
<lstm_4/TensorArrayUnstack/TensorListFromTensor/element_shape?
.lstm_4/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_4/transpose:y:0Elstm_4/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type020
.lstm_4/TensorArrayUnstack/TensorListFromTensor?
lstm_4/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_4/strided_slice_1/stack?
lstm_4/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_4/strided_slice_1/stack_1?
lstm_4/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_4/strided_slice_1/stack_2?
lstm_4/strided_slice_1StridedSlicelstm_4/transpose:y:0%lstm_4/strided_slice_1/stack:output:0'lstm_4/strided_slice_1/stack_1:output:0'lstm_4/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2
lstm_4/strided_slice_1?
(lstm_4/lstm_cell_6/MatMul/ReadVariableOpReadVariableOp1lstm_4_lstm_cell_6_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02*
(lstm_4/lstm_cell_6/MatMul/ReadVariableOp?
lstm_4/lstm_cell_6/MatMulMatMullstm_4/strided_slice_1:output:00lstm_4/lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_4/lstm_cell_6/MatMul?
*lstm_4/lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp3lstm_4_lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02,
*lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp?
,lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1ReadVariableOp5lstm_4_lstm_cell_6_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02.
,lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1?
lstm_4/lstm_cell_6/MatMul_1MatMul2lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp:value:04lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_4/lstm_cell_6/MatMul_1?
lstm_4/lstm_cell_6/addAddV2#lstm_4/lstm_cell_6/MatMul:product:0%lstm_4/lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_4/lstm_cell_6/add?
)lstm_4/lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp2lstm_4_lstm_cell_6_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02+
)lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp?
lstm_4/lstm_cell_6/BiasAddBiasAddlstm_4/lstm_cell_6/add:z:01lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_4/lstm_cell_6/BiasAdd?
"lstm_4/lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"lstm_4/lstm_cell_6/split/split_dim?
lstm_4/lstm_cell_6/splitSplit+lstm_4/lstm_cell_6/split/split_dim:output:0#lstm_4/lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_4/lstm_cell_6/splity
lstm_4/lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_4/lstm_cell_6/Const}
lstm_4/lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_4/lstm_cell_6/Const_1?
lstm_4/lstm_cell_6/MulMul!lstm_4/lstm_cell_6/split:output:0!lstm_4/lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Mul?
lstm_4/lstm_cell_6/Add_1AddV2lstm_4/lstm_cell_6/Mul:z:0#lstm_4/lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Add_1?
*lstm_4/lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2,
*lstm_4/lstm_cell_6/clip_by_value/Minimum/y?
(lstm_4/lstm_cell_6/clip_by_value/MinimumMinimumlstm_4/lstm_cell_6/Add_1:z:03lstm_4/lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222*
(lstm_4/lstm_cell_6/clip_by_value/Minimum?
"lstm_4/lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"lstm_4/lstm_cell_6/clip_by_value/y?
 lstm_4/lstm_cell_6/clip_by_valueMaximum,lstm_4/lstm_cell_6/clip_by_value/Minimum:z:0+lstm_4/lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222"
 lstm_4/lstm_cell_6/clip_by_value}
lstm_4/lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_4/lstm_cell_6/Const_2}
lstm_4/lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_4/lstm_cell_6/Const_3?
lstm_4/lstm_cell_6/Mul_1Mul!lstm_4/lstm_cell_6/split:output:1#lstm_4/lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Mul_1?
lstm_4/lstm_cell_6/Add_2AddV2lstm_4/lstm_cell_6/Mul_1:z:0#lstm_4/lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Add_2?
,lstm_4/lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_4/lstm_cell_6/clip_by_value_1/Minimum/y?
*lstm_4/lstm_cell_6/clip_by_value_1/MinimumMinimumlstm_4/lstm_cell_6/Add_2:z:05lstm_4/lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222,
*lstm_4/lstm_cell_6/clip_by_value_1/Minimum?
$lstm_4/lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_4/lstm_cell_6/clip_by_value_1/y?
"lstm_4/lstm_cell_6/clip_by_value_1Maximum.lstm_4/lstm_cell_6/clip_by_value_1/Minimum:z:0-lstm_4/lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222$
"lstm_4/lstm_cell_6/clip_by_value_1?
'lstm_4/lstm_cell_6/mul_2/ReadVariableOpReadVariableOp0lstm_4_lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02)
'lstm_4/lstm_cell_6/mul_2/ReadVariableOp?
lstm_4/lstm_cell_6/mul_2Mul&lstm_4/lstm_cell_6/clip_by_value_1:z:0/lstm_4/lstm_cell_6/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/mul_2?
lstm_4/lstm_cell_6/TanhTanh!lstm_4/lstm_cell_6/split:output:2*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Tanh?
lstm_4/lstm_cell_6/mul_3Mul$lstm_4/lstm_cell_6/clip_by_value:z:0lstm_4/lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/mul_3?
lstm_4/lstm_cell_6/add_3AddV2lstm_4/lstm_cell_6/mul_2:z:0lstm_4/lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/add_3}
lstm_4/lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_4/lstm_cell_6/Const_4}
lstm_4/lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_4/lstm_cell_6/Const_5?
lstm_4/lstm_cell_6/Mul_4Mul!lstm_4/lstm_cell_6/split:output:3#lstm_4/lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Mul_4?
lstm_4/lstm_cell_6/Add_4AddV2lstm_4/lstm_cell_6/Mul_4:z:0#lstm_4/lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Add_4?
,lstm_4/lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2.
,lstm_4/lstm_cell_6/clip_by_value_2/Minimum/y?
*lstm_4/lstm_cell_6/clip_by_value_2/MinimumMinimumlstm_4/lstm_cell_6/Add_4:z:05lstm_4/lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222,
*lstm_4/lstm_cell_6/clip_by_value_2/Minimum?
$lstm_4/lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2&
$lstm_4/lstm_cell_6/clip_by_value_2/y?
"lstm_4/lstm_cell_6/clip_by_value_2Maximum.lstm_4/lstm_cell_6/clip_by_value_2/Minimum:z:0-lstm_4/lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222$
"lstm_4/lstm_cell_6/clip_by_value_2?
lstm_4/lstm_cell_6/Tanh_1Tanhlstm_4/lstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/Tanh_1?
lstm_4/lstm_cell_6/mul_5Mul&lstm_4/lstm_cell_6/clip_by_value_2:z:0lstm_4/lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_4/lstm_cell_6/mul_5?
$lstm_4/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2&
$lstm_4/TensorArrayV2_1/element_shape?
lstm_4/TensorArrayV2_1TensorListReserve-lstm_4/TensorArrayV2_1/element_shape:output:0lstm_4/strided_slice:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_4/TensorArrayV2_1\
lstm_4/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_4/time?
lstm_4/ReadVariableOpReadVariableOp3lstm_4_lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_4/ReadVariableOp?
lstm_4/ReadVariableOp_1ReadVariableOp0lstm_4_lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02
lstm_4/ReadVariableOp_1?
lstm_4/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2!
lstm_4/while/maximum_iterationsx
lstm_4/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_4/while/loop_counter?
lstm_4/whileWhile"lstm_4/while/loop_counter:output:0(lstm_4/while/maximum_iterations:output:0lstm_4/time:output:0lstm_4/TensorArrayV2_1:handle:0lstm_4/ReadVariableOp:value:0lstm_4/ReadVariableOp_1:value:0lstm_4/strided_slice:output:0>lstm_4/TensorArrayUnstack/TensorListFromTensor:output_handle:01lstm_4_lstm_cell_6_matmul_readvariableop_resource5lstm_4_lstm_cell_6_matmul_1_readvariableop_1_resource2lstm_4_lstm_cell_6_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*:
_output_shapes(
&: : : : :22:22: : : : : *%
_read_only_resource_inputs
	
*
_stateful_parallelism( *#
bodyR
lstm_4_while_body_39766*#
condR
lstm_4_while_cond_39765*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
lstm_4/while?
7lstm_4/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7lstm_4/TensorArrayV2Stack/TensorListStack/element_shape?
)lstm_4/TensorArrayV2Stack/TensorListStackTensorListStacklstm_4/while:output:3@lstm_4/TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
element_dtype02+
)lstm_4/TensorArrayV2Stack/TensorListStack?
lstm_4/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
lstm_4/strided_slice_2/stack?
lstm_4/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2 
lstm_4/strided_slice_2/stack_1?
lstm_4/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_4/strided_slice_2/stack_2?
lstm_4/strided_slice_2StridedSlice2lstm_4/TensorArrayV2Stack/TensorListStack:tensor:0%lstm_4/strided_slice_2/stack:output:0'lstm_4/strided_slice_2/stack_1:output:0'lstm_4/strided_slice_2/stack_2:output:0*
Index0*
T0*
_output_shapes

:22*
shrink_axis_mask2
lstm_4/strided_slice_2?
lstm_4/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_4/transpose_1/perm?
lstm_4/transpose_1	Transpose2lstm_4/TensorArrayV2Stack/TensorListStack:tensor:0 lstm_4/transpose_1/perm:output:0*
T0*"
_output_shapes
:2
22
lstm_4/transpose_1t
lstm_4/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_4/runtime?
lstm_4/AssignVariableOpAssignVariableOp3lstm_4_lstm_cell_6_matmul_1_readvariableop_resourcelstm_4/while:output:4^lstm_4/ReadVariableOp+^lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_4/AssignVariableOp?
lstm_4/AssignVariableOp_1AssignVariableOp0lstm_4_lstm_cell_6_mul_2_readvariableop_resourcelstm_4/while:output:5^lstm_4/ReadVariableOp_1(^lstm_4/lstm_cell_6/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
lstm_4/AssignVariableOp_1?
 time_distributed_2/Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2"
 time_distributed_2/Reshape/shape?
time_distributed_2/ReshapeReshapelstm_4/transpose_1:y:0)time_distributed_2/Reshape/shape:output:0*
T0*
_output_shapes
:	?22
time_distributed_2/Reshape?
0time_distributed_2/dense_2/MatMul/ReadVariableOpReadVariableOp9time_distributed_2_dense_2_matmul_readvariableop_resource*
_output_shapes

:2*
dtype022
0time_distributed_2/dense_2/MatMul/ReadVariableOp?
!time_distributed_2/dense_2/MatMulMatMul#time_distributed_2/Reshape:output:08time_distributed_2/dense_2/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2#
!time_distributed_2/dense_2/MatMul?
1time_distributed_2/dense_2/BiasAdd/ReadVariableOpReadVariableOp:time_distributed_2_dense_2_biasadd_readvariableop_resource*
_output_shapes
:*
dtype023
1time_distributed_2/dense_2/BiasAdd/ReadVariableOp?
"time_distributed_2/dense_2/BiasAddBiasAdd+time_distributed_2/dense_2/MatMul:product:09time_distributed_2/dense_2/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2$
"time_distributed_2/dense_2/BiasAdd?
"time_distributed_2/Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      2$
"time_distributed_2/Reshape_1/shape?
time_distributed_2/Reshape_1Reshape+time_distributed_2/dense_2/BiasAdd:output:0+time_distributed_2/Reshape_1/shape:output:0*
T0*"
_output_shapes
:2
2
time_distributed_2/Reshape_1?
"time_distributed_2/Reshape_2/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2$
"time_distributed_2/Reshape_2/shape?
time_distributed_2/Reshape_2Reshapelstm_4/transpose_1:y:0+time_distributed_2/Reshape_2/shape:output:0*
T0*
_output_shapes
:	?22
time_distributed_2/Reshape_2{
IdentityIdentity%time_distributed_2/Reshape_1:output:0^NoOp*
T0*"
_output_shapes
:2
2

Identity?
NoOpNoOp^lstm_4/AssignVariableOp^lstm_4/AssignVariableOp_1^lstm_4/ReadVariableOp^lstm_4/ReadVariableOp_1*^lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp)^lstm_4/lstm_cell_6/MatMul/ReadVariableOp+^lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp-^lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1(^lstm_4/lstm_cell_6/mul_2/ReadVariableOp^lstm_4/while^lstm_5/AssignVariableOp^lstm_5/AssignVariableOp_1^lstm_5/ReadVariableOp^lstm_5/ReadVariableOp_1*^lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp)^lstm_5/lstm_cell_7/MatMul/ReadVariableOp+^lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp-^lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1(^lstm_5/lstm_cell_7/mul_2/ReadVariableOp^lstm_5/while2^time_distributed_2/dense_2/BiasAdd/ReadVariableOp1^time_distributed_2/dense_2/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:2
: : : : : : : : : : : : 22
lstm_4/AssignVariableOplstm_4/AssignVariableOp26
lstm_4/AssignVariableOp_1lstm_4/AssignVariableOp_12.
lstm_4/ReadVariableOplstm_4/ReadVariableOp22
lstm_4/ReadVariableOp_1lstm_4/ReadVariableOp_12V
)lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp)lstm_4/lstm_cell_6/BiasAdd/ReadVariableOp2T
(lstm_4/lstm_cell_6/MatMul/ReadVariableOp(lstm_4/lstm_cell_6/MatMul/ReadVariableOp2X
*lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp*lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp2\
,lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_1,lstm_4/lstm_cell_6/MatMul_1/ReadVariableOp_12R
'lstm_4/lstm_cell_6/mul_2/ReadVariableOp'lstm_4/lstm_cell_6/mul_2/ReadVariableOp2
lstm_4/whilelstm_4/while22
lstm_5/AssignVariableOplstm_5/AssignVariableOp26
lstm_5/AssignVariableOp_1lstm_5/AssignVariableOp_12.
lstm_5/ReadVariableOplstm_5/ReadVariableOp22
lstm_5/ReadVariableOp_1lstm_5/ReadVariableOp_12V
)lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp)lstm_5/lstm_cell_7/BiasAdd/ReadVariableOp2T
(lstm_5/lstm_cell_7/MatMul/ReadVariableOp(lstm_5/lstm_cell_7/MatMul/ReadVariableOp2X
*lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp*lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp2\
,lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_1,lstm_5/lstm_cell_7/MatMul_1/ReadVariableOp_12R
'lstm_5/lstm_cell_7/mul_2/ReadVariableOp'lstm_5/lstm_cell_7/mul_2/ReadVariableOp2
lstm_5/whilelstm_5/while2f
1time_distributed_2/dense_2/BiasAdd/ReadVariableOp1time_distributed_2/dense_2/BiasAdd/ReadVariableOp2d
0time_distributed_2/dense_2/MatMul/ReadVariableOp0time_distributed_2/dense_2/MatMul/ReadVariableOp:J F
"
_output_shapes
:2

 
_user_specified_nameinputs
?
?
while_cond_40963
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_40963___redundant_placeholder03
/while_while_cond_40963___redundant_placeholder13
/while_while_cond_40963___redundant_placeholder23
/while_while_cond_40963___redundant_placeholder3
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
?	
?
&__inference_lstm_4_layer_call_fn_41440
inputs_0
unknown:22
	unknown_0:22
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
:2?????????2*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *J
fERC
A__inference_lstm_4_layer_call_and_return_conditional_losses_373542
StatefulPartitionedCall
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*+
_output_shapes
:2?????????22

Identityh
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2?????????2: : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:U Q
+
_output_shapes
:2?????????2
"
_user_specified_name
inputs/0
?
?
+__inference_lstm_cell_6_layer_call_fn_42395

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
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_423822
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
?m
?
A__inference_lstm_4_layer_call_and_return_conditional_losses_41425

inputs=
*lstm_cell_6_matmul_readvariableop_resource:	2?>
,lstm_cell_6_matmul_1_readvariableop_resource:22A
.lstm_cell_6_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_6_biasadd_readvariableop_resource:	?;
)lstm_cell_6_mul_2_readvariableop_resource:22
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_6/BiasAdd/ReadVariableOp?!lstm_cell_6/MatMul/ReadVariableOp?#lstm_cell_6/MatMul_1/ReadVariableOp?%lstm_cell_6/MatMul_1/ReadVariableOp_1? lstm_cell_6/mul_2/ReadVariableOp?whileu
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_6/MatMul/ReadVariableOpReadVariableOp*lstm_cell_6_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02#
!lstm_cell_6/MatMul/ReadVariableOp?
lstm_cell_6/MatMulMatMulstrided_slice_1:output:0)lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/MatMul?
#lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#lstm_cell_6/MatMul_1/ReadVariableOp?
%lstm_cell_6/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_6_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_6/MatMul_1/ReadVariableOp_1?
lstm_cell_6/MatMul_1MatMul+lstm_cell_6/MatMul_1/ReadVariableOp:value:0-lstm_cell_6/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/MatMul_1?
lstm_cell_6/addAddV2lstm_cell_6/MatMul:product:0lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/add?
"lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_6_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_6/BiasAdd/ReadVariableOp?
lstm_cell_6/BiasAddBiasAddlstm_cell_6/add:z:0*lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/BiasAdd|
lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_6/split/split_dim?
lstm_cell_6/splitSplit$lstm_cell_6/split/split_dim:output:0lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_6/splitk
lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Consto
lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_1?
lstm_cell_6/MulMullstm_cell_6/split:output:0lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul?
lstm_cell_6/Add_1AddV2lstm_cell_6/Mul:z:0lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_1?
#lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_6/clip_by_value/Minimum/y?
!lstm_cell_6/clip_by_value/MinimumMinimumlstm_cell_6/Add_1:z:0,lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222#
!lstm_cell_6/clip_by_value/Minimum
lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value/y?
lstm_cell_6/clip_by_valueMaximum%lstm_cell_6/clip_by_value/Minimum:z:0$lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_valueo
lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Const_2o
lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_3?
lstm_cell_6/Mul_1Mullstm_cell_6/split:output:1lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul_1?
lstm_cell_6/Add_2AddV2lstm_cell_6/Mul_1:z:0lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_2?
%lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_6/clip_by_value_1/Minimum/y?
#lstm_cell_6/clip_by_value_1/MinimumMinimumlstm_cell_6/Add_2:z:0.lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_6/clip_by_value_1/Minimum?
lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value_1/y?
lstm_cell_6/clip_by_value_1Maximum'lstm_cell_6/clip_by_value_1/Minimum:z:0&lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_value_1?
 lstm_cell_6/mul_2/ReadVariableOpReadVariableOp)lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02"
 lstm_cell_6/mul_2/ReadVariableOp?
lstm_cell_6/mul_2Mullstm_cell_6/clip_by_value_1:z:0(lstm_cell_6/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_2q
lstm_cell_6/TanhTanhlstm_cell_6/split:output:2*
T0*
_output_shapes

:222
lstm_cell_6/Tanh?
lstm_cell_6/mul_3Mullstm_cell_6/clip_by_value:z:0lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_3?
lstm_cell_6/add_3AddV2lstm_cell_6/mul_2:z:0lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_6/add_3o
lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Const_4o
lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_5?
lstm_cell_6/Mul_4Mullstm_cell_6/split:output:3lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul_4?
lstm_cell_6/Add_4AddV2lstm_cell_6/Mul_4:z:0lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_4?
%lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_6/clip_by_value_2/Minimum/y?
#lstm_cell_6/clip_by_value_2/MinimumMinimumlstm_cell_6/Add_4:z:0.lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_6/clip_by_value_2/Minimum?
lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value_2/y?
lstm_cell_6/clip_by_value_2Maximum'lstm_cell_6/clip_by_value_2/Minimum:z:0&lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_value_2p
lstm_cell_6/Tanh_1Tanhlstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_6/Tanh_1?
lstm_cell_6/mul_5Mullstm_cell_6/clip_by_value_2:z:0lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp,lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_6_matmul_readvariableop_resource.lstm_cell_6_matmul_1_readvariableop_1_resource+lstm_cell_6_biasadd_readvariableop_resource*
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
while_body_41320*
condR
while_cond_41319*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*"
_output_shapes
:
22*
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

:22*
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
:2
22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_6_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_6/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_6_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_6/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1e
IdentityIdentitytranspose_1:y:0^NoOp*
T0*"
_output_shapes
:2
22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_6/BiasAdd/ReadVariableOp"^lstm_cell_6/MatMul/ReadVariableOp$^lstm_cell_6/MatMul_1/ReadVariableOp&^lstm_cell_6/MatMul_1/ReadVariableOp_1!^lstm_cell_6/mul_2/ReadVariableOp^while*"
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
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_6/BiasAdd/ReadVariableOp"lstm_cell_6/BiasAdd/ReadVariableOp2F
!lstm_cell_6/MatMul/ReadVariableOp!lstm_cell_6/MatMul/ReadVariableOp2J
#lstm_cell_6/MatMul_1/ReadVariableOp#lstm_cell_6/MatMul_1/ReadVariableOp2N
%lstm_cell_6/MatMul_1/ReadVariableOp_1%lstm_cell_6/MatMul_1/ReadVariableOp_12D
 lstm_cell_6/mul_2/ReadVariableOp lstm_cell_6/mul_2/ReadVariableOp2
whilewhile:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
?
?
2__inference_time_distributed_2_layer_call_fn_41564

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
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_379462
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
?
?
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_37994

inputs
dense_2_37984:2
dense_2_37986:
identity??dense_2/StatefulPartitionedCallD
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
dense_2/StatefulPartitionedCallStatefulPartitionedCallReshape:output:0dense_2_37984dense_2_37986*
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
B__inference_dense_2_layer_call_and_return_conditional_losses_379352!
dense_2/StatefulPartitionedCallq
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
	Reshape_1Reshape(dense_2/StatefulPartitionedCall:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :??????????????????2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :??????????????????2

Identityp
NoOpNoOp ^dense_2/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:??????????????????2: : 2B
dense_2/StatefulPartitionedCalldense_2/StatefulPartitionedCall:\ X
4
_output_shapes"
 :??????????????????2
 
_user_specified_nameinputs
?~
?
$sequential_2_lstm_5_while_body_36068D
@sequential_2_lstm_5_while_sequential_2_lstm_5_while_loop_counterJ
Fsequential_2_lstm_5_while_sequential_2_lstm_5_while_maximum_iterations)
%sequential_2_lstm_5_while_placeholder+
'sequential_2_lstm_5_while_placeholder_1+
'sequential_2_lstm_5_while_placeholder_2+
'sequential_2_lstm_5_while_placeholder_3A
=sequential_2_lstm_5_while_sequential_2_lstm_5_strided_slice_0
{sequential_2_lstm_5_while_tensorarrayv2read_tensorlistgetitem_sequential_2_lstm_5_tensorarrayunstack_tensorlistfromtensor_0Y
Fsequential_2_lstm_5_while_lstm_cell_7_matmul_readvariableop_resource_0:	?[
Hsequential_2_lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource_0:	2?V
Gsequential_2_lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource_0:	?&
"sequential_2_lstm_5_while_identity(
$sequential_2_lstm_5_while_identity_1(
$sequential_2_lstm_5_while_identity_2(
$sequential_2_lstm_5_while_identity_3(
$sequential_2_lstm_5_while_identity_4(
$sequential_2_lstm_5_while_identity_5?
;sequential_2_lstm_5_while_sequential_2_lstm_5_strided_slice}
ysequential_2_lstm_5_while_tensorarrayv2read_tensorlistgetitem_sequential_2_lstm_5_tensorarrayunstack_tensorlistfromtensorW
Dsequential_2_lstm_5_while_lstm_cell_7_matmul_readvariableop_resource:	?Y
Fsequential_2_lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource:	2?T
Esequential_2_lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource:	???<sequential_2/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp?;sequential_2/lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp?=sequential_2/lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp?
Ksequential_2/lstm_5/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      2M
Ksequential_2/lstm_5/while/TensorArrayV2Read/TensorListGetItem/element_shape?
=sequential_2/lstm_5/while/TensorArrayV2Read/TensorListGetItemTensorListGetItem{sequential_2_lstm_5_while_tensorarrayv2read_tensorlistgetitem_sequential_2_lstm_5_tensorarrayunstack_tensorlistfromtensor_0%sequential_2_lstm_5_while_placeholderTsequential_2/lstm_5/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02?
=sequential_2/lstm_5/while/TensorArrayV2Read/TensorListGetItem?
;sequential_2/lstm_5/while/lstm_cell_7/MatMul/ReadVariableOpReadVariableOpFsequential_2_lstm_5_while_lstm_cell_7_matmul_readvariableop_resource_0*
_output_shapes
:	?*
dtype02=
;sequential_2/lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp?
,sequential_2/lstm_5/while/lstm_cell_7/MatMulMatMulDsequential_2/lstm_5/while/TensorArrayV2Read/TensorListGetItem:item:0Csequential_2/lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2.
,sequential_2/lstm_5/while/lstm_cell_7/MatMul?
=sequential_2/lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOpHsequential_2_lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02?
=sequential_2/lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp?
.sequential_2/lstm_5/while/lstm_cell_7/MatMul_1MatMul'sequential_2_lstm_5_while_placeholder_2Esequential_2/lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?20
.sequential_2/lstm_5/while/lstm_cell_7/MatMul_1?
)sequential_2/lstm_5/while/lstm_cell_7/addAddV26sequential_2/lstm_5/while/lstm_cell_7/MatMul:product:08sequential_2/lstm_5/while/lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2+
)sequential_2/lstm_5/while/lstm_cell_7/add?
<sequential_2/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOpGsequential_2_lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02>
<sequential_2/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp?
-sequential_2/lstm_5/while/lstm_cell_7/BiasAddBiasAdd-sequential_2/lstm_5/while/lstm_cell_7/add:z:0Dsequential_2/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2/
-sequential_2/lstm_5/while/lstm_cell_7/BiasAdd?
5sequential_2/lstm_5/while/lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :27
5sequential_2/lstm_5/while/lstm_cell_7/split/split_dim?
+sequential_2/lstm_5/while/lstm_cell_7/splitSplit>sequential_2/lstm_5/while/lstm_cell_7/split/split_dim:output:06sequential_2/lstm_5/while/lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2-
+sequential_2/lstm_5/while/lstm_cell_7/split?
+sequential_2/lstm_5/while/lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2-
+sequential_2/lstm_5/while/lstm_cell_7/Const?
-sequential_2/lstm_5/while/lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2/
-sequential_2/lstm_5/while/lstm_cell_7/Const_1?
)sequential_2/lstm_5/while/lstm_cell_7/MulMul4sequential_2/lstm_5/while/lstm_cell_7/split:output:04sequential_2/lstm_5/while/lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222+
)sequential_2/lstm_5/while/lstm_cell_7/Mul?
+sequential_2/lstm_5/while/lstm_cell_7/Add_1AddV2-sequential_2/lstm_5/while/lstm_cell_7/Mul:z:06sequential_2/lstm_5/while/lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_5/while/lstm_cell_7/Add_1?
=sequential_2/lstm_5/while/lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2?
=sequential_2/lstm_5/while/lstm_cell_7/clip_by_value/Minimum/y?
;sequential_2/lstm_5/while/lstm_cell_7/clip_by_value/MinimumMinimum/sequential_2/lstm_5/while/lstm_cell_7/Add_1:z:0Fsequential_2/lstm_5/while/lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222=
;sequential_2/lstm_5/while/lstm_cell_7/clip_by_value/Minimum?
5sequential_2/lstm_5/while/lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    27
5sequential_2/lstm_5/while/lstm_cell_7/clip_by_value/y?
3sequential_2/lstm_5/while/lstm_cell_7/clip_by_valueMaximum?sequential_2/lstm_5/while/lstm_cell_7/clip_by_value/Minimum:z:0>sequential_2/lstm_5/while/lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:2225
3sequential_2/lstm_5/while/lstm_cell_7/clip_by_value?
-sequential_2/lstm_5/while/lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2/
-sequential_2/lstm_5/while/lstm_cell_7/Const_2?
-sequential_2/lstm_5/while/lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2/
-sequential_2/lstm_5/while/lstm_cell_7/Const_3?
+sequential_2/lstm_5/while/lstm_cell_7/Mul_1Mul4sequential_2/lstm_5/while/lstm_cell_7/split:output:16sequential_2/lstm_5/while/lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_5/while/lstm_cell_7/Mul_1?
+sequential_2/lstm_5/while/lstm_cell_7/Add_2AddV2/sequential_2/lstm_5/while/lstm_cell_7/Mul_1:z:06sequential_2/lstm_5/while/lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_5/while/lstm_cell_7/Add_2?
?sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2A
?sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum/y?
=sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_1/MinimumMinimum/sequential_2/lstm_5/while/lstm_cell_7/Add_2:z:0Hsequential_2/lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222?
=sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum?
7sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    29
7sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_1/y?
5sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_1MaximumAsequential_2/lstm_5/while/lstm_cell_7/clip_by_value_1/Minimum:z:0@sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:2227
5sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_1?
+sequential_2/lstm_5/while/lstm_cell_7/mul_2Mul9sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_1:z:0'sequential_2_lstm_5_while_placeholder_3*
T0*
_output_shapes

:222-
+sequential_2/lstm_5/while/lstm_cell_7/mul_2?
*sequential_2/lstm_5/while/lstm_cell_7/TanhTanh4sequential_2/lstm_5/while/lstm_cell_7/split:output:2*
T0*
_output_shapes

:222,
*sequential_2/lstm_5/while/lstm_cell_7/Tanh?
+sequential_2/lstm_5/while/lstm_cell_7/mul_3Mul7sequential_2/lstm_5/while/lstm_cell_7/clip_by_value:z:0.sequential_2/lstm_5/while/lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_5/while/lstm_cell_7/mul_3?
+sequential_2/lstm_5/while/lstm_cell_7/add_3AddV2/sequential_2/lstm_5/while/lstm_cell_7/mul_2:z:0/sequential_2/lstm_5/while/lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_5/while/lstm_cell_7/add_3?
-sequential_2/lstm_5/while/lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2/
-sequential_2/lstm_5/while/lstm_cell_7/Const_4?
-sequential_2/lstm_5/while/lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2/
-sequential_2/lstm_5/while/lstm_cell_7/Const_5?
+sequential_2/lstm_5/while/lstm_cell_7/Mul_4Mul4sequential_2/lstm_5/while/lstm_cell_7/split:output:36sequential_2/lstm_5/while/lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_5/while/lstm_cell_7/Mul_4?
+sequential_2/lstm_5/while/lstm_cell_7/Add_4AddV2/sequential_2/lstm_5/while/lstm_cell_7/Mul_4:z:06sequential_2/lstm_5/while/lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_5/while/lstm_cell_7/Add_4?
?sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2A
?sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum/y?
=sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_2/MinimumMinimum/sequential_2/lstm_5/while/lstm_cell_7/Add_4:z:0Hsequential_2/lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222?
=sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum?
7sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    29
7sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_2/y?
5sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_2MaximumAsequential_2/lstm_5/while/lstm_cell_7/clip_by_value_2/Minimum:z:0@sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:2227
5sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_2?
,sequential_2/lstm_5/while/lstm_cell_7/Tanh_1Tanh/sequential_2/lstm_5/while/lstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222.
,sequential_2/lstm_5/while/lstm_cell_7/Tanh_1?
+sequential_2/lstm_5/while/lstm_cell_7/mul_5Mul9sequential_2/lstm_5/while/lstm_cell_7/clip_by_value_2:z:00sequential_2/lstm_5/while/lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222-
+sequential_2/lstm_5/while/lstm_cell_7/mul_5?
>sequential_2/lstm_5/while/TensorArrayV2Write/TensorListSetItemTensorListSetItem'sequential_2_lstm_5_while_placeholder_1%sequential_2_lstm_5_while_placeholder/sequential_2/lstm_5/while/lstm_cell_7/mul_5:z:0*
_output_shapes
: *
element_dtype02@
>sequential_2/lstm_5/while/TensorArrayV2Write/TensorListSetItem?
sequential_2/lstm_5/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2!
sequential_2/lstm_5/while/add/y?
sequential_2/lstm_5/while/addAddV2%sequential_2_lstm_5_while_placeholder(sequential_2/lstm_5/while/add/y:output:0*
T0*
_output_shapes
: 2
sequential_2/lstm_5/while/add?
!sequential_2/lstm_5/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2#
!sequential_2/lstm_5/while/add_1/y?
sequential_2/lstm_5/while/add_1AddV2@sequential_2_lstm_5_while_sequential_2_lstm_5_while_loop_counter*sequential_2/lstm_5/while/add_1/y:output:0*
T0*
_output_shapes
: 2!
sequential_2/lstm_5/while/add_1?
"sequential_2/lstm_5/while/IdentityIdentity#sequential_2/lstm_5/while/add_1:z:0^sequential_2/lstm_5/while/NoOp*
T0*
_output_shapes
: 2$
"sequential_2/lstm_5/while/Identity?
$sequential_2/lstm_5/while/Identity_1IdentityFsequential_2_lstm_5_while_sequential_2_lstm_5_while_maximum_iterations^sequential_2/lstm_5/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_2/lstm_5/while/Identity_1?
$sequential_2/lstm_5/while/Identity_2Identity!sequential_2/lstm_5/while/add:z:0^sequential_2/lstm_5/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_2/lstm_5/while/Identity_2?
$sequential_2/lstm_5/while/Identity_3IdentityNsequential_2/lstm_5/while/TensorArrayV2Write/TensorListSetItem:output_handle:0^sequential_2/lstm_5/while/NoOp*
T0*
_output_shapes
: 2&
$sequential_2/lstm_5/while/Identity_3?
$sequential_2/lstm_5/while/Identity_4Identity/sequential_2/lstm_5/while/lstm_cell_7/mul_5:z:0^sequential_2/lstm_5/while/NoOp*
T0*
_output_shapes

:222&
$sequential_2/lstm_5/while/Identity_4?
$sequential_2/lstm_5/while/Identity_5Identity/sequential_2/lstm_5/while/lstm_cell_7/add_3:z:0^sequential_2/lstm_5/while/NoOp*
T0*
_output_shapes

:222&
$sequential_2/lstm_5/while/Identity_5?
sequential_2/lstm_5/while/NoOpNoOp=^sequential_2/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp<^sequential_2/lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp>^sequential_2/lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2 
sequential_2/lstm_5/while/NoOp"Q
"sequential_2_lstm_5_while_identity+sequential_2/lstm_5/while/Identity:output:0"U
$sequential_2_lstm_5_while_identity_1-sequential_2/lstm_5/while/Identity_1:output:0"U
$sequential_2_lstm_5_while_identity_2-sequential_2/lstm_5/while/Identity_2:output:0"U
$sequential_2_lstm_5_while_identity_3-sequential_2/lstm_5/while/Identity_3:output:0"U
$sequential_2_lstm_5_while_identity_4-sequential_2/lstm_5/while/Identity_4:output:0"U
$sequential_2_lstm_5_while_identity_5-sequential_2/lstm_5/while/Identity_5:output:0"?
Esequential_2_lstm_5_while_lstm_cell_7_biasadd_readvariableop_resourceGsequential_2_lstm_5_while_lstm_cell_7_biasadd_readvariableop_resource_0"?
Fsequential_2_lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resourceHsequential_2_lstm_5_while_lstm_cell_7_matmul_1_readvariableop_resource_0"?
Dsequential_2_lstm_5_while_lstm_cell_7_matmul_readvariableop_resourceFsequential_2_lstm_5_while_lstm_cell_7_matmul_readvariableop_resource_0"|
;sequential_2_lstm_5_while_sequential_2_lstm_5_strided_slice=sequential_2_lstm_5_while_sequential_2_lstm_5_strided_slice_0"?
ysequential_2_lstm_5_while_tensorarrayv2read_tensorlistgetitem_sequential_2_lstm_5_tensorarrayunstack_tensorlistfromtensor{sequential_2_lstm_5_while_tensorarrayv2read_tensorlistgetitem_sequential_2_lstm_5_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2|
<sequential_2/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp<sequential_2/lstm_5/while/lstm_cell_7/BiasAdd/ReadVariableOp2z
;sequential_2/lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp;sequential_2/lstm_5/while/lstm_cell_7/MatMul/ReadVariableOp2~
=sequential_2/lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp=sequential_2/lstm_5/while/lstm_cell_7/MatMul_1/ReadVariableOp: 
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
?,
?
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41701

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
:	2?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOps
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2	
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
(:22:22:22:22*
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
 *  ??2
clip_by_value/Minimum/y?
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
clip_by_value/y?
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
 *  ??2
clip_by_value_1/Minimum/y?
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
clip_by_value_1/y?
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
 *  ??2
clip_by_value_2/Minimum/y?
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
clip_by_value_2/y?
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
?
?
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_38448

inputs8
&dense_2_matmul_readvariableop_resource:25
'dense_2_biasadd_readvariableop_resource:
identity??dense_2/BiasAdd/ReadVariableOp?dense_2/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2
Reshape/shapeg
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes
:	?22	
Reshape?
dense_2/MatMul/ReadVariableOpReadVariableOp&dense_2_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_2/MatMul/ReadVariableOp?
dense_2/MatMulMatMulReshape:output:0%dense_2/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
dense_2/MatMul?
dense_2/BiasAdd/ReadVariableOpReadVariableOp'dense_2_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_2/BiasAdd/ReadVariableOp?
dense_2/BiasAddBiasAdddense_2/MatMul:product:0&dense_2/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
dense_2/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      2
Reshape_1/shape?
	Reshape_1Reshapedense_2/BiasAdd:output:0Reshape_1/shape:output:0*
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

Identity?
NoOpNoOp^dense_2/BiasAdd/ReadVariableOp^dense_2/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:2
2: : 2@
dense_2/BiasAdd/ReadVariableOpdense_2/BiasAdd/ReadVariableOp2>
dense_2/MatMul/ReadVariableOpdense_2/MatMul/ReadVariableOp:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
?,
?
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_37442

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
:	2?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2	
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
(:22:22:22:22*
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
 *  ??2
clip_by_value/Minimum/y?
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
clip_by_value/y?
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
 *  ??2
clip_by_value_1/Minimum/y?
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
clip_by_value_1/y?
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
 *  ??2
clip_by_value_2/Minimum/y?
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
clip_by_value_2/y?
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
?m
?
A__inference_lstm_4_layer_call_and_return_conditional_losses_41069
inputs_0=
*lstm_cell_6_matmul_readvariableop_resource:	2?>
,lstm_cell_6_matmul_1_readvariableop_resource:22A
.lstm_cell_6_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_6_biasadd_readvariableop_resource:	?;
)lstm_cell_6_mul_2_readvariableop_resource:22
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_6/BiasAdd/ReadVariableOp?!lstm_cell_6/MatMul/ReadVariableOp?#lstm_cell_6/MatMul_1/ReadVariableOp?%lstm_cell_6/MatMul_1/ReadVariableOp_1? lstm_cell_6/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:?????????222
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
valueB"2   2   27
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

:22*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_6/MatMul/ReadVariableOpReadVariableOp*lstm_cell_6_matmul_readvariableop_resource*
_output_shapes
:	2?*
dtype02#
!lstm_cell_6/MatMul/ReadVariableOp?
lstm_cell_6/MatMulMatMulstrided_slice_1:output:0)lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/MatMul?
#lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#lstm_cell_6/MatMul_1/ReadVariableOp?
%lstm_cell_6/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_6_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_6/MatMul_1/ReadVariableOp_1?
lstm_cell_6/MatMul_1MatMul+lstm_cell_6/MatMul_1/ReadVariableOp:value:0-lstm_cell_6/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/MatMul_1?
lstm_cell_6/addAddV2lstm_cell_6/MatMul:product:0lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/add?
"lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_6_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_6/BiasAdd/ReadVariableOp?
lstm_cell_6/BiasAddBiasAddlstm_cell_6/add:z:0*lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_6/BiasAdd|
lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_6/split/split_dim?
lstm_cell_6/splitSplit$lstm_cell_6/split/split_dim:output:0lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_6/splitk
lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Consto
lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_1?
lstm_cell_6/MulMullstm_cell_6/split:output:0lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul?
lstm_cell_6/Add_1AddV2lstm_cell_6/Mul:z:0lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_1?
#lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_6/clip_by_value/Minimum/y?
!lstm_cell_6/clip_by_value/MinimumMinimumlstm_cell_6/Add_1:z:0,lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222#
!lstm_cell_6/clip_by_value/Minimum
lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value/y?
lstm_cell_6/clip_by_valueMaximum%lstm_cell_6/clip_by_value/Minimum:z:0$lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_valueo
lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Const_2o
lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_3?
lstm_cell_6/Mul_1Mullstm_cell_6/split:output:1lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul_1?
lstm_cell_6/Add_2AddV2lstm_cell_6/Mul_1:z:0lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_2?
%lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_6/clip_by_value_1/Minimum/y?
#lstm_cell_6/clip_by_value_1/MinimumMinimumlstm_cell_6/Add_2:z:0.lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_6/clip_by_value_1/Minimum?
lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value_1/y?
lstm_cell_6/clip_by_value_1Maximum'lstm_cell_6/clip_by_value_1/Minimum:z:0&lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_value_1?
 lstm_cell_6/mul_2/ReadVariableOpReadVariableOp)lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02"
 lstm_cell_6/mul_2/ReadVariableOp?
lstm_cell_6/mul_2Mullstm_cell_6/clip_by_value_1:z:0(lstm_cell_6/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_2q
lstm_cell_6/TanhTanhlstm_cell_6/split:output:2*
T0*
_output_shapes

:222
lstm_cell_6/Tanh?
lstm_cell_6/mul_3Mullstm_cell_6/clip_by_value:z:0lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_3?
lstm_cell_6/add_3AddV2lstm_cell_6/mul_2:z:0lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_6/add_3o
lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_6/Const_4o
lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_6/Const_5?
lstm_cell_6/Mul_4Mullstm_cell_6/split:output:3lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Mul_4?
lstm_cell_6/Add_4AddV2lstm_cell_6/Mul_4:z:0lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_6/Add_4?
%lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_6/clip_by_value_2/Minimum/y?
#lstm_cell_6/clip_by_value_2/MinimumMinimumlstm_cell_6/Add_4:z:0.lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_6/clip_by_value_2/Minimum?
lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_6/clip_by_value_2/y?
lstm_cell_6/clip_by_value_2Maximum'lstm_cell_6/clip_by_value_2/Minimum:z:0&lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_6/clip_by_value_2p
lstm_cell_6/Tanh_1Tanhlstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_6/Tanh_1?
lstm_cell_6/mul_5Mullstm_cell_6/clip_by_value_2:z:0lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_6/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp,lstm_cell_6_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_6_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_6_matmul_readvariableop_resource.lstm_cell_6_matmul_1_readvariableop_1_resource+lstm_cell_6_biasadd_readvariableop_resource*
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
while_body_40964*
condR
while_cond_40963*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????22*
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

:22*
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
:2?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_6_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_6/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_6_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_6/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_6/BiasAdd/ReadVariableOp"^lstm_cell_6/MatMul/ReadVariableOp$^lstm_cell_6/MatMul_1/ReadVariableOp&^lstm_cell_6/MatMul_1/ReadVariableOp_1!^lstm_cell_6/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2?????????2: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_6/BiasAdd/ReadVariableOp"lstm_cell_6/BiasAdd/ReadVariableOp2F
!lstm_cell_6/MatMul/ReadVariableOp!lstm_cell_6/MatMul/ReadVariableOp2J
#lstm_cell_6/MatMul_1/ReadVariableOp#lstm_cell_6/MatMul_1/ReadVariableOp2N
%lstm_cell_6/MatMul_1/ReadVariableOp_1%lstm_cell_6/MatMul_1/ReadVariableOp_12D
 lstm_cell_6/mul_2/ReadVariableOp lstm_cell_6/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:2?????????2
"
_user_specified_name
inputs/0
?,
?
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_37308

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
:	2?2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes
:	2?*
dtype02
MatMul_1/ReadVariableOpq
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2	
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
(:22:22:22:22*
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
 *  ??2
clip_by_value/Minimum/y?
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
clip_by_value/y?
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
 *  ??2
clip_by_value_1/Minimum/y?
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
clip_by_value_1/y?
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
 *  ??2
clip_by_value_2/Minimum/y?
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
clip_by_value_2/y?
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
?m
?
A__inference_lstm_5_layer_call_and_return_conditional_losses_40297
inputs_0=
*lstm_cell_7_matmul_readvariableop_resource:	?>
,lstm_cell_7_matmul_1_readvariableop_resource:22A
.lstm_cell_7_matmul_1_readvariableop_1_resource:	2?:
+lstm_cell_7_biasadd_readvariableop_resource:	?;
)lstm_cell_7_mul_2_readvariableop_resource:22
identity??AssignVariableOp?AssignVariableOp_1?ReadVariableOp?ReadVariableOp_1?"lstm_cell_7/BiasAdd/ReadVariableOp?!lstm_cell_7/MatMul/ReadVariableOp?#lstm_cell_7/MatMul_1/ReadVariableOp?%lstm_cell_7/MatMul_1/ReadVariableOp_1? lstm_cell_7/mul_2/ReadVariableOp?whileu
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm|
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*+
_output_shapes
:?????????22
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
valueB"2      27
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

:2*
shrink_axis_mask2
strided_slice_1?
!lstm_cell_7/MatMul/ReadVariableOpReadVariableOp*lstm_cell_7_matmul_readvariableop_resource*
_output_shapes
:	?*
dtype02#
!lstm_cell_7/MatMul/ReadVariableOp?
lstm_cell_7/MatMulMatMulstrided_slice_1:output:0)lstm_cell_7/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/MatMul?
#lstm_cell_7/MatMul_1/ReadVariableOpReadVariableOp,lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02%
#lstm_cell_7/MatMul_1/ReadVariableOp?
%lstm_cell_7/MatMul_1/ReadVariableOp_1ReadVariableOp.lstm_cell_7_matmul_1_readvariableop_1_resource*
_output_shapes
:	2?*
dtype02'
%lstm_cell_7/MatMul_1/ReadVariableOp_1?
lstm_cell_7/MatMul_1MatMul+lstm_cell_7/MatMul_1/ReadVariableOp:value:0-lstm_cell_7/MatMul_1/ReadVariableOp_1:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/MatMul_1?
lstm_cell_7/addAddV2lstm_cell_7/MatMul:product:0lstm_cell_7/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/add?
"lstm_cell_7/BiasAdd/ReadVariableOpReadVariableOp+lstm_cell_7_biasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02$
"lstm_cell_7/BiasAdd/ReadVariableOp?
lstm_cell_7/BiasAddBiasAddlstm_cell_7/add:z:0*lstm_cell_7/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
lstm_cell_7/BiasAdd|
lstm_cell_7/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_7/split/split_dim?
lstm_cell_7/splitSplit$lstm_cell_7/split/split_dim:output:0lstm_cell_7/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
lstm_cell_7/splitk
lstm_cell_7/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Consto
lstm_cell_7/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_1?
lstm_cell_7/MulMullstm_cell_7/split:output:0lstm_cell_7/Const:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul?
lstm_cell_7/Add_1AddV2lstm_cell_7/Mul:z:0lstm_cell_7/Const_1:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_1?
#lstm_cell_7/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2%
#lstm_cell_7/clip_by_value/Minimum/y?
!lstm_cell_7/clip_by_value/MinimumMinimumlstm_cell_7/Add_1:z:0,lstm_cell_7/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222#
!lstm_cell_7/clip_by_value/Minimum
lstm_cell_7/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value/y?
lstm_cell_7/clip_by_valueMaximum%lstm_cell_7/clip_by_value/Minimum:z:0$lstm_cell_7/clip_by_value/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_valueo
lstm_cell_7/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Const_2o
lstm_cell_7/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_3?
lstm_cell_7/Mul_1Mullstm_cell_7/split:output:1lstm_cell_7/Const_2:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul_1?
lstm_cell_7/Add_2AddV2lstm_cell_7/Mul_1:z:0lstm_cell_7/Const_3:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_2?
%lstm_cell_7/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_7/clip_by_value_1/Minimum/y?
#lstm_cell_7/clip_by_value_1/MinimumMinimumlstm_cell_7/Add_2:z:0.lstm_cell_7/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_7/clip_by_value_1/Minimum?
lstm_cell_7/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value_1/y?
lstm_cell_7/clip_by_value_1Maximum'lstm_cell_7/clip_by_value_1/Minimum:z:0&lstm_cell_7/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_value_1?
 lstm_cell_7/mul_2/ReadVariableOpReadVariableOp)lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
dtype02"
 lstm_cell_7/mul_2/ReadVariableOp?
lstm_cell_7/mul_2Mullstm_cell_7/clip_by_value_1:z:0(lstm_cell_7/mul_2/ReadVariableOp:value:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_2q
lstm_cell_7/TanhTanhlstm_cell_7/split:output:2*
T0*
_output_shapes

:222
lstm_cell_7/Tanh?
lstm_cell_7/mul_3Mullstm_cell_7/clip_by_value:z:0lstm_cell_7/Tanh:y:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_3?
lstm_cell_7/add_3AddV2lstm_cell_7/mul_2:z:0lstm_cell_7/mul_3:z:0*
T0*
_output_shapes

:222
lstm_cell_7/add_3o
lstm_cell_7/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
lstm_cell_7/Const_4o
lstm_cell_7/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
lstm_cell_7/Const_5?
lstm_cell_7/Mul_4Mullstm_cell_7/split:output:3lstm_cell_7/Const_4:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Mul_4?
lstm_cell_7/Add_4AddV2lstm_cell_7/Mul_4:z:0lstm_cell_7/Const_5:output:0*
T0*
_output_shapes

:222
lstm_cell_7/Add_4?
%lstm_cell_7/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2'
%lstm_cell_7/clip_by_value_2/Minimum/y?
#lstm_cell_7/clip_by_value_2/MinimumMinimumlstm_cell_7/Add_4:z:0.lstm_cell_7/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222%
#lstm_cell_7/clip_by_value_2/Minimum?
lstm_cell_7/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_cell_7/clip_by_value_2/y?
lstm_cell_7/clip_by_value_2Maximum'lstm_cell_7/clip_by_value_2/Minimum:z:0&lstm_cell_7/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222
lstm_cell_7/clip_by_value_2p
lstm_cell_7/Tanh_1Tanhlstm_cell_7/add_3:z:0*
T0*
_output_shapes

:222
lstm_cell_7/Tanh_1?
lstm_cell_7/mul_5Mullstm_cell_7/clip_by_value_2:z:0lstm_cell_7/Tanh_1:y:0*
T0*
_output_shapes

:222
lstm_cell_7/mul_5?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   2
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
ReadVariableOpReadVariableOp,lstm_cell_7_matmul_1_readvariableop_resource*
_output_shapes

:22*
dtype02
ReadVariableOp?
ReadVariableOp_1ReadVariableOp)lstm_cell_7_mul_2_readvariableop_resource*
_output_shapes

:22*
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
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0ReadVariableOp:value:0ReadVariableOp_1:value:0strided_slice:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0*lstm_cell_7_matmul_readvariableop_resource.lstm_cell_7_matmul_1_readvariableop_1_resource+lstm_cell_7_biasadd_readvariableop_resource*
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
while_body_40192*
condR
while_cond_40191*9
output_shapes(
&: : : : :22:22: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:?????????22*
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

:22*
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
:2?????????22
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
AssignVariableOpAssignVariableOp,lstm_cell_7_matmul_1_readvariableop_resourcewhile:output:4^ReadVariableOp$^lstm_cell_7/MatMul_1/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp?
AssignVariableOp_1AssignVariableOp)lstm_cell_7_mul_2_readvariableop_resourcewhile:output:5^ReadVariableOp_1!^lstm_cell_7/mul_2/ReadVariableOp*
_output_shapes
 *
dtype02
AssignVariableOp_1n
IdentityIdentitytranspose_1:y:0^NoOp*
T0*+
_output_shapes
:2?????????22

Identity?
NoOpNoOp^AssignVariableOp^AssignVariableOp_1^ReadVariableOp^ReadVariableOp_1#^lstm_cell_7/BiasAdd/ReadVariableOp"^lstm_cell_7/MatMul/ReadVariableOp$^lstm_cell_7/MatMul_1/ReadVariableOp&^lstm_cell_7/MatMul_1/ReadVariableOp_1!^lstm_cell_7/mul_2/ReadVariableOp^while*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*4
_input_shapes#
!:2?????????: : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12 
ReadVariableOpReadVariableOp2$
ReadVariableOp_1ReadVariableOp_12H
"lstm_cell_7/BiasAdd/ReadVariableOp"lstm_cell_7/BiasAdd/ReadVariableOp2F
!lstm_cell_7/MatMul/ReadVariableOp!lstm_cell_7/MatMul/ReadVariableOp2J
#lstm_cell_7/MatMul_1/ReadVariableOp#lstm_cell_7/MatMul_1/ReadVariableOp2N
%lstm_cell_7/MatMul_1/ReadVariableOp_1%lstm_cell_7/MatMul_1/ReadVariableOp_12D
 lstm_cell_7/mul_2/ReadVariableOp lstm_cell_7/mul_2/ReadVariableOp2
whilewhile:U Q
+
_output_shapes
:2?????????
"
_user_specified_name
inputs/0
?0
?
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_37533

inputs
states:22
states_1:221
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
:	2?2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:22*
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
:	2?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2	
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
(:22:22:22:22*
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
 *  ??2
clip_by_value/Minimum/y?
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
clip_by_value/y?
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
 *  ??2
clip_by_value_1/Minimum/y?
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
clip_by_value_1/y?
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
 *  ??2
clip_by_value_2/Minimum/y?
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
clip_by_value_2/y?
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
?
?
while_cond_40369
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_40369___redundant_placeholder03
/while_while_cond_40369___redundant_placeholder13
/while_while_cond_40369___redundant_placeholder23
/while_while_cond_40369___redundant_placeholder3
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
?
?
+__inference_lstm_cell_7_layer_call_fn_41993

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
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_419802
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
?.
?
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41648

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
:	2?2
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
?
?
$sequential_2_lstm_4_while_cond_36241D
@sequential_2_lstm_4_while_sequential_2_lstm_4_while_loop_counterJ
Fsequential_2_lstm_4_while_sequential_2_lstm_4_while_maximum_iterations)
%sequential_2_lstm_4_while_placeholder+
'sequential_2_lstm_4_while_placeholder_1+
'sequential_2_lstm_4_while_placeholder_2+
'sequential_2_lstm_4_while_placeholder_3D
@sequential_2_lstm_4_while_less_sequential_2_lstm_4_strided_slice[
Wsequential_2_lstm_4_while_sequential_2_lstm_4_while_cond_36241___redundant_placeholder0[
Wsequential_2_lstm_4_while_sequential_2_lstm_4_while_cond_36241___redundant_placeholder1[
Wsequential_2_lstm_4_while_sequential_2_lstm_4_while_cond_36241___redundant_placeholder2[
Wsequential_2_lstm_4_while_sequential_2_lstm_4_while_cond_36241___redundant_placeholder3&
"sequential_2_lstm_4_while_identity
?
sequential_2/lstm_4/while/LessLess%sequential_2_lstm_4_while_placeholder@sequential_2_lstm_4_while_less_sequential_2_lstm_4_strided_slice*
T0*
_output_shapes
: 2 
sequential_2/lstm_4/while/Less?
"sequential_2/lstm_4/while/IdentityIdentity"sequential_2/lstm_4/while/Less:z:0*
T0
*
_output_shapes
: 2$
"sequential_2/lstm_4/while/Identity"Q
"sequential_2_lstm_4_while_identity+sequential_2/lstm_4/while/Identity:output:0*(
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
?
?
while_cond_40785
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_40785___redundant_placeholder03
/while_while_cond_40785___redundant_placeholder13
/while_while_cond_40785___redundant_placeholder23
/while_while_cond_40785___redundant_placeholder3
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
?.
?
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41811

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
:	2?2
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
?X
?
while_body_41142
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_6_matmul_readvariableop_resource_0:	2?G
4while_lstm_cell_6_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_6_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_6_matmul_readvariableop_resource:	2?E
2while_lstm_cell_6_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_6_biasadd_readvariableop_resource:	???(while/lstm_cell_6/BiasAdd/ReadVariableOp?'while/lstm_cell_6/MatMul/ReadVariableOp?)while/lstm_cell_6/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_6/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_6_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02)
'while/lstm_cell_6/MatMul/ReadVariableOp?
while/lstm_cell_6/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/MatMul?
)while/lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_6_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_6/MatMul_1/ReadVariableOp?
while/lstm_cell_6/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_6/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/MatMul_1?
while/lstm_cell_6/addAddV2"while/lstm_cell_6/MatMul:product:0$while/lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/add?
(while/lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_6_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_6/BiasAdd/ReadVariableOp?
while/lstm_cell_6/BiasAddBiasAddwhile/lstm_cell_6/add:z:00while/lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/BiasAdd?
!while/lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_6/split/split_dim?
while/lstm_cell_6/splitSplit*while/lstm_cell_6/split/split_dim:output:0"while/lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_6/splitw
while/lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const{
while/lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_1?
while/lstm_cell_6/MulMul while/lstm_cell_6/split:output:0 while/lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul?
while/lstm_cell_6/Add_1AddV2while/lstm_cell_6/Mul:z:0"while/lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_1?
)while/lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_6/clip_by_value/Minimum/y?
'while/lstm_cell_6/clip_by_value/MinimumMinimumwhile/lstm_cell_6/Add_1:z:02while/lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222)
'while/lstm_cell_6/clip_by_value/Minimum?
!while/lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_6/clip_by_value/y?
while/lstm_cell_6/clip_by_valueMaximum+while/lstm_cell_6/clip_by_value/Minimum:z:0*while/lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222!
while/lstm_cell_6/clip_by_value{
while/lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const_2{
while/lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_3?
while/lstm_cell_6/Mul_1Mul while/lstm_cell_6/split:output:1"while/lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul_1?
while/lstm_cell_6/Add_2AddV2while/lstm_cell_6/Mul_1:z:0"while/lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_2?
+while/lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_6/clip_by_value_1/Minimum/y?
)while/lstm_cell_6/clip_by_value_1/MinimumMinimumwhile/lstm_cell_6/Add_2:z:04while/lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_6/clip_by_value_1/Minimum?
#while/lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_6/clip_by_value_1/y?
!while/lstm_cell_6/clip_by_value_1Maximum-while/lstm_cell_6/clip_by_value_1/Minimum:z:0,while/lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_6/clip_by_value_1?
while/lstm_cell_6/mul_2Mul%while/lstm_cell_6/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_2?
while/lstm_cell_6/TanhTanh while/lstm_cell_6/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_6/Tanh?
while/lstm_cell_6/mul_3Mul#while/lstm_cell_6/clip_by_value:z:0while/lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_3?
while/lstm_cell_6/add_3AddV2while/lstm_cell_6/mul_2:z:0while/lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_6/add_3{
while/lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const_4{
while/lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_5?
while/lstm_cell_6/Mul_4Mul while/lstm_cell_6/split:output:3"while/lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul_4?
while/lstm_cell_6/Add_4AddV2while/lstm_cell_6/Mul_4:z:0"while/lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_4?
+while/lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_6/clip_by_value_2/Minimum/y?
)while/lstm_cell_6/clip_by_value_2/MinimumMinimumwhile/lstm_cell_6/Add_4:z:04while/lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_6/clip_by_value_2/Minimum?
#while/lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_6/clip_by_value_2/y?
!while/lstm_cell_6/clip_by_value_2Maximum-while/lstm_cell_6/clip_by_value_2/Minimum:z:0,while/lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_6/clip_by_value_2?
while/lstm_cell_6/Tanh_1Tanhwhile/lstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Tanh_1?
while/lstm_cell_6/mul_5Mul%while/lstm_cell_6/clip_by_value_2:z:0while/lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_6/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_6/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_6/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_6/BiasAdd/ReadVariableOp(^while/lstm_cell_6/MatMul/ReadVariableOp*^while/lstm_cell_6/MatMul_1/ReadVariableOp*"
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
1while_lstm_cell_6_biasadd_readvariableop_resource3while_lstm_cell_6_biasadd_readvariableop_resource_0"j
2while_lstm_cell_6_matmul_1_readvariableop_resource4while_lstm_cell_6_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_6_matmul_readvariableop_resource2while_lstm_cell_6_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2T
(while/lstm_cell_6/BiasAdd/ReadVariableOp(while/lstm_cell_6/BiasAdd/ReadVariableOp2R
'while/lstm_cell_6/MatMul/ReadVariableOp'while/lstm_cell_6/MatMul/ReadVariableOp2V
)while/lstm_cell_6/MatMul_1/ReadVariableOp)while/lstm_cell_6/MatMul_1/ReadVariableOp: 
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
?
?
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_37946

inputs
dense_2_37936:2
dense_2_37938:
identity??dense_2/StatefulPartitionedCallD
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
dense_2/StatefulPartitionedCallStatefulPartitionedCallReshape:output:0dense_2_37936dense_2_37938*
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
B__inference_dense_2_layer_call_and_return_conditional_losses_379352!
dense_2/StatefulPartitionedCallq
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
	Reshape_1Reshape(dense_2/StatefulPartitionedCall:output:0Reshape_1/shape:output:0*
T0*4
_output_shapes"
 :??????????????????2
	Reshape_1z
IdentityIdentityReshape_1:output:0^NoOp*
T0*4
_output_shapes"
 :??????????????????2

Identityp
NoOpNoOp ^dense_2/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*7
_input_shapes&
$:??????????????????2: : 2B
dense_2/StatefulPartitionedCalldense_2/StatefulPartitionedCall:\ X
4
_output_shapes"
 :??????????????????2
 
_user_specified_nameinputs
?0
?
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_37212

inputs
states:22
states_1:221
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
:	2?2
MatMuly
MatMul_1/ReadVariableOpReadVariableOpstates*
_output_shapes

:22*
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
:	2?2

MatMul_1c
addAddV2MatMul:product:0MatMul_1:product:0*
T0*
_output_shapes
:	2?2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:?*
dtype02
BiasAdd/ReadVariableOpp
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2	
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
(:22:22:22:22*
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
 *  ??2
clip_by_value/Minimum/y?
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
clip_by_value/y?
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
 *  ??2
clip_by_value_1/Minimum/y?
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
clip_by_value_1/y?
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
 *  ??2
clip_by_value_2/Minimum/y?
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
clip_by_value_2/y?
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
?	
?
lstm_4_while_cond_39765*
&lstm_4_while_lstm_4_while_loop_counter0
,lstm_4_while_lstm_4_while_maximum_iterations
lstm_4_while_placeholder
lstm_4_while_placeholder_1
lstm_4_while_placeholder_2
lstm_4_while_placeholder_3*
&lstm_4_while_less_lstm_4_strided_sliceA
=lstm_4_while_lstm_4_while_cond_39765___redundant_placeholder0A
=lstm_4_while_lstm_4_while_cond_39765___redundant_placeholder1A
=lstm_4_while_lstm_4_while_cond_39765___redundant_placeholder2A
=lstm_4_while_lstm_4_while_cond_39765___redundant_placeholder3
lstm_4_while_identity
?
lstm_4/while/LessLesslstm_4_while_placeholder&lstm_4_while_less_lstm_4_strided_slice*
T0*
_output_shapes
: 2
lstm_4/while/Lessr
lstm_4/while/IdentityIdentitylstm_4/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_4/while/Identity"7
lstm_4_while_identitylstm_4/while/Identity:output:0*(
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
?
?
+__inference_lstm_cell_7_layer_call_fn_41828

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
:22:22:22*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *O
fJRH
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_365322
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
?
?
+__inference_lstm_cell_7_layer_call_fn_41919

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
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_419062
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
?
?
while_cond_37231
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice3
/while_while_cond_37231___redundant_placeholder03
/while_while_cond_37231___redundant_placeholder13
/while_while_cond_37231___redundant_placeholder23
/while_while_cond_37231___redundant_placeholder3
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
?$
?
while_body_37232
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0,
while_lstm_cell_6_37309_0:	2?,
while_lstm_cell_6_37311_0:	2?(
while_lstm_cell_6_37313_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor*
while_lstm_cell_6_37309:	2?*
while_lstm_cell_6_37311:	2?&
while_lstm_cell_6_37313:	???)while/lstm_cell_6/StatefulPartitionedCall?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
)while/lstm_cell_6/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_6_37309_0while_lstm_cell_6_37311_0while_lstm_cell_6_37313_0*
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
GPU 2J 8? *O
fJRH
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_373082+
)while/lstm_cell_6/StatefulPartitionedCall?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder2while/lstm_cell_6/StatefulPartitionedCall:output:0*
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
while/Identity_4Identity2while/lstm_cell_6/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identity2while/lstm_cell_6/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp*^while/lstm_cell_6/StatefulPartitionedCall*"
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
while_lstm_cell_6_37309while_lstm_cell_6_37309_0"4
while_lstm_cell_6_37311while_lstm_cell_6_37311_0"4
while_lstm_cell_6_37313while_lstm_cell_6_37313_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_6/StatefulPartitionedCall)while/lstm_cell_6/StatefulPartitionedCall: 
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
?$
?
while_body_36826
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0,
while_lstm_cell_7_36850_0:	?,
while_lstm_cell_7_36852_0:	2?(
while_lstm_cell_7_36854_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor*
while_lstm_cell_7_36850:	?*
while_lstm_cell_7_36852:	2?&
while_lstm_cell_7_36854:	???)while/lstm_cell_7/StatefulPartitionedCall?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2      29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:2*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
)while/lstm_cell_7/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_7_36850_0while_lstm_cell_7_36852_0while_lstm_cell_7_36854_0*
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
GPU 2J 8? *O
fJRH
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_366662+
)while/lstm_cell_7/StatefulPartitionedCall?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder2while/lstm_cell_7/StatefulPartitionedCall:output:0*
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
while/Identity_4Identity2while/lstm_cell_7/StatefulPartitionedCall:output:1^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identity2while/lstm_cell_7/StatefulPartitionedCall:output:2^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp*^while/lstm_cell_7/StatefulPartitionedCall*"
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
while_lstm_cell_7_36850while_lstm_cell_7_36850_0"4
while_lstm_cell_7_36852while_lstm_cell_7_36852_0"4
while_lstm_cell_7_36854while_lstm_cell_7_36854_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2V
)while/lstm_cell_7/StatefulPartitionedCall)while/lstm_cell_7/StatefulPartitionedCall: 
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
?
?
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_38509

inputs8
&dense_2_matmul_readvariableop_resource:25
'dense_2_biasadd_readvariableop_resource:
identity??dense_2/BiasAdd/ReadVariableOp?dense_2/MatMul/ReadVariableOpo
Reshape/shapeConst*
_output_shapes
:*
dtype0*
valueB"????2   2
Reshape/shapeg
ReshapeReshapeinputsReshape/shape:output:0*
T0*
_output_shapes
:	?22	
Reshape?
dense_2/MatMul/ReadVariableOpReadVariableOp&dense_2_matmul_readvariableop_resource*
_output_shapes

:2*
dtype02
dense_2/MatMul/ReadVariableOp?
dense_2/MatMulMatMulReshape:output:0%dense_2/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
dense_2/MatMul?
dense_2/BiasAdd/ReadVariableOpReadVariableOp'dense_2_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02 
dense_2/BiasAdd/ReadVariableOp?
dense_2/BiasAddBiasAdddense_2/MatMul:product:0&dense_2/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	?2
dense_2/BiasAddw
Reshape_1/shapeConst*
_output_shapes
:*
dtype0*!
valueB"????
      2
Reshape_1/shape?
	Reshape_1Reshapedense_2/BiasAdd:output:0Reshape_1/shape:output:0*
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

Identity?
NoOpNoOp^dense_2/BiasAdd/ReadVariableOp^dense_2/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 2
NoOp"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*%
_input_shapes
:2
2: : 2@
dense_2/BiasAdd/ReadVariableOpdense_2/BiasAdd/ReadVariableOp2>
dense_2/MatMul/ReadVariableOpdense_2/MatMul/ReadVariableOp:J F
"
_output_shapes
:2
2
 
_user_specified_nameinputs
?.
?
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41980

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
:	2?2
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
?

?
B__inference_dense_2_layer_call_and_return_conditional_losses_42405

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
?
?
,__inference_sequential_2_layer_call_fn_39941

inputs
unknown:	?
	unknown_0:22
	unknown_1:	2?
	unknown_2:	?
	unknown_3:22
	unknown_4:	2?
	unknown_5:22
	unknown_6:	2?
	unknown_7:	?
	unknown_8:22
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
:2
**
_read_only_resource_inputs

	*-
config_proto

CPU

GPU 2J 8? *P
fKRI
G__inference_sequential_2_layer_call_and_return_conditional_losses_389962
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
?
?
$sequential_2_lstm_5_while_cond_36067D
@sequential_2_lstm_5_while_sequential_2_lstm_5_while_loop_counterJ
Fsequential_2_lstm_5_while_sequential_2_lstm_5_while_maximum_iterations)
%sequential_2_lstm_5_while_placeholder+
'sequential_2_lstm_5_while_placeholder_1+
'sequential_2_lstm_5_while_placeholder_2+
'sequential_2_lstm_5_while_placeholder_3D
@sequential_2_lstm_5_while_less_sequential_2_lstm_5_strided_slice[
Wsequential_2_lstm_5_while_sequential_2_lstm_5_while_cond_36067___redundant_placeholder0[
Wsequential_2_lstm_5_while_sequential_2_lstm_5_while_cond_36067___redundant_placeholder1[
Wsequential_2_lstm_5_while_sequential_2_lstm_5_while_cond_36067___redundant_placeholder2[
Wsequential_2_lstm_5_while_sequential_2_lstm_5_while_cond_36067___redundant_placeholder3&
"sequential_2_lstm_5_while_identity
?
sequential_2/lstm_5/while/LessLess%sequential_2_lstm_5_while_placeholder@sequential_2_lstm_5_while_less_sequential_2_lstm_5_strided_slice*
T0*
_output_shapes
: 2 
sequential_2/lstm_5/while/Less?
"sequential_2/lstm_5/while/IdentityIdentity"sequential_2/lstm_5/while/Less:z:0*
T0
*
_output_shapes
: 2$
"sequential_2/lstm_5/while/Identity"Q
"sequential_2_lstm_5_while_identity+sequential_2/lstm_5/while/Identity:output:0*(
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
?.
?
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42050

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
:	2?2
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
?X
?
while_body_41320
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0E
2while_lstm_cell_6_matmul_readvariableop_resource_0:	2?G
4while_lstm_cell_6_matmul_1_readvariableop_resource_0:	2?B
3while_lstm_cell_6_biasadd_readvariableop_resource_0:	?
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_sliceU
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorC
0while_lstm_cell_6_matmul_readvariableop_resource:	2?E
2while_lstm_cell_6_matmul_1_readvariableop_resource:	2?@
1while_lstm_cell_6_biasadd_readvariableop_resource:	???(while/lstm_cell_6/BiasAdd/ReadVariableOp?'while/lstm_cell_6/MatMul/ReadVariableOp?)while/lstm_cell_6/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"2   2   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*
_output_shapes

:22*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
'while/lstm_cell_6/MatMul/ReadVariableOpReadVariableOp2while_lstm_cell_6_matmul_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02)
'while/lstm_cell_6/MatMul/ReadVariableOp?
while/lstm_cell_6/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:0/while/lstm_cell_6/MatMul/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/MatMul?
)while/lstm_cell_6/MatMul_1/ReadVariableOpReadVariableOp4while_lstm_cell_6_matmul_1_readvariableop_resource_0*
_output_shapes
:	2?*
dtype02+
)while/lstm_cell_6/MatMul_1/ReadVariableOp?
while/lstm_cell_6/MatMul_1MatMulwhile_placeholder_21while/lstm_cell_6/MatMul_1/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/MatMul_1?
while/lstm_cell_6/addAddV2"while/lstm_cell_6/MatMul:product:0$while/lstm_cell_6/MatMul_1:product:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/add?
(while/lstm_cell_6/BiasAdd/ReadVariableOpReadVariableOp3while_lstm_cell_6_biasadd_readvariableop_resource_0*
_output_shapes	
:?*
dtype02*
(while/lstm_cell_6/BiasAdd/ReadVariableOp?
while/lstm_cell_6/BiasAddBiasAddwhile/lstm_cell_6/add:z:00while/lstm_cell_6/BiasAdd/ReadVariableOp:value:0*
T0*
_output_shapes
:	2?2
while/lstm_cell_6/BiasAdd?
!while/lstm_cell_6/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2#
!while/lstm_cell_6/split/split_dim?
while/lstm_cell_6/splitSplit*while/lstm_cell_6/split/split_dim:output:0"while/lstm_cell_6/BiasAdd:output:0*
T0*<
_output_shapes*
(:22:22:22:22*
	num_split2
while/lstm_cell_6/splitw
while/lstm_cell_6/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const{
while/lstm_cell_6/Const_1Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_1?
while/lstm_cell_6/MulMul while/lstm_cell_6/split:output:0 while/lstm_cell_6/Const:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul?
while/lstm_cell_6/Add_1AddV2while/lstm_cell_6/Mul:z:0"while/lstm_cell_6/Const_1:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_1?
)while/lstm_cell_6/clip_by_value/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2+
)while/lstm_cell_6/clip_by_value/Minimum/y?
'while/lstm_cell_6/clip_by_value/MinimumMinimumwhile/lstm_cell_6/Add_1:z:02while/lstm_cell_6/clip_by_value/Minimum/y:output:0*
T0*
_output_shapes

:222)
'while/lstm_cell_6/clip_by_value/Minimum?
!while/lstm_cell_6/clip_by_value/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2#
!while/lstm_cell_6/clip_by_value/y?
while/lstm_cell_6/clip_by_valueMaximum+while/lstm_cell_6/clip_by_value/Minimum:z:0*while/lstm_cell_6/clip_by_value/y:output:0*
T0*
_output_shapes

:222!
while/lstm_cell_6/clip_by_value{
while/lstm_cell_6/Const_2Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const_2{
while/lstm_cell_6/Const_3Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_3?
while/lstm_cell_6/Mul_1Mul while/lstm_cell_6/split:output:1"while/lstm_cell_6/Const_2:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul_1?
while/lstm_cell_6/Add_2AddV2while/lstm_cell_6/Mul_1:z:0"while/lstm_cell_6/Const_3:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_2?
+while/lstm_cell_6/clip_by_value_1/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_6/clip_by_value_1/Minimum/y?
)while/lstm_cell_6/clip_by_value_1/MinimumMinimumwhile/lstm_cell_6/Add_2:z:04while/lstm_cell_6/clip_by_value_1/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_6/clip_by_value_1/Minimum?
#while/lstm_cell_6/clip_by_value_1/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_6/clip_by_value_1/y?
!while/lstm_cell_6/clip_by_value_1Maximum-while/lstm_cell_6/clip_by_value_1/Minimum:z:0,while/lstm_cell_6/clip_by_value_1/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_6/clip_by_value_1?
while/lstm_cell_6/mul_2Mul%while/lstm_cell_6/clip_by_value_1:z:0while_placeholder_3*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_2?
while/lstm_cell_6/TanhTanh while/lstm_cell_6/split:output:2*
T0*
_output_shapes

:222
while/lstm_cell_6/Tanh?
while/lstm_cell_6/mul_3Mul#while/lstm_cell_6/clip_by_value:z:0while/lstm_cell_6/Tanh:y:0*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_3?
while/lstm_cell_6/add_3AddV2while/lstm_cell_6/mul_2:z:0while/lstm_cell_6/mul_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_6/add_3{
while/lstm_cell_6/Const_4Const*
_output_shapes
: *
dtype0*
valueB
 *??L>2
while/lstm_cell_6/Const_4{
while/lstm_cell_6/Const_5Const*
_output_shapes
: *
dtype0*
valueB
 *   ?2
while/lstm_cell_6/Const_5?
while/lstm_cell_6/Mul_4Mul while/lstm_cell_6/split:output:3"while/lstm_cell_6/Const_4:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Mul_4?
while/lstm_cell_6/Add_4AddV2while/lstm_cell_6/Mul_4:z:0"while/lstm_cell_6/Const_5:output:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Add_4?
+while/lstm_cell_6/clip_by_value_2/Minimum/yConst*
_output_shapes
: *
dtype0*
valueB
 *  ??2-
+while/lstm_cell_6/clip_by_value_2/Minimum/y?
)while/lstm_cell_6/clip_by_value_2/MinimumMinimumwhile/lstm_cell_6/Add_4:z:04while/lstm_cell_6/clip_by_value_2/Minimum/y:output:0*
T0*
_output_shapes

:222+
)while/lstm_cell_6/clip_by_value_2/Minimum?
#while/lstm_cell_6/clip_by_value_2/yConst*
_output_shapes
: *
dtype0*
valueB
 *    2%
#while/lstm_cell_6/clip_by_value_2/y?
!while/lstm_cell_6/clip_by_value_2Maximum-while/lstm_cell_6/clip_by_value_2/Minimum:z:0,while/lstm_cell_6/clip_by_value_2/y:output:0*
T0*
_output_shapes

:222#
!while/lstm_cell_6/clip_by_value_2?
while/lstm_cell_6/Tanh_1Tanhwhile/lstm_cell_6/add_3:z:0*
T0*
_output_shapes

:222
while/lstm_cell_6/Tanh_1?
while/lstm_cell_6/mul_5Mul%while/lstm_cell_6/clip_by_value_2:z:0while/lstm_cell_6/Tanh_1:y:0*
T0*
_output_shapes

:222
while/lstm_cell_6/mul_5?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_6/mul_5:z:0*
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
while/Identity_4Identitywhile/lstm_cell_6/mul_5:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_6/add_3:z:0^while/NoOp*
T0*
_output_shapes

:222
while/Identity_5?

while/NoOpNoOp)^while/lstm_cell_6/BiasAdd/ReadVariableOp(^while/lstm_cell_6/MatMul/ReadVariableOp*^while/lstm_cell_6/MatMul_1/ReadVariableOp*"
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
1while_lstm_cell_6_biasadd_readvariableop_resource3while_lstm_cell_6_biasadd_readvariableop_resource_0"j
2while_lstm_cell_6_matmul_1_readvariableop_resource4while_lstm_cell_6_matmul_1_readvariableop_resource_0"f
0while_lstm_cell_6_matmul_readvariableop_resource2while_lstm_cell_6_matmul_readvariableop_resource_0",
while_strided_slicewhile_strided_slice_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&: : : : :22:22: : : : : 2T
(while/lstm_cell_6/BiasAdd/ReadVariableOp(while/lstm_cell_6/BiasAdd/ReadVariableOp2R
'while/lstm_cell_6/MatMul/ReadVariableOp'while/lstm_cell_6/MatMul/ReadVariableOp2V
)while/lstm_cell_6/MatMul_1/ReadVariableOp)while/lstm_cell_6/MatMul_1/ReadVariableOp: 
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
: "?L
saver_filename:0StatefulPartitionedCall_1:0StatefulPartitionedCall_28"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*?
serving_default?
@
lstm_5_input0
serving_default_lstm_5_input:02
A
time_distributed_2+
StatefulPartitionedCall:02
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
*z&call_and_return_all_conditional_losses
{_default_save_signature
|__call__"
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
*}&call_and_return_all_conditional_losses
~__call__"
_tf_keras_rnn_layer
?
cell

state_spec
	variables
trainable_variables
regularization_losses
	keras_api
*&call_and_return_all_conditional_losses
?__call__"
_tf_keras_rnn_layer
?
	layer
	variables
trainable_variables
regularization_losses
	keras_api
+?&call_and_return_all_conditional_losses
?__call__"
_tf_keras_layer
?
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
?
(layer_regularization_losses
	variables
)non_trainable_variables
trainable_variables

*layers
+layer_metrics
regularization_losses
,metrics
|__call__
{_default_save_signature
*z&call_and_return_all_conditional_losses
&z"call_and_return_conditional_losses"
_generic_user_object
-
?serving_default"
signature_map
?
-
state_size

 kernel
!recurrent_kernel
"bias
.	variables
/trainable_variables
0regularization_losses
1	keras_api
+?&call_and_return_all_conditional_losses
?__call__"
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
?
2layer_regularization_losses
	variables
3non_trainable_variables
trainable_variables

4layers
5layer_metrics

6states
regularization_losses
7metrics
~__call__
*}&call_and_return_all_conditional_losses
&}"call_and_return_conditional_losses"
_generic_user_object
?
8
state_size

#kernel
$recurrent_kernel
%bias
9	variables
:trainable_variables
;regularization_losses
<	keras_api
+?&call_and_return_all_conditional_losses
?__call__"
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
?
=layer_regularization_losses
	variables
>non_trainable_variables
trainable_variables

?layers
@layer_metrics

Astates
regularization_losses
Bmetrics
?__call__
*&call_and_return_all_conditional_losses
&"call_and_return_conditional_losses"
_generic_user_object
?

&kernel
'bias
C	variables
Dtrainable_variables
Eregularization_losses
F	keras_api
+?&call_and_return_all_conditional_losses
?__call__"
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
?
Glayer_regularization_losses
Hnon_trainable_variables
	variables
trainable_variables

Ilayers
Jlayer_metrics
regularization_losses
Kmetrics
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
_generic_user_object
:	 (2	Adam/iter
: (2Adam/beta_1
: (2Adam/beta_2
: (2
Adam/decay
: (2Adam/learning_rate
,:*	?2lstm_5/lstm_cell_7/kernel
6:4	2?2#lstm_5/lstm_cell_7/recurrent_kernel
&:$?2lstm_5/lstm_cell_7/bias
,:*	2?2lstm_4/lstm_cell_6/kernel
6:4	2?2#lstm_4/lstm_cell_6/recurrent_kernel
&:$?2lstm_4/lstm_cell_6/bias
+:)22time_distributed_2/kernel
%:#2time_distributed_2/bias
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
L0
M1"
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
?
Nlayer_regularization_losses
Onon_trainable_variables
.	variables
/trainable_variables

Players
Qlayer_metrics
0regularization_losses
Rmetrics
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
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
S0
T1"
trackable_list_wrapper
 "
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
?
Ulayer_regularization_losses
Vnon_trainable_variables
9	variables
:trainable_variables

Wlayers
Xlayer_metrics
;regularization_losses
Ymetrics
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
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
Z0
[1"
trackable_list_wrapper
 "
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
?
\layer_regularization_losses
]non_trainable_variables
C	variables
Dtrainable_variables

^layers
_layer_metrics
Eregularization_losses
`metrics
?__call__
+?&call_and_return_all_conditional_losses
'?"call_and_return_conditional_losses"
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
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
!:222lstm_5/Variable
!:222lstm_5/Variable
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
!:222lstm_4/Variable
!:222lstm_4/Variable
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
1:/	?2 Adam/lstm_5/lstm_cell_7/kernel/m
;:9	2?2*Adam/lstm_5/lstm_cell_7/recurrent_kernel/m
+:)?2Adam/lstm_5/lstm_cell_7/bias/m
1:/	2?2 Adam/lstm_4/lstm_cell_6/kernel/m
;:9	2?2*Adam/lstm_4/lstm_cell_6/recurrent_kernel/m
+:)?2Adam/lstm_4/lstm_cell_6/bias/m
0:.22 Adam/time_distributed_2/kernel/m
*:(2Adam/time_distributed_2/bias/m
1:/	?2 Adam/lstm_5/lstm_cell_7/kernel/v
;:9	2?2*Adam/lstm_5/lstm_cell_7/recurrent_kernel/v
+:)?2Adam/lstm_5/lstm_cell_7/bias/v
1:/	2?2 Adam/lstm_4/lstm_cell_6/kernel/v
;:9	2?2*Adam/lstm_4/lstm_cell_6/recurrent_kernel/v
+:)?2Adam/lstm_4/lstm_cell_6/bias/v
0:.22 Adam/time_distributed_2/kernel/v
*:(2Adam/time_distributed_2/bias/v
?2?
G__inference_sequential_2_layer_call_and_return_conditional_losses_39519
G__inference_sequential_2_layer_call_and_return_conditional_losses_39883
G__inference_sequential_2_layer_call_and_return_conditional_losses_39085
G__inference_sequential_2_layer_call_and_return_conditional_losses_39118?
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
 __inference__wrapped_model_36359lstm_5_input"?
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
,__inference_sequential_2_layer_call_fn_38484
,__inference_sequential_2_layer_call_fn_39912
,__inference_sequential_2_layer_call_fn_39941
,__inference_sequential_2_layer_call_fn_39052?
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
A__inference_lstm_5_layer_call_and_return_conditional_losses_40119
A__inference_lstm_5_layer_call_and_return_conditional_losses_40297
A__inference_lstm_5_layer_call_and_return_conditional_losses_40475
A__inference_lstm_5_layer_call_and_return_conditional_losses_40653?
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
&__inference_lstm_5_layer_call_fn_40668
&__inference_lstm_5_layer_call_fn_40683
&__inference_lstm_5_layer_call_fn_40698
&__inference_lstm_5_layer_call_fn_40713?
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
A__inference_lstm_4_layer_call_and_return_conditional_losses_40891
A__inference_lstm_4_layer_call_and_return_conditional_losses_41069
A__inference_lstm_4_layer_call_and_return_conditional_losses_41247
A__inference_lstm_4_layer_call_and_return_conditional_losses_41425?
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
&__inference_lstm_4_layer_call_fn_41440
&__inference_lstm_4_layer_call_fn_41455
&__inference_lstm_4_layer_call_fn_41470
&__inference_lstm_4_layer_call_fn_41485?
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
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_41506
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_41527
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_41541
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_41555?
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
2__inference_time_distributed_2_layer_call_fn_41564
2__inference_time_distributed_2_layer_call_fn_41573
2__inference_time_distributed_2_layer_call_fn_41582
2__inference_time_distributed_2_layer_call_fn_41591?
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
#__inference_signature_wrapper_39155lstm_5_input"?
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
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41648
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41701
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41754
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41811?
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
+__inference_lstm_cell_7_layer_call_fn_41828
+__inference_lstm_cell_7_layer_call_fn_41845
+__inference_lstm_cell_7_layer_call_fn_41919
+__inference_lstm_cell_7_layer_call_fn_41993?
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
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42050
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42103
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42156
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42213?
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
+__inference_lstm_cell_6_layer_call_fn_42230
+__inference_lstm_cell_6_layer_call_fn_42247
+__inference_lstm_cell_6_layer_call_fn_42321
+__inference_lstm_cell_6_layer_call_fn_42395?
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
B__inference_dense_2_layer_call_and_return_conditional_losses_42405?
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
'__inference_dense_2_layer_call_fn_42414?
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
 __inference__wrapped_model_36359? S!"T#Z$%[&'0?-
&?#
!?
lstm_5_input2

? "B??
=
time_distributed_2'?$
time_distributed_22
?
B__inference_dense_2_layer_call_and_return_conditional_losses_42405\&'/?,
%?"
 ?
inputs?????????2
? "%?"
?
0?????????
? z
'__inference_dense_2_layer_call_fn_42414O&'/?,
%?"
 ?
inputs?????????2
? "???????????
A__inference_lstm_4_layer_call_and_return_conditional_losses_40891z#Z$%[F?C
<?9
+?(
&?#
inputs/02?????????2

 
p 

 
? ")?&
?
02?????????2
? ?
A__inference_lstm_4_layer_call_and_return_conditional_losses_41069z#Z$%[F?C
<?9
+?(
&?#
inputs/02?????????2

 
p

 
? ")?&
?
02?????????2
? ?
A__inference_lstm_4_layer_call_and_return_conditional_losses_41247a#Z$%[6?3
,?)
?
inputs2
2

 
p 

 
? " ?
?
02
2
? ?
A__inference_lstm_4_layer_call_and_return_conditional_losses_41425a#Z$%[6?3
,?)
?
inputs2
2

 
p

 
? " ?
?
02
2
? ?
&__inference_lstm_4_layer_call_fn_41440mZ[#$%F?C
<?9
+?(
&?#
inputs/02?????????2

 
p 

 
? "?2?????????2?
&__inference_lstm_4_layer_call_fn_41455mZ[#$%F?C
<?9
+?(
&?#
inputs/02?????????2

 
p

 
? "?2?????????2~
&__inference_lstm_4_layer_call_fn_41470T#Z$%[6?3
,?)
?
inputs2
2

 
p 

 
? "?2
2~
&__inference_lstm_4_layer_call_fn_41485T#Z$%[6?3
,?)
?
inputs2
2

 
p

 
? "?2
2?
A__inference_lstm_5_layer_call_and_return_conditional_losses_40119z S!"TF?C
<?9
+?(
&?#
inputs/02?????????

 
p 

 
? ")?&
?
02?????????2
? ?
A__inference_lstm_5_layer_call_and_return_conditional_losses_40297z S!"TF?C
<?9
+?(
&?#
inputs/02?????????

 
p

 
? ")?&
?
02?????????2
? ?
A__inference_lstm_5_layer_call_and_return_conditional_losses_40475a S!"T6?3
,?)
?
inputs2


 
p 

 
? " ?
?
02
2
? ?
A__inference_lstm_5_layer_call_and_return_conditional_losses_40653a S!"T6?3
,?)
?
inputs2


 
p

 
? " ?
?
02
2
? ?
&__inference_lstm_5_layer_call_fn_40668mST !"F?C
<?9
+?(
&?#
inputs/02?????????

 
p 

 
? "?2?????????2?
&__inference_lstm_5_layer_call_fn_40683mST !"F?C
<?9
+?(
&?#
inputs/02?????????

 
p

 
? "?2?????????2~
&__inference_lstm_5_layer_call_fn_40698T S!"T6?3
,?)
?
inputs2


 
p 

 
? "?2
2~
&__inference_lstm_5_layer_call_fn_40713T S!"T6?3
,?)
?
inputs2


 
p

 
? "?2
2?
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42050?#$%???
???
?
inputs22
s?p
6?3	!?
?22
?

jstates/0VariableSpec
6?3	!?
?22
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
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42103?#$%e?b
[?X
?
inputs22
9?6
?
states/022
?
states/122
p 
? "X?U
N?K
?
0/022
3?0
?
0/1/022
?
0/1/122
? ?
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42156?#$%e?b
[?X
?
inputs22
9?6
?
states/022
?
states/122
p
? "X?U
N?K
?
0/022
3?0
?
0/1/022
?
0/1/122
? ?
F__inference_lstm_cell_6_layer_call_and_return_conditional_losses_42213?#$%???
???
?
inputs22
s?p
6?3	!?
?22
?

jstates/0VariableSpec
6?3	!?
?22
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
+__inference_lstm_cell_6_layer_call_fn_42230?#$%e?b
[?X
?
inputs22
9?6
?
states/022
?
states/122
p 
? "H?E
?
022
/?,
?
1/022
?
1/122?
+__inference_lstm_cell_6_layer_call_fn_42247?#$%e?b
[?X
?
inputs22
9?6
?
states/022
?
states/122
p
? "H?E
?
022
/?,
?
1/022
?
1/122?
+__inference_lstm_cell_6_layer_call_fn_42321?#$%???
???
?
inputs22
s?p
6?3	!?
?22
?

jstates/0VariableSpec
6?3	!?
?22
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
+__inference_lstm_cell_6_layer_call_fn_42395?#$%???
???
?
inputs22
s?p
6?3	!?
?22
?

jstates/0VariableSpec
6?3	!?
?22
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
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41648? !"???
???
?
inputs2
s?p
6?3	!?
?22
?

jstates/0VariableSpec
6?3	!?
?22
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
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41701? !"e?b
[?X
?
inputs2
9?6
?
states/022
?
states/122
p 
? "X?U
N?K
?
0/022
3?0
?
0/1/022
?
0/1/122
? ?
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41754? !"e?b
[?X
?
inputs2
9?6
?
states/022
?
states/122
p
? "X?U
N?K
?
0/022
3?0
?
0/1/022
?
0/1/122
? ?
F__inference_lstm_cell_7_layer_call_and_return_conditional_losses_41811? !"???
???
?
inputs2
s?p
6?3	!?
?22
?

jstates/0VariableSpec
6?3	!?
?22
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
+__inference_lstm_cell_7_layer_call_fn_41828? !"e?b
[?X
?
inputs2
9?6
?
states/022
?
states/122
p 
? "H?E
?
022
/?,
?
1/022
?
1/122?
+__inference_lstm_cell_7_layer_call_fn_41845? !"e?b
[?X
?
inputs2
9?6
?
states/022
?
states/122
p
? "H?E
?
022
/?,
?
1/022
?
1/122?
+__inference_lstm_cell_7_layer_call_fn_41919? !"???
???
?
inputs2
s?p
6?3	!?
?22
?

jstates/0VariableSpec
6?3	!?
?22
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
+__inference_lstm_cell_7_layer_call_fn_41993? !"???
???
?
inputs2
s?p
6?3	!?
?22
?

jstates/0VariableSpec
6?3	!?
?22
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
G__inference_sequential_2_layer_call_and_return_conditional_losses_39085j S!"T#Z$%[&'8?5
.?+
!?
lstm_5_input2

p 

 
? " ?
?
02

? ?
G__inference_sequential_2_layer_call_and_return_conditional_losses_39118j S!"T#Z$%[&'8?5
.?+
!?
lstm_5_input2

p

 
? " ?
?
02

? ?
G__inference_sequential_2_layer_call_and_return_conditional_losses_39519d S!"T#Z$%[&'2?/
(?%
?
inputs2

p 

 
? " ?
?
02

? ?
G__inference_sequential_2_layer_call_and_return_conditional_losses_39883d S!"T#Z$%[&'2?/
(?%
?
inputs2

p

 
? " ?
?
02

? ?
,__inference_sequential_2_layer_call_fn_38484] S!"T#Z$%[&'8?5
.?+
!?
lstm_5_input2

p 

 
? "?2
?
,__inference_sequential_2_layer_call_fn_39052] S!"T#Z$%[&'8?5
.?+
!?
lstm_5_input2

p

 
? "?2
?
,__inference_sequential_2_layer_call_fn_39912W S!"T#Z$%[&'2?/
(?%
?
inputs2

p 

 
? "?2
?
,__inference_sequential_2_layer_call_fn_39941W S!"T#Z$%[&'2?/
(?%
?
inputs2

p

 
? "?2
?
#__inference_signature_wrapper_39155? S!"T#Z$%[&'@?=
? 
6?3
1
lstm_5_input!?
lstm_5_input2
"B??
=
time_distributed_2'?$
time_distributed_22
?
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_41506~&'D?A
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
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_41527~&'D?A
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
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_41541Z&'2?/
(?%
?
inputs2
2
p 

 
? " ?
?
02

? ?
M__inference_time_distributed_2_layer_call_and_return_conditional_losses_41555Z&'2?/
(?%
?
inputs2
2
p

 
? " ?
?
02

? ?
2__inference_time_distributed_2_layer_call_fn_41564q&'D?A
:?7
-?*
inputs??????????????????2
p 

 
? "%?"???????????????????
2__inference_time_distributed_2_layer_call_fn_41573q&'D?A
:?7
-?*
inputs??????????????????2
p

 
? "%?"???????????????????
2__inference_time_distributed_2_layer_call_fn_41582M&'2?/
(?%
?
inputs2
2
p 

 
? "?2
?
2__inference_time_distributed_2_layer_call_fn_41591M&'2?/
(?%
?
inputs2
2
p

 
? "?2
