#lang racket
(list "SA_UPCALL_NEWPROC This upcall notifies the process of a new processor allocation. The first upcall to a program, triggered by
sa_enable(), will be of this type.
SA_UPCALL_PREEMPTED This upcall notifies the process of a reduction in its processor allocation. There may be multiple ``event''
activations if the allocation was reduced by several processors.
SA_UPCALL_BLOCKED This upcall notifies the process that an activation has blocked in the kernel. The sa_context field of the event
should not be continued until a SA_UPCALL_UNBLOCKED event has been delivered for the same activation.
SA_UPCALL_UNBLOCKED This upcall notifies the process that an activation which previously blocked (and for which a SA_UPCALL_BLOCKED
upcall was delivered) is now ready to be continued.
SA_UPCALL_SIGNAL This upcall is used to deliver a POSIX-style signal to the process. If the signal is a synchronous trap, then event is 1,
and sas[1] points to the activation which triggered the trap. For asynchronous signals, event is 0. The arg parameter points to a
siginfo_t structure that describes the signal being delivered.
SA_UPCALL_USER This upcall is delivered when requested by the process itself with sa_preempt(). The sas[1] activation will be the
activation specified in the call.")
