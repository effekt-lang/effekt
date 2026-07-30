#include <pthread.h>
#include <stdio.h>
#include <stdatomic.h>
#include "types.c"

// struct ThreadObject {
//     long id;
//     void* function_pointer;
//     void* object;
//     void* stack;
// };

// MessageType will be stored in Pos tag
enum MessageType {
    RESUME,
    TERMINATE,
    EMIT,
};

struct MessageData {
    long message; // Might need to be Pos
    Stack continuation;
};

# define QUEUE_SIZE 16
struct MessageQueue {
    long head;
    long tail;
    struct Pos* data;
    // Maybe conditions
};

// void c_thread_erase_noop(void *envPtr) { (void)envPtr; }

// struct Pos c_thread_start(void* function_pointer, void* function_arguments) {

//     void *objPtr = malloc(sizeof(struct Header) + sizeof(struct ThreadObject));
//     struct Header *headerPtr = objPtr;
//     *headerPtr = (struct Header) { .rc = 0, .eraser = c_thread_erase_noop, };

//     long threadId;
//     int ret = pthread_create(&threadId, NULL, function_pointer, function_arguments);
//     if (!ret) {
//         printf("Failed to create pthread\n");
//     }

//     struct ThreadObject *threadObject = objPtr + sizeof(struct Header);
//     *threadObject = (struct ThreadObject) { 
//         .id = threadId, 
//         .function_pointer = function_pointer, 
//         .object = NULL,
//         .stack = NULL,
//     };

//     return (struct Pos) {
//         .tag = threadId,
//         .obj = objPtr,
//     };
// }

// struct ThreadObject* c_thread_object(const struct Pos pos) {
//     struct ThreadObject *data = pos.obj + sizeof(struct Header);
//     return data;
// }

// void c_thread_join(const struct Pos pos) {
//     int ret = pthread_join(pos.tag, NULL);
//     if (!ret) {
//         printf("Failed to join pthread\n");
//     }

//     erasePositive(pos);
// }

void c_queue_erase(void *queuePtr) { 
    pthread_mutex_t *mutex = queuePtr;
    pthread_mutex_destroy(mutex);

    long *head = queuePtr + sizeof(pthread_mutex_t);
    long *tail = head + sizeof(long);
    struct Pos *dataPtr = (struct Pos*) tail + sizeof(long);
    for (int i = *head; i < *tail; i++) {
        struct Pos *data_ptr = dataPtr + i * sizeof(struct Pos);
        struct Pos data = *data_ptr;
        erasePositive(data);
    } 
}

void c_queue_message_erase(void *queuePtr) {
    return;
}

struct Pos c_queue_new() {
    void *objPtr = malloc(sizeof(struct Header) + sizeof(pthread_mutex_t) + sizeof(struct MessageQueue) + QUEUE_SIZE * sizeof(struct Pos));
    struct Header *headerPtr = objPtr;
    *headerPtr = (struct Header) { .rc = 0, .eraser = c_queue_erase, };

    pthread_mutex_t *mutexPtr = objPtr + sizeof(struct Header);
    pthread_mutex_init(mutexPtr, NULL);

    struct MessageQueue *queuePtr = (struct MessageQueue*) mutexPtr + sizeof(pthread_mutex_t);
    *queuePtr = (struct MessageQueue) {
        .head = 0,
        .tail = 0,
        .data = (struct Pos*) queuePtr + sizeof(struct MessageQueue),
    };

    return (struct Pos) {
        .tag = 0, // We don't use the tag, as the queue is fixed size
        .obj = objPtr,
    };
}

struct Pos c_message_new_raw() {
    void *objPtr = malloc(sizeof(struct Header) + sizeof(struct MessageData));
    struct Header *headerPtr = objPtr;
    *headerPtr = (struct Header) { .rc = 0, .eraser = c_queue_message_erase, };

    return (struct Pos) {
        .tag = 0,
        .obj = objPtr,
    };
}

// Needs to take resume value as well, when necessary
struct Pos c_message_new_resume(Stack stack) {
    struct Pos raw_message = c_message_new_raw();
    struct MessageData *dataPtr = (struct MessageData*) raw_message.obj + sizeof(struct Header);
    raw_message.tag = RESUME;
    dataPtr->continuation = stack;

    return raw_message;
}

struct Pos c_message_new_emit(long message, Stack stack) {
    struct Pos raw_message = c_message_new_raw();
    struct MessageData *dataPtr = (struct MessageData*) raw_message.obj + sizeof(struct Header);
    raw_message.tag = EMIT;
    dataPtr->message = message;
    dataPtr->continuation = stack;

    sharePositive(raw_message);
    return raw_message;
}

struct Pos c_message_new_terminate() {
    struct Pos raw_message = c_message_new_raw();
    struct MessageData *dataPtr = (struct MessageData*) raw_message.obj + sizeof(struct Header);
    raw_message.tag = TERMINATE;

    return raw_message;
}

// Might need to be Pos
long c_message_value(struct Pos message) {
    return c_message(message)->message;
}

Stack c_message_continuation(struct Pos message) {
    return c_message(message)->continuation;
}

struct MessageData* c_message(struct Pos message) {
    struct MessageData *data = (struct MessageData*) message.obj + sizeof(struct Header);
    return data;
}

pthread_mutex_t *c_queue_mutex(struct Pos queue) {
    pthread_mutex_t *mutex_ptr = queue.obj + sizeof(struct Header);
    return mutex_ptr;
}

struct MessageQueue* c_queue(struct Pos queue) {
    struct MessageQueue *data = (struct MessageQueue*) c_queue_mutex(queue) + sizeof(pthread_mutex_t);
    return data;
}

struct Pos c_queue_empty(struct Pos queue) {
    pthread_mutex_t *mutex = c_queue_mutex(queue);
    pthread_mutex_lock(mutex);
    struct MessageQueue q = *c_queue(queue);
    pthread_mutex_unlock(mutex);
    
    if (q.head == q.tail) {
        return BooleanTrue;
    } else {
        return BooleanFalse;
    }
}

struct Pos c_queue_full(struct Pos queue) {
    pthread_mutex_t *mutex = c_queue_mutex(queue);
    pthread_mutex_lock(mutex);
    struct MessageQueue q = *c_queue(queue);
    pthread_mutex_unlock(mutex);

    if ((q.head + 1) % QUEUE_SIZE == q.tail) {
        return BooleanTrue;
    } else {
        return BooleanFalse;
    }
}

struct Pos c_queue_unsafeEnqueue(struct Pos queue, struct Pos value) {
    pthread_mutex_t *mutex = c_queue_mutex(queue);
    pthread_mutex_lock(mutex);
    struct MessageQueue *q = c_queue(queue);

    q->data[q->tail] = value;
    q->tail = (q->tail + 1) % QUEUE_SIZE;

    pthread_mutex_unlock(mutex);

    return Unit;
}

struct Pos c_queue_unsafeDequeue(struct Pos queue) {
    pthread_mutex_t *mutex = c_queue_mutex(queue);
    pthread_mutex_lock(mutex);
    struct MessageQueue *q = c_queue(queue);
    q->head = (q->head + 1) % QUEUE_SIZE;
    pthread_mutex_unlock(mutex);

    return Unit;
}

struct Pos c_queue_unsafePeek(struct Pos queue) {
    pthread_mutex_t *mutex = c_queue_mutex(queue);
    pthread_mutex_lock(mutex);
    struct MessageQueue *q = c_queue(queue);
    struct Pos value = q->data[q->head];
    pthread_mutex_unlock(mutex);
    return value;
}

struct Pos c_queue_unsafePop(struct Pos queue) {
    pthread_mutex_t *mutex = c_queue_mutex(queue);
    pthread_mutex_lock(mutex);
    struct MessageQueue *q = c_queue(queue);
    struct Pos value = q->data[q->head];
    q->head = (q->head + 1) % QUEUE_SIZE;
    pthread_mutex_unlock(mutex);
    return value;
}

// Handler & Emitter need to be MessageQueues
struct Pos c_queue_send_emit(long value, struct Pos handler, Stack cont) {
    // Should handle handler queue full here
    c_queue_unsafeEnqueue(handler, c_message_new_emit(value, cont));

    erasePositive(handler);

    resume_Pos(cont, Unit);

    return Unit;
}

struct Pos c_queue_send_resume(struct Pos emitter, Stack cont) {
    c_queue_unsafeEnqueue(emitter, c_message_new_resume(cont));
    erasePositive(emitter);

    return Unit;
}

struct Pos c_queue_resume(struct Pos emitter) {
    struct Pos popped = c_queue_unsafePop(emitter);
    // assert(popped.tag == RESUME);
    struct MessageData* message = c_message(popped);
    // Use resume value here instead of Unit
    resume_Pos(message->continuation, Unit);

    erasePositive(emitter);
    return Unit;
}