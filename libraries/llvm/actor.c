#include <pthread.h>
#include <stdio.h>
#include <stdatomic.h>
#include <stdbool.h>
#include "types.c"


enum MessageType {
    // "Special messages"
    RESUME = -2,
    TERMINATE = -1,
    // Effects
    EMIT,
};

# define QUEUE_SIZE 16
struct MessageQueue {
    long head;
    long tail;
    pthread_mutex_t mutex;
    struct Pos* data;
    // Maybe conditions
};

struct Actor {
    struct MessageQueue* queue;
    pthread_cond_t signal;
    pthread_mutex_t signal_mutex;
};
typedef struct Actor actor_t;

struct MessageData {
    long tag;
    long message; // Might need to be Pos
    struct Pos reply_to;
    Stack continuation;
};

void c_queue_free(struct MessageQueue* queue) {
    // printf("Freeing queue: %p\n", queue);
    long head = queue->head;
    long tail = queue->tail;
    for (int i = head; i < tail; i++) {
        struct Pos data = queue->data[i];
        erasePositive(data);
    }
    free(queue->data);
}

void c_actor_free(void* actorPtr) {
    // printf("Freeing actor: %p\n", actorPtr);
    struct Actor* actor = actorPtr;
    c_queue_free(actor->queue);
    free(actor->queue);
    pthread_cond_destroy(&actor->signal);
    pthread_mutex_destroy(&actor->signal_mutex);
}

struct MessageQueue* c_queue_allocate() {
    struct MessageQueue* alloc = malloc(sizeof(struct MessageQueue));
    struct Pos* data = calloc(QUEUE_SIZE, sizeof(struct Pos));
    struct MessageQueue queue = (struct MessageQueue) { .head = 0, .tail = 0, .mutex = PTHREAD_MUTEX_INITIALIZER, .data = data };
    *alloc = queue;
    return alloc;
}

struct MessageData c_message_allocate() {
    return (struct MessageData) {};
}

struct Actor c_actor_allocate() {
    return (struct Actor) {
        .queue = c_queue_allocate(),
        .signal = PTHREAD_COND_INITIALIZER,
        .signal_mutex = PTHREAD_MUTEX_INITIALIZER,
    };
}

void c_erase_log(void* obj) {
    printf("Erasing object\n");
    return;
}

struct Pos c_actor_new() {
    void* wrap_alloc = malloc(sizeof(struct Header) + sizeof(struct Actor));

    struct Header* headerPtr = wrap_alloc;
    *headerPtr = (struct Header) { .rc = 0, .eraser = c_actor_free };

    struct Actor* objPtr = wrap_alloc + sizeof(struct Header); 
    *objPtr = c_actor_allocate();
    struct Pos wrapped = (struct Pos) {
      .tag = 0,
      .obj = wrap_alloc,
    };
    return wrapped;
}

void c_erase_message(void* message) {
    // printf("Erasing message: %p\n", message);
    return;
}

struct Pos c_message_wrap(struct MessageData message) {
    void* wrap_alloc = malloc(sizeof(struct Header) + sizeof(struct MessageData));

    struct Header* headerPtr = wrap_alloc;
    *headerPtr = (struct Header) { .rc = 0, .eraser = c_erase_message };

    struct MessageData* messagePtr = wrap_alloc + sizeof(struct Header); 
    *messagePtr = message;
    struct Pos wrapped_message = (struct Pos) {
      .tag = 0,
      .obj = wrap_alloc,
    };
    return wrapped_message;
}

struct MessageData c_message_effect(struct Pos reply_to, long effect, long message, Stack stack) {
    return (struct MessageData) {
        .tag = effect,
        .message = message,
        .reply_to = reply_to,
        .continuation = stack,
    };
}

struct MessageData c_message_resume(Stack stack) {
    return (struct MessageData) {
        .tag = RESUME,
        .continuation = stack,
    };
}

struct MessageData c_message_terminate() {
    return (struct MessageData) {
        .tag = TERMINATE,
    };
}

bool c_queue_isEmpty(struct MessageQueue* queue) {
    pthread_mutex_lock(&queue->mutex);
    bool res = queue->head == queue->tail;
    pthread_mutex_unlock(&queue->mutex);
    return res;
}

bool c_queue_isFull(struct MessageQueue* queue) {
    pthread_mutex_lock(&queue->mutex);
    bool res = ((queue->head + 1) % QUEUE_SIZE) == queue->tail;
    pthread_mutex_unlock(&queue->mutex);
    return res;
}

void c_queue_unsafeEnqueue(struct MessageQueue* queue, struct Pos value) {
    pthread_mutex_lock(&queue->mutex);

    queue->data[queue->tail] = value;
    queue->tail = (queue->tail + 1) % QUEUE_SIZE;

    pthread_mutex_unlock(&queue->mutex);
}

void c_queue_unsafeDequeue(struct MessageQueue* queue) {
    pthread_mutex_lock(&queue->mutex);
    queue->head = (queue->head + 1) % QUEUE_SIZE; 
    pthread_mutex_unlock(&queue->mutex);
}

struct Pos c_queue_unsafePeek(struct MessageQueue* queue) {
    pthread_mutex_lock(&queue->mutex);
    struct Pos value = queue->data[queue->head];
    pthread_mutex_unlock(&queue->mutex);

    return value;
}

struct Pos c_queue_unsafePop(struct MessageQueue* queue) {
    pthread_mutex_lock(&queue->mutex);
    struct Pos value = queue->data[queue->head];
    queue->head = (queue->head + 1) % QUEUE_SIZE;
    pthread_mutex_unlock(&queue->mutex);

    return value;
}

struct Actor* c_actor(struct Pos pos) {
    struct Actor* actor = pos.obj + sizeof(struct Header);
    return actor;
}

void c_queue_send_emit(struct Pos reply_to, long value, struct Pos handler_pos, Stack cont) {
    struct Actor* handler = c_actor(handler_pos);
    struct MessageData message = c_message_effect(reply_to, EMIT, value, cont);
    c_queue_unsafeEnqueue(handler->queue, c_message_wrap(message));
}

void c_queue_send_resume(struct Pos emitter_pos, Stack cont) {
    struct Actor* emitter = c_actor(emitter_pos);
    struct MessageData message = c_message_resume(cont);
    c_queue_unsafeEnqueue(emitter->queue, c_message_wrap(message));
}

void c_queue_send_terminate(struct Pos actor_pos) {
    struct Actor* actor = c_actor(actor_pos);
    struct MessageData message = c_message_terminate();
    c_queue_unsafeEnqueue(actor->queue, c_message_wrap(message));
}

void c_actor_inspect_queue(struct Pos actor_pos) {
    struct Actor* actor = c_actor(actor_pos);

    pthread_mutex_lock(&actor->queue->mutex);

    long length = actor->queue->tail - actor->queue->head;
    printf("Queue: %ld\n", length);
    for (int i=actor->queue->head; i < actor->queue->tail; i++) {
        struct Pos element = actor->queue->data[i];
        struct MessageData* message = element.obj + sizeof(struct Header);
        printf("(%ld %ld) ", message->message, message->tag);
    }
    printf("\n");

    pthread_mutex_unlock(&actor->queue->mutex);
}

void c_inspect_rc(struct Pos pos) {
    struct Header* header = pos.obj;
    printf("RC: %ld\n", header->rc);
}

void c_actor_start(struct Pos actor_pos, Stack stack) {
    struct Actor* actor = actor_pos.obj + sizeof(struct Header);
    printf("Starting actor\n");
    resume_Pos(stack, Unit);
    while (true) {
        if (!c_queue_isEmpty(actor->queue)) {
            struct Pos message_pos = c_queue_unsafePop(actor->queue);
            struct MessageData* messagePtr = message_pos.obj + sizeof(struct Header);
            struct MessageData message = *messagePtr;
            erasePositive(message_pos);
            switch(message.tag) {
                case RESUME:
                    printf("Resuming\n");
                    resume_Int(message.continuation, message.message);
                    continue;
                case TERMINATE:
                    printf("Got terminate\n");
                    erasePositive(actor_pos);
                    return;
                case EMIT:
                    // Handler things
                    printf("Emit: %ld\n", message.message);
                    // Resume
                    c_queue_send_resume(message.reply_to, message.continuation);
                    continue;
            }
        }
    }
}
