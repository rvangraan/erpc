erpc
====

An Erlang remote procedure call library that uses sockets for communication

This library contains two applications:
 * erpc - a generic RPC client
 * erpc_srv - a generic RPC server.

ERPC uses a set of TCP/IP connections to proxy requests to the server and receive responses back.

Features:
 * Calls are completely synchronous from client to server
 * Exceptions and associated stacks are maintained across the interface
 * A basic round-robin load balancing mechanism allows clients to transparently
   failover between multiple backends
 * ERPC includes a call-back and streaming mechanism that allows the server to synchronously
   stream data to the client. This is useful for use cases such as sending files via HTTP etc.
 * Simple security mechanism to allow the client to only call known modules 
 * Ability to declare multiple pools of backends
 
Issues:
 1. No support for SSL yet
 2. Round Robin load balancing isn't plugable
 3. Large blobs in either the request or the response can delay other requests or responses
 4. No support for alarms 

