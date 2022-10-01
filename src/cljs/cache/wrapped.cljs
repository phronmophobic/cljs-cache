;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.cache.wrapped
  "A higher level way to use clojure.core.cache that assumes the immutable
  cache is wrapped in an atom.

  The API is (almost) the same as clojure.core.cache -- including the factory
  functions -- but instead of accepting immutable caches, the functions
  here accept atoms containing those caches. The factory functions return
  new atoms containing the newly created cache.

  In addition, lookup-or-miss provides a safe, atomic way to retrieve a
  value from a cache or compute it if it is missing, without risking a
  cache stampede."
  (:require [cljs.cache :as c]))



(defn lookup
  "Retrieve the value associated with `e` if it exists, else `nil` in
  the 2-arg case.  Retrieve the value associated with `e` if it exists,
  else `not-found` in the 3-arg case.

  Reads from the current version of the atom."
  ([cache-atom e]
   (c/lookup @cache-atom e))
  ([cache-atom e not-found]
   (c/lookup @cache-atom e not-found)))

(def ^{:private true} default-wrapper-fn #(%1 %2))

(defn lookup-or-miss
  "Retrieve the value associated with `e` if it exists, else compute the
  value (using value-fn, and optionally wrap-fn), update the cache for `e`
  and then perform the lookup again.

  value-fn (and wrap-fn) will only be called (at most) once even in the
  case of retries, so there is no risk of cache stampede.

  Since lookup can cause invalidation in some caches (such as TTL), we
  trap that case and retry (a maximum of ten times)."
  ([cache-atom e value-fn]
   (lookup-or-miss cache-atom e default-wrapper-fn value-fn))
  ([cache-atom e wrap-fn value-fn]
   (let [d-new-value (delay (wrap-fn value-fn e))]
     (loop [n 0
            v (c/lookup (swap! cache-atom
                               c/through-cache
                               e
                               default-wrapper-fn
                               (fn [_] @d-new-value))
                        e
                        ::expired)]
       (when (< n 10)
         (if (= ::expired v)
           (recur (inc n)
                  (c/lookup (swap! cache-atom
                                   c/through-cache
                                   e
                                   default-wrapper-fn
                                   (fn [_] @d-new-value))
                            e
                            ::expired))
           v))))))

(defn has?
  "Checks if the cache contains a value associated with `e`.

  Reads from the current version of the atom."
  [cache-atom e]
  (c/has? @cache-atom e))

(defn hit
  "Is meant to be called if the cache is determined to contain a value
  associated with `e`.

  Returns the updated cache from the atom. Provided for completeness."
  [cache-atom e]
  (swap! cache-atom c/hit e))

(defn miss
  "Is meant to be called if the cache is determined to **not** contain a
  value associated with `e`.

  Returns the updated cache from the atom. Provided for completeness."
  [cache-atom e ret]
  (swap! cache-atom c/miss e ret))

(defn evict
  "Removes an entry from the cache.

  Returns the updated cache from the atom."
  [cache-atom e]
  (swap! cache-atom c/evict e))

(defn seed
  "Is used to signal that the cache should be created with a seed.
  The contract is that said cache should return an instance of its
  own type.

  Returns the updated cache from the atom. Provided for completeness."
  [cache-atom base]
  (swap! cache-atom c/seed base))

(defn through
  "The basic hit/miss logic for the cache system.  Expects a wrap function and
  value function.  The wrap function takes the value function and the item in question
  and is expected to run the value function with the item whenever a cache
  miss occurs.  The intent is to hide any cache-specific cells from leaking
  into the cache logic itelf."
  ([cache-atom item] (through default-wrapper-fn identity cache-atom item))
  ([value-fn cache-atom item] (through default-wrapper-fn value-fn cache-atom item))
  ([wrap-fn value-fn cache-atom item]
   (swap! cache-atom c/through-cache item wrap-fn value-fn)))

(defn through-cache
  "The basic hit/miss logic for the cache system.  Like through but always has
  the cache argument in the first position."
  ([cache-atom item] (through-cache cache-atom item default-wrapper-fn identity))
  ([cache-atom item value-fn] (through-cache cache-atom item default-wrapper-fn value-fn))
  ([cache-atom item wrap-fn value-fn]
   (swap! cache-atom c/through-cache item wrap-fn value-fn)))

(defn basic-cache-factory
  "Returns a pluggable basic cache initialized to `base`"
  [base]
  (atom (c/basic-cache-factory base)))

(defn lru-cache-factory
  "Returns an LRU cache with the cache and usage-table initialized to `base` --
   each entry is initialized with the same usage value.

   This function takes an optional `:threshold` argument that defines the maximum number
   of elements in the cache before the LRU semantics apply (default is 32)."
  [base & {threshold :threshold :or {threshold 32}}]
  (atom (c/lru-cache-factory base :threshold threshold)))

(defn ttl-cache-factory
  "Returns a TTL cache with the cache and expiration-table initialized to `base` --
   each with the same time-to-live.

   This function also allows an optional `:ttl` argument that defines the default
   time in milliseconds that entries are allowed to reside in the cache."
  [base & {ttl :ttl :or {ttl 2000}}]
  (atom (c/ttl-cache-factory base :ttl ttl)))

