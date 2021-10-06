
# Table of Contents

1.  [A logging implementation for Janet - jlogger](#org1eb2f12)
2.  [Purpose](#orgc2dc0a0)
3.  [Installation](#orga47e831)
4.  [How it works](#org403cee6)
    1.  [Heirarchy:](#org16583f0)
    2.  [log-manager](#org8dc69cf)
    3.  [log-sink-combo (special)](#org40f11fe)
    4.  [log-sink](#org5ebf1df)
    5.  [log-formatter](#org53d8f6d)
    6.  [log-namer](#orgae82369)
5.  [Usage](#org4fa27fc)
6.  [This should be flexible!](#orge002613)
7.  [What&rsquo;s left to do](#org61f55ae)
    1.  [Proxy objects for all log types (hide their internals)](#org7017098)
    2.  [Documentation](#org4cc439e)


<a id="org1eb2f12"></a>

# A logging implementation for Janet - jlogger


<a id="orgc2dc0a0"></a>

# Purpose

Provide a decent abstraction on logging along with a nice interface for one to
be able to design their own logging backends.


<a id="orga47e831"></a>

# Installation

**To Be Done**


<a id="org403cee6"></a>

# How it works


<a id="org16583f0"></a>

## Heirarchy:

log-manager -> (log-sink-combo) -> log-sink&#x2026;
log-formatter
log-namer


<a id="org8dc69cf"></a>

## log-manager

This manages all the logs for the application, it&rsquo;s purpose is to open loggers
and return a logging object (logger).


<a id="org40f11fe"></a>

## log-sink-combo (special)

A logger is usually just a log-sink except when a logger actually combines
multiple log-sink&rsquo;s into one logger object.


<a id="org5ebf1df"></a>

## log-sink

Any object conforming to the following interface (TBD).


<a id="org53d8f6d"></a>

## log-formatter

Something that can format a log, supporting all that string/format does and
outputting things in it&rsquo;s unique manner.


<a id="orgae82369"></a>

## log-namer

Something to format the name of a log.


<a id="org4fa27fc"></a>

# Usage

    (import logging)
    
    # get a log manager
    (def manager (:new logging/manager))
    # optional parameters, with defaults -
    #  :rotation        {:bytes :never :lines :never
    #                    :interval :never :boundary :daily}
    #  :flush-frequency {:bytes 0 :lines 0 :interval 0}
    #  :formatter       formatters/default
    #  :namer           namers/default
    #  :levels          [:trace :debug :info :warn :error :fatal]
    
    # open a log sink (it's a combo sink, so will need to open sub-sinks)
    (def combo (:open m :type :combo :name "arbitray"))
    
    # open a file sink, could do this direct on a log manager, same way
    # just one would instead assign the result to a variable to be able
    # to use it.
    (:open combo :type :file :name "arbitrary-file")
    # optional:
    #  :initializer {:split false :directory "." :levels nil}
    # NOTE: levels should match one of the levels defined when opening
    # a log manager, such as :trace from default, or it'll default to all
    # levels. When defined, it'll log only at that level and levels above it.
    # When split, each level will be logged to its own file.
    
    # open a console sink
    (:open combo :type :console :name "arbitrary-console")
    # optional
    #  :initializer {:levels nil :errors nil}
    # NOTE: levels should be same as described for files, yet must be a
    # level below what :errors is set to. With :errors, nil is same as
    # the last level + 1. levels defaults to first level. When levels is
    # defined, any level at the level specified, and the ones above it'll
    # will be logged to stdout, up to the level defined by :errors. Levels
    # at the same as :errors, and above, will be logged to stderr.
    
    (:error combo "Error")
    (:error combo "Formatted %q" '[a b c d])
    (:trace combo "Trace")
    (:write combo :trace "Trace")
    (:write combo :trace "Formatted %q" '[a b c d])
    
    # can optionally close logs individually, but should only do it from
    # the manager like (:close manager "arbitrary-name-from-before")
    
    # close all logs
    (:close manager)


<a id="orge002613"></a>

# This should be flexible!

The way it works should support the addition of any logger type from
networked, to threaded, or sql, or whatever the imagination gives.

For instance, for JSON formatting either you can open the logging/manager with
a formatter that emits JSON or you could, when opening a log with the log
manager, instruct the individual log (per it&rsquo;s :new method) to output using a
certain formatter.

Each log-sink should be responsible for it&rsquo;s own rotation (you can&rsquo;t rotate a
console for instance like you could files).

The above example seems a bit verbose, maybe there should be an easy default
way?

Things may change as this project evolves, this gives an idea as to what is
envisioned however.


<a id="org61f55ae"></a>

# What&rsquo;s left to do


<a id="org7017098"></a>

## TODO Proxy objects for all log types (hide their internals)


<a id="org4cc439e"></a>

## TODO Documentation

The idea is to take the root log-manager proxy, and append each logging type&rsquo;s
documentation to it where sensible.

