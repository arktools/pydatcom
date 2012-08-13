#!/usr/bin/env python
"""
Finite state machine class.

The fsm class stores dictionary of state/input keys, values are
next state and action

when searching for matching state/input key, exact match
is checked first, then the input is matched against any regular
expressions associated with the state. As a last resort, state/None
can be used as a default for that state.

action is a function to execute which takes the current state
and input as arguments

Exported symbols:

    class FSM
    exception FSMError     - raised when an execution error occurs
    exception RestartError - raised by an action routine to bail back to
                             a restart state
    int FSMEOF             - used as a special input by users to signal
                             termination of the machine

"""

class FSMError(Exception):
    pass

class RestartError(Exception):
    pass

FSMEOF = -1

import re
_RGXT = type(re.compile('foo'))
del re

class FSM:
    """
    Finite State Machine
    simple example:

    >>> def do_faq(state, input):
    ...   print 'send faqfile'
    >>> def do_help(state, input):
    ...   print 'send helpfile'
    >>> def cleanup(state, input):
    ...   print 'clean up'
    >>> import re
    >>> fsm = FSM()
    >>> fsm.add('start', re.compile('help', re.I), 'start', do_help)
    >>> fsm.add('start', 'faq', 'start', do_faq)
    >>> # matches anything, does nothing
    >>> fsm.add('start', None, 'start')
    >>> fsm.add('start', FSMEOF, 'done', cleanup)
    >>> fsm.start('start')
    >>> for line in ['faq', 'help', 'FRED', FSMEOF]:
    ...   try:
    ...     fsm.execute(line)
    ...   except FSMError:
    ...     sys.stderr.write('Invalid input: %r' % line)
    ...
    send faqfile
    send helpfile
    clean up
    """

    def __init__(self):
        self.states = {}
        self.state = None
        self.dbg = None

    # add another state to the fsm
    def add(self, state, input, newstate, action=None):
        """add a new state to the state machine"""
        try:
            self.states[state][input] = (newstate, action)
        except KeyError:
            self.states[state] = {}
            self.states[state][input] = (newstate, action)

    # perform a state transition and action execution
    def execute(self, input):
        """execute the action for the current (state,input) pair"""

        if self.state not in self.states:
            raise FSMError('Invalid state: %s' % self.state)

        state = self.states[self.state]
        # exact state match?
        if input in state:
            newstate, action = state[input]
            if action is not None:
                try:
                    action(self.state, input)
                except RestartError, restartto:
                    # action routine can raise RestartError to force
                    # jump to a different state - usually back to start
                    # if input didn't look like it was supposed to
                    self.state = restartto
                    return
            self.state = newstate
            return

        # no, how about a regex match? (first match wins)
        else:
            for s in state:
                if isinstance(s, _RGXT) and s.match(input) is not None:
                    newstate, action = state[s]
                    if action is not None:
                        try:
                            action(self.state, input)
                        except RestartError, restartto:
                            # action routine can raise RestartError to force
                            # jump to a different state - usually back to start
                            # if input didn't look like it was supposed to
                            self.state = restartto
                            return
                    self.state = newstate
                    return

            if None in state:
                newstate, action = state[None]
                if action is not None:
                    try:
                        action(self.state, input)
                    except RestartError, restartto:
                        # action routine can raise RestartError to force
                        # jump to a different state - usually back to start
                        # if input didn't look like it was supposed to
                        self.state = restartto
                        return
                self.state = newstate
                return

            # oh well, bombed out...
            else:
                raise FSMError('Invalid input to finite state machine: %s' %
                               input)

    # define the start state
    def start(self, state):
        """set the start state"""
        self.state = state

    # assign a writable file to catch debugging transitions
    def debug(self, out=None):
        """assign a debugging file handle"""
        self.dbg = out

if __name__ == "__main__":
    import doctest
    doctest.testmod()
