# hs-rl

A toy project for myself to learn Haskell and improve my knowledge in Reinforcement Learning.

Any comments from real Haskeller would be welcome !

## Motivations

In this repository, I try to implement Reinforcement Learning algorithms in a way that should allow a lot of flexibility.
I find that in a lot of RL gyms written in Python, there are too many constraints that in the end you have to write your gym yourself.

Using the flexibility, expressivity, and the ability to write abstractions of high-level Haskell, I want to explore how much it is possible to unify the vast landscape of Reinforcement Learning.

## Examples

You can run the examples in `Learning.Reinforcement.Examples`.

### 1. The Gambler's problem

Find the example in `Learning.Reinforcement.Examples.Gambler`.

It is an example on how RL should be about defining your problem then picking the algorithm, and not the other way around.

In the *Gambler's problem* (from Barto & Sutton 2018, Example 4.3), the state $s$ is the current amount owned by the gambler.
The goal for the gambler is to reach at least 100.
The action $a$ is betting from 0 to $min(s, 100 - s)$.
With 0.5 probability, the gambler loses his bet and goes to $s - a$, or wins and goes to $s + a$.
A game ends whenever the gambler loses all his money or reaches the objective.

The example uses the SARSA algorithm to optimize a state-action function, which is called $Q(s, a)$.
It is a on-policy method with an $\epsilon$-greedy policy.
The reward is 1 only if the gambler reaches the objective ($s$ greater or equal to 100).

## Tools

This project uses Nix to handle the development environment.
I use ghcide, brittany and VS Code to write the Haskell code with a IDE-like experience.

## Thoughts

* It is written as a package, but I haven't yet worked out how to include it into other projects.
* The sarsa algorithm is just a special case of TD learning, I should be able to generalize it more.