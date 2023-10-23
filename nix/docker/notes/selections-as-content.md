# Selections as Content

**Selections** are them selfs **content** for mkImages and not
**configuration**. As a result **selections** have a heterarchical relationship
with the global **configuration**.

This means that there are no "defualt" **selections**. Some portion of there
details must be specified if they are desired. (Makese sens, other than thet set
of all Images)

## Motivation

This keps the "all images" **configuration** object more easy to provide a
defaults and more easy to extended.

Allows the "all images" **configuration** object to use as the configuration
object for **selections** without allowing "**selections** of **selections**"
thru recursion.
