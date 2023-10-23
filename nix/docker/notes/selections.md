# Selections

Provided images can be selected into named **selections**.

**Selections** are considered **content**.

The specification of **selections** is entirely optional.

When am image is accssed threw the **selection**, global and image config are
combined and then **selection** overrides are applied before builind the image.

## Motivation

1. Provide a way to for users to create named sets (**selections**) that they
can preform aggrigate operations on (i.e publish all the "runtime").
2. Apply a common config to **selections** of images to support aggrigate
operations (i.e when all the "runtime" images are published, use "ghcr" as the
destination).

## Other benifits

Can easily include an image in multiple **selections**.
- Images that other images depend on can be published with different
**selections** that have different concerns.
- An image could be given other settings in another **selection**, to provide
a varient of the image that is addpated to fit different needs.

Should be relativly easy to change what **selections** an image is included in
and it should require little changes.
