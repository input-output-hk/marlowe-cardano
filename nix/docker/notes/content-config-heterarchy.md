# Content and Configuration should have a heterarchical relationship

For a given object its content and its configuration should have a
heterarchical relationship, that is, they are siblings.

For entirty of the images this means that the list/set of all images should not
be nested in the configuration of all images. So the `images` and
`defualtPublishOptions` should not be direct siblings, but rather `images` and
`config` should be siblings and `defualtPublishOptions` can be in `config`.

## Motivation

Keping separated **config** from **content** leaves many more options for
delivering UX features.

- We can more easily make an provide various default configs.
- We can also more easily make a system for extending or modifiying a default
config.
- This has a good matinance story, as we can update our default configs and
downstream users can seamlessly use the updated configs with there changes.
- We can provide a function that only takes a list of images and automatically
applies the default config. Making it easy for users to not worry about config
if they dont want to.
- Users have the choice to think about required config options or chosing
of default config set.

## Other thoughts

While it seems clear that the "image set config" and the "image deffinitions"
should be "siblings", the question remains if parts of the "image set config"
or of the "image deffinition" should be de-nested for the same benifits.
