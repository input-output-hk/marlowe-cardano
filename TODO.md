# TODO

1. Selections config should override, probaly the override should have visiblty
on image config its overriding

# Publishing

images grouped for publishing
images can be published to **different** destinations
default destinations for all/group/and image

# Examples
```json
{
  globalConfig = { size = "1G"; };
  imageSets = {
    karenSet = {
      setConfig = { size = "8G"; };
      images = [ "faa" "fee" ];
    };
    runtime-images = {
      setConfig = { ... };
      images = [ "faa" "fee" ];
    };
    client-apps = {
      setConfig = { ... };
      images = [ "foo" "fee" ];
    };
  };
  images = [
    { name = "foo";
    }
    { name = "faa"
      size = "5G";
    }
    { name = "fee" }
  ];
}

images.karenSet.fee.size
```
