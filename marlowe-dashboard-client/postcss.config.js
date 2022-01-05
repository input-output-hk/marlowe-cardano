"use strict";

const extraPlugins =
  process.env.NODE_ENV === "production"
    ? [
        require("cssnano")({
          preset: [
            "default",
            {
              discardComments: {
                removeAll: true,
              },
            },
          ],
        }),
      ]
    : [];

module.exports = {
  plugins: [
    require("postcss-import")({
      resolve: (path) =>
        path.replace("@WEB_COMMON_SRC@", process.env.WEB_COMMON_SRC),
    }),
    require("tailwindcss"),
    require("autoprefixer"),
    ...extraPlugins,
  ],
};
