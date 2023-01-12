#!/usr/bin/env bash

gsutil -m -h 'Cache-Control:public,max-age=600' rsync -x ".*\.csv$" out gs://testing.marlowe.run/database/

