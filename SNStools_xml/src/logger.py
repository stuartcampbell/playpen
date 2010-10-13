#!/usr/bin/env python
import datetime
import os
from version import version as __version__

def get_year():
    now = datetime.datetime.now()
    return str(now.year)

def get_ucams():
    return os.environ['LOGNAME']

def logger(application='N/A'):
    cmd = '/usr/bin/logger -p local5.notice IDLtools'
    cmd += ' ' + application
    cmd += '_' + __version__
    cmd += ' ' + get_ucams()
    cmd += ' ' + get_year()
    os.system(cmd)

if __name__ == "__main__":
    logger()
    