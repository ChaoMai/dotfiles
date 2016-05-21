#!/bin/python3

import argparse
import pip
from sys import exit
from subprocess import call


def update(packages, is_check):
    for package in packages:
        if is_check:
            print('\nupgrade: {0} ?[y/n]: '.format(package))
            is_upgrade = input()
            if is_upgrade == 'y':
                perform_update(package)
            elif is_upgrade == 'n':
                print('{0} canceled'.format(package))
            else:
                print('input y or n')
                exit(0)
        else:
            perform_update(package)


def perform_update(package):
    upgrade_command = 'sudo pip3 install --upgrade ' + package
    print('\nupgrading {0}'.format(package))
    #  print(upgrade_command.split())
    call(upgrade_command.split())


def main():
    # upgrading modes
    MODE_ALL = 'ALL'
    MODE_SPECIFIC = 'SPECIFIC'
    parser = argparse.ArgumentParser(description='upgrading pip3 packages.')
    parser.add_argument(
        '-mode',
        nargs='?',
        default=MODE_ALL,
        metavar='mode',
        dest='mode',
        help='specify upgrading mode. <ALL | SPECIFIC> (default: %(default)s)')
    parser.add_argument(
        '-packages',
        nargs='*',
        default='None',
        metavar='packages...',
        dest='package_names',
        help=
        'if don\'t upgrade all packages, then specify them. (default: %(default)s)')
    parser.add_argument(
        '-is-check',
        nargs='?',
        default='True',
        metavar='check or not',
        dest='is_check',
        help=
        'specify whether each package. <True | False> (default: %(default)s)')

    args = vars(parser.parse_args())
    mode = MODE_ALL if (args['mode']).upper() == MODE_ALL else MODE_SPECIFIC
    package_names = args['package_names']
    is_check = args['is_check'] == 'True'

    outdated_packages = [dist.project_name
                         for dist in pip.get_installed_distributions()]

    print('\nmode: {0}\nis check: {1}'.format(mode, is_check))

    if mode == MODE_ALL:
        print('\nto upgraed:\n{0}'.format(outdated_packages))
        update(outdated_packages, is_check)
    else:
        intalled_set = set(outdated_packages)
        specified_set = set(package_names)
        to_upgrade_set = intalled_set.intersection(specified_set)
        to_upgrade = list(to_upgrade_set)
        if len(to_upgrade) == 0:
            print('\nno such packages installed:\n{0}'.format(package_names))
        else:
            print('\nto upgraed:\n{0}'.format(to_upgrade))
            update(to_upgrade, is_check)

#  def get_packages(is_outdated=True, proxy=''):
#  commands = pip.commands.list.ListCommand()
#  opts, args = commands.parser.parse_args()
#  opts.outdated = is_outdated
#  commands.run(opts, args)

if __name__ == '__main__':
    main()
