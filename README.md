# rmexpbak

Delete backups leaving only a very small fraction of them.

Before I upgrade my system I make a backup of the root filesystem in the form of
a Btrfs snapshot in `/media/hdd/backups`. These backups end up taking a lot of
space, so I regularly delete some of them. However, it can be tricky to decide
which backups to delete.

Suppose in `/media/hdd/backups` I have 5 btrfs snapshots named by date.

```
2021-09-25  2021-11-07  2021-12-01  2022-01-15  2022-02-27
```

This program groups backups into increasing powers of two and deletes all but
one backup in the group. This means an exponential number of backups are deleted
and a logarithmic amount of backups are kept. Since so few backups are kept a
lot of disk space is saved.

```
2021-09-25  2021-11-07 |  2021-12-01  2022-01-15  |   2022-02-27      |
 Group of 4 backups    |  Group of 2 backups      | Group of 1 backup |
```

Keep one backup in each group.

```
                       |              2022-01-15  |   2022-02-27      |
 Group of 4 backups    |  Group of 2 backups      | Group of 1 backup |
```

Note the "Group of 4 backups" deletes all the backups because there is not
enough backups to make a real group of 4.

Of the 5 backups in the example above, only floor(lb(5)) = 2 are kept. The
pruning is more drastic if there are more backups. For example, if there were
256 backups, only 8 would be kept. This allows you to make backups everyday
without wasting a lot of space.

The will program remember the backups it deleted, so running the program twice
on the same directory should be a no-op if no additional backups have been made.

## Usage

For the program to work, it needs to know the command used to delete backups.
Create a [Dhall configuration file](https://dhall-lang.org/) named
`.rmexpbak.dhall` in the directory containing the backups.  e.g.

``` dhall
{rmCmd = ["btrfs", "subvolume", "delete"]}
```

Run `rmexpbak <path to backups>` to see the commands the program will run. Once
you are satisfied, run `rmexpbak --delete <path to backups>` to actually delete
the backups. e.g.

``` sh
$ tee /media/hdd/backups/.rmexpbak.dhall << EOF
{rmCmd = ["btrfs", "subvolume", "delete"]}
EOF
$ rmexpbak /media/hdd/backups/
Dry-run. Specify --delete to actually delete files
["btrfs","subvolume","delete","/media/hdd/backups/2021-09-25"]
["btrfs","subvolume","delete","/media/hdd/backups/2021-11-07"]
["btrfs","subvolume","delete","/media/hdd/backups/2021-12-01"]
Dry-run. Specify --delete to actually delete files
$ rmexpbak --delete /media/hdd/backups
["btrfs","subvolume","delete","/media/hdd/backups/2021-09-25"]
Delete subvolume (no-commit): '/media/hdd/backups/2021-09-25'
["btrfs","subvolume","delete","/media/hdd/backups/2021-11-07"]
Delete subvolume (no-commit): '/media/hdd/backups/2021-11-07'
["btrfs","subvolume","delete","/media/hdd/backups/2021-12-01"]
Delete subvolume (no-commit): '/media/hdd/backups/2021-12-01'
```

## Pre-commit checklist

1. Run tests (`stack test`)
2. Run coverage on unit tests (`stack test --coverage`)
3. Run hlint (`hlint .`)

