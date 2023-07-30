# Usage

ipl2py works on individual files. To compile `script.ipl` to Python simply

```shell
ipl2py script.ipl  # creates script.ipl.py
```

You can specify an output file as well.

```shell
ipl2py script.ipl -o script.py
```

By default comments are preserved. They can be discarded by passing the
`--no-comments` flag.

```shell
ipl2py script.ipl -o script.py --no-comments
```
