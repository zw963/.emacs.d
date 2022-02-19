mode: snippet -*-
# name: ListView.builder
# key: ListView
# contributor: Billy.Zheng vil963@gmail.com
# --
ListView.builder(
      itemBuilder: (context, index) => _buildItem(_${1:items}[index]),
      itemCount: $1.length,
    )