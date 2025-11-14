#!/bin/bash

# 获取所有子模块路径
submodule_paths=$(git config --file .gitmodules --get-regexp path | awk '{ print $2 }')

# 定义切换分支的函数
switch_branch() {
  submodule_path="$1"
  echo "Processing submodule: $submodule_path"

  cd "$submodule_path" || exit 1
  git fetch origin

  if git branch -r | grep -q "origin/main"; then
    git checkout main
    echo "Switched $submodule_path to main"
  elif git branch -r | grep -q "origin/master"; then
    git checkout master
    echo "Switched $submodule_path to master"
  else
    echo "Neither main nor master branch exists in $submodule_path. Skipping."
  fi

  cd - > /dev/null || exit 1
}

export -f switch_branch

# 使用 GNU parallel 并行处理
echo "$submodule_paths" | parallel -j "$(nproc)" switch_branch
