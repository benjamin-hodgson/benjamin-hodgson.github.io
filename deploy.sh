if [ -z "$1" ]
then echo "No commit message"; exit 1
fi

git stash

stack build
stack exec benjamin-pizza clean
stack exec benjamin-pizza build

git checkout master

rsync -a --filter='P _site/' --filter='P _cache/' --filter='P .git/' --filter='P .gitignore' --filter='P .stack-work' --delete-excluded _site/ .

git add -A
git commit -m "$1"

read -p "Say yes to confirm push" yn

case $yn in
    [Yy]* ) git push origin master;
esac

git co develop
git stash pop
