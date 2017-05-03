{-# LANGUAGE LambdaCase #-}
module Main where

import Devops.Cli (Method(..), appMethod, methodArg, App(..), appMain)
import Devops.Optimize (optimizeDebianPackages)

import Recipes

main :: IO ()
main = do
    let app = App parseRecipes unparseRecipes recipes [optimizeDebianPackages]
    appMain app

unparseRecipes :: Recipes -> Method -> [String]
unparseRecipes recipe m = case recipe of
    PrepareSSH      -> [ "_init_", methodArg m ]
    DockerChroot    -> [ "_chroot_", methodArg m ]
    (DockerBuild x) -> [ "_docker_", show x, methodArg m ]
    Root            -> [ methodArg m ]

parseRecipes :: [String] -> (Recipes, Method)
parseRecipes = \case
    ("_init_":arg:[])     -> (PrepareSSH, appMethod arg)
    ("_chroot_":arg:[])   -> (DockerChroot, appMethod arg)
    ("_docker_":x:arg:[]) -> (DockerBuild $ read x, appMethod arg)
    (arg:[])              -> (Root, appMethod arg)
    args                  -> error $ "unparsed args: " ++ show args
