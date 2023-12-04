package com.rockthejvm

import cats.effect.IO

object Utils {
  implicit class IOOps[A](io: IO[A]){
    def debug: IO[A] = io.map(x => {println(s"[${Thread.currentThread().getName}] $x"); x})
  }
}
