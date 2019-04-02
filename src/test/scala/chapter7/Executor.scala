package chapter7

import java.util.concurrent.TimeUnit

abstract class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}

trait Callable[A] {def call: A}

trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}
