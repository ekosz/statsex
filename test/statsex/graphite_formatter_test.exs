defmodule GraphineFormatterTest do
  use ExUnit.Case, async: true

  alias StatsEx.State, as: State
  alias StatsEx.GraphiteFormatter, as: F

  test "formats a single counter" do
    assert F.format(State[counts: [dogs: 1]]) =~ %r/stats\.dogs\.count 1/
  end

  test "formats multiable counters" do
    formated = F.format(State[counts: [dogs: 1, cats: 2]]) 
    assert formated =~ %r/stats\.dogs\.count 1/
    assert formated =~ %r/stats\.cats\.count 2/
  end

  test "formats a single gauge" do
    assert F.format(State[gauges: [speed: 40]]) =~ %r/stats\.gauges\.speed 40/
  end

  test "formats a multiple gauge" do
    formatted = F.format(State[gauges: [speed: 40, visitors: 5]]) 
    assert formatted =~ %r/stats\.gauges\.speed 40/
    assert formatted =~ %r/stats\.gauges\.visitors 5/
  end

  test "formats a single set" do
    assert F.format(State[sets: [users: [1, 3]]]) =~ %r/stats\.sets\.users\.count 2/
  end

  test "formats a multiple sets" do
    formatted = F.format(State[sets: [users: [5,6], admins: [1,3,5]]]) 
    assert formatted =~ %r/stats\.sets\.users\.count 2/
    assert formatted =~ %r/stats\.sets\.admins\.count 3/
  end

  test "formats a timer" do
    formatted = F.format(State[timers: [page_load: [data: [1,3], average: 2, lower: 1, upper: 3, sum: 4, standard_deviation: 1.0]]])
    assert formatted =~ %r/stats\.timers\.page_load\.average 2/
    assert formatted =~ %r/stats\.timers\.page_load\.lower 1/
    assert formatted =~ %r/stats\.timers\.page_load\.upper 3/
    assert formatted =~ %r/stats\.timers\.page_load\.sum 4/
    assert formatted =~ %r/stats\.timers\.page_load\.standard_deviation 1.0/
    refute formatted =~ %r/stats\.timers\.page_load\.data/
  end

  test "inserts a timestamp into the formatted lines" do
    assert F.format(State[counts: [dogs: 1]], 100) =~ %r/ 100\n/
  end

  test "inserts a data point on calculation time" do
    assert F.format(State.new) =~ %r/stats.graphiteStats.calculationtime \d+/
  end
end
