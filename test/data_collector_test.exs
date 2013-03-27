Code.require_file "../test_helper.exs", __FILE__

defmodule DataCollectorTest do
  use ExUnit.Case, async: true

  alias DataCollector.State, as: State

  test "when I have no counts for a bucket, it will create it" do
    state = State.new
    new_state = DataCollector.collect {'visit', '1', 'c'}, state
    assert new_state.counts[:visit] == 1
  end

  test "when I have 3 counts in visit bucket and count 1, I have 4 counts" do
    state = State[counts: [visit: 3]]
    new_state = DataCollector.collect {'visit', '1', 'c'}, state
    assert new_state.counts[:visit] == 4
  end

  test "when I have a set of {1,2} in set bucket and add 3 to it, I have {1,2,3}" do
    state = State[sets: [set: [1,2]]]
    new_state = DataCollector.collect {'set', '3', 's'}, state
    assert new_state.sets[:set] == [1,2,3]
  end

  test "when I have a set of {1,2} in set bucket and add 2 to it, I have {1,2}" do
    state = State[sets: [set: [1,2]]]
    new_state = DataCollector.collect {'set', '2', 's'}, state
    assert new_state.sets[:set] == [1,2]
  end

  test "when I have a gauge of 2 in speed bucket and I set it to 4, the gauge will be 4" do
    state = State[gauges: [speed: 2]]
    new_state = DataCollector.collect {'speed', '4', 'g'}, state
    assert new_state.gauges[:speed] == 4
  end

  test "when I have a gauge of 2 in speed bucket and I set it to +4, the gauge will be 6" do
    state = State[gauges: [speed: 2]]
    new_state = DataCollector.collect {'speed', '+4', 'g'}, state
    assert new_state.gauges[:speed] == 6
  end

  test "when I have a gauge of 10 in speed bucket and I set it to -4, the gauge will be 6" do
    state = State[gauges: [speed: 10]]
    new_state = DataCollector.collect {'speed', '-4', 'g'}, state
    assert new_state.gauges[:speed] == 6
  end

  test "when I have a timer of 1 in load bucket and I add another time of 3, the data will be [1,3]" do
    state = State[timers: [load: [data: [1]]]]
    new_state = DataCollector.collect {'load', '3', 'ms'}, state
    assert new_state.timers[:load][:data] == [1,3]
  end

  test "when I have a timer of 1 in load bucket and I add another time of 3, the mean will be 2" do
    state = State[timers: [load: [data: [1]]]]
    new_state = DataCollector.collect {'load', '3', 'ms'}, state
    assert new_state.timers[:load][:mean] == 2
  end

  test "when I have a timer of 1 in load bucket and I add another time of 3, the sum will be 4" do
    state = State[timers: [load: [data: [1]]]]
    new_state = DataCollector.collect {'load', '3', 'ms'}, state
    assert new_state.timers[:load][:sum] == 4
  end

  test "when I have a timer of 1 in load bucket and I add another time of 3, the upper will be 3" do
    state = State[timers: [load: [data: [1]]]]
    new_state = DataCollector.collect {'load', '3', 'ms'}, state
    assert new_state.timers[:load][:upper] == 3
  end

  test "when I have a timer of 1 in load bucket and I add another time of 3, the lower will be 1" do
    state = State[timers: [load: [data: [1]]]]
    new_state = DataCollector.collect {'load', '3', 'ms'}, state
    assert new_state.timers[:load][:lower] == 1
  end

  test "when I have a timer of 1 in load bucket and I add another time of 3, the standard deviation will be 1.0" do
    state = State[timers: [load: [data: [1]]]]
    new_state = DataCollector.collect {'load', '3', 'ms'}, state
    assert new_state.timers[:load][:standard_deviation] == 1.0
  end

end
