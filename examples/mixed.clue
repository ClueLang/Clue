local bool_result = condition ? "true" : "false"
local result = 0
local output = "[Output] ";

match user_input {
  1 => {
    result = 1
  }
  2 || 3 => {
    result = 10
  }
}

output ..= bool_result .. " " .. result

print(output)