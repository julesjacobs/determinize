#!/usr/bin/env bash
# Stop / Notification: audible or desktop notification (AGENTS.md asks for macOS `say`;
# this degrades gracefully on Linux and in headless sessions).
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"
read_hook_input
case "$(jfield hook_event_name)" in
  Stop) notify_user "Claude finished" ;;
  Notification)
    case "$(jfield notification_type)" in
      permission_prompt) notify_user "Claude needs permission" ;;
      idle_prompt) notify_user "Claude is waiting for input" ;;
    esac
    ;;
esac
exit 0
