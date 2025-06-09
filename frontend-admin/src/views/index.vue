<script setup lang="ts">
import useUserStore from '@/store/modules/user'

const Title = import.meta.env.VITE_APP_TITLE
const userStore = useUserStore()
</script>

<template>
  <div>
    <PageHeader>
      <template #title>
        <div class="flex items-center gap-4">
          欢迎使用 {{ Title }}
        </div>
      </template>
      <template #content />
    </PageHeader>
    <PageMain>
      {{ userStore.account }} 欢迎👏
    </PageMain>
  </div>
</template>
