<script lang="ts" setup>
defineOptions({
  name: 'PageMainFull',
})
</script>

<template>
  <div class="absolute-container">
    <PageMain>
      <!-- 布局 -->
      <slot />
    </PageMain>
  </div>
</template>

<style lang="scss" scoped>
// 样式
.absolute-container {
  position: absolute;
  display: flex;
  flex-direction: column;
  width: 100%;
  height: 100%;

  .page-header {
    margin-bottom: 0;
  }

  // 让 page-main 的高度自适应
  .page-main {
    flex: 1;
    overflow: auto;

    :deep(.main-container) {
      display: flex;
      flex: 1;
      flex-direction: column;
      overflow: auto;
    }
  }
}
</style>
